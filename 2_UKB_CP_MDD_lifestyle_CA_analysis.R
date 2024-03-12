#install.packages("Gmisc")
library(Gmisc, quietly = TRUE) 
library(glue)
library(grid)
library(nnet)
library(lubridate)
library(mice)
library(nnet)
library(MatchThem)
library(ggplot2)
library(survey)
library(mitools)
library(dplyr)
set.seed(151097)

## Load in UKB data ----
data <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/UKB.csv")
EOP_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/EOP_keep.csv")
alcohol_exclude <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/alcohol_exclude.csv")
british_irish_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/british_irish_keep.csv")

data_na <- na.omit(data)
cor(data_na$followup_depression, data_na$followup_chronic_pain)

## Sample flow chart ----
## Get overlap of participants with EOP data who also drink and are british/irish
EOP_alcohol_keep <- EOP_keep$x[!EOP_keep$x %in% alcohol_exclude$x]
eligible_keep <- EOP_alcohol_keep[EOP_alcohol_keep %in% british_irish_keep$x]

UKB_pop <- boxGrob(glue("UK Biobank Sample",
             "n = {pop}",
             pop = txtInt(nrow(data)),
             .sep = "\n"), 
        y = 0.75, x = 0.5, bjust = c(0.5, 0.5),
        just = "centre")

eligible_pop <- boxGrob(glue("Eligible",
         "n = {pop}",
         pop = txtInt(length(eligible_keep)),
         .sep = "\n"), 
         y = 0.25, x = 0.5, bjust = c(0.5, 0.5),
        just = "centre")


exclude_pop <- boxGrob(glue("Excluded (n = {tot}):",
             " - No EOP: {no_EOP}",
             " - Non-drinker (in EOP sample): {non_drinker}",
             " - Non-British/Irish (in EOP drinker sample): {non_british_irish}",
             tot = txtInt(nrow(data) - length(eligible_keep)),
             no_EOP = txtInt(nrow(data) - nrow(EOP_keep)),
             non_drinker = txtInt(nrow(EOP_keep) - length(EOP_alcohol_keep)),
             non_british_irish = txtInt((length(EOP_alcohol_keep) - length(eligible_keep))),
             .sep = "\n"), 
        y = 0.5, x = 0.75, bjust = c(0.5, 0.5),
        just = "left")

grid.newpage()

UKB_pop
eligible_pop
exclude_pop

connectGrob(UKB_pop, eligible_pop, "N")
connectGrob(UKB_pop, exclude_pop, "L")

## Data Prep ----
### Remove ineligible ----
## Remove ineligible participants from analysis
data_eligible <- data[data$f.eid %in% eligible_keep,]

## Save eligible data
write.csv(data_eligible, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/UKB_eligible.csv", row.names = F)

### Convert comorbidity followup to numeric (required format for MI)
data_eligible$comorbid_CPDep <- recode(data_eligible$comorbid_CPDep,
       `CP-Dep-` = 0,
       `CP+Dep-` = 1,
       `CP-Dep+` = 2,
       `CP+Dep+` = 3)

### Factorize categorical variables ----
categorical_cols <- c("comorbid_CPDep", "PA_low", "bad_sleep", "lonely","smoking", "high_alcohol_consumption", "obese",
                      "unhealthy_diet", "sex", "employment", "general_health", "living_with_partner")

data_eligible <- data_eligible %>%
  mutate_at(categorical_cols, as.factor)

### Get timings ----
## Get average date of questionnaires
average_touchscreen_date <- mean(as.Date(data_eligible$touchscreen_date), na.rm = T)
average_EOP_date <- mean(as.Date(data_eligible$EOP_date))

## Get difference between dates
date_diff <- difftime(as.Date(data_eligible$EOP_date), as.Date(data_eligible$touchscreen_date))
mean(date_diff, na.rm = T)
sd(date_diff, na.rm = T)

## Remove dates
data_eligible <- data_eligible %>%
  select(-c(touchscreen_date, EOP_date))


## Deal with data missingness ----
#data_eligible <- data_eligible[1:5000,] ## Reduce for now

## Get missingness percentage
missingness_table <- as.data.frame(colMeans(is.na(data_eligible))*100)
missingness_table <- round(missingness_table, 2)
names(missingness_table) <- "missingness (%)"

## Impute datasets in full dataset and sex-stratified datasets
## Number of imputations based on highest missingness percentage - 32% (unhealthy diet)
### Full sample ----
data_eligible_full_imputed <- mice(data_eligible, m = 5, maxit = 20, method = "rf")
### Male sample ----
data_eligible_males_imputed <- mice(subset(data_eligible, sex == 0), m = 5, maxit = 20, method = "rf")
### Female sample ----
data_eligible_females_imputed <- mice(subset(data_eligible, sex == 1), m = 5, maxit = 20, method = "rf")

## Balance datasets ----
### Full sample ----
## Iterate through each exposure variable
exposures <- c("PA_low", "lonely", "smoking", "high_alcohol_consumption", "obese", "unhealthy_diet", "bad_sleep")
for (exposure in exposures){
  
  ## Get matching variables
  matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
  
  ## Make formula
  f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
  
  assign(paste0(exposure, "_full_balanced"), 
         matchthem(as.formula(f), 
                   datasets = data_eligible_full_imputed, 
                   approach = "within",
                   method = "nearest",
                   ratio = 1,
                   caliper = 0.2,
                   distance = "glm")
         )
  ## Love plot
  assign(paste0(exposure, "_full_love_plot"),
         cobalt::love.plot(get(paste0(exposure, "_full_balanced")))
  )
  ## Balance table
  assign(paste0(exposure, "_full_bal_table"),
         as.data.frame(cobalt::bal.tab(get(paste0(exposure, "_full_balanced")),
                                un = TRUE)[[which(grepl("^Balance", names(cobalt::bal.tab(get(paste0(exposure, "_full_balanced")), un = TRUE))))]])
  )
  ## Observation table
  assign(paste0(exposure, "_full_observation_table"),
         as.data.frame(cobalt::bal.tab(get(paste0(exposure, "_full_balanced")))[["Observations"]])
  )
}

### Male and female samples ----
for (sex in c("male", "female")){
  for (exposure in exposures){
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("sex", "f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    
    ## Make formula
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
    
    assign(paste0(exposure,"_", sex, "_balanced"), 
           matchthem(as.formula(f), 
                     datasets = get(paste0("data_eligible_", sex, "s_imputed")), 
                     approach = "within",
                     method = "nearest",
                     ratio = 1,
                     caliper = 0.2,
                     distance = "glm")
    )
    
    ## Love plot
    assign(paste0(exposure,"_", sex, "_love_plot"),
           cobalt::love.plot(get(paste0(exposure,"_", sex, "_balanced")))
    )
    
    ## Balance table
    assign(paste0(exposure,"_", sex, "_bal_table"),
           as.data.frame(cobalt::bal.tab(get(paste0(exposure,"_", sex, "_balanced")),
                                         un = TRUE)[[which(grepl("^Balance", names(cobalt::bal.tab(get(paste0(exposure,"_", sex, "_balanced")), un = TRUE))))]])
    )
    
    ## Observation table
    assign(paste0(exposure,"_", sex, "_observation_table"),
           as.data.frame(cobalt::bal.tab(get(paste0(exposure,"_", sex, "_balanced")))[["Observations"]])
    )
  }
}

## Outcome model ----
### Linear regression ----
#### Full sample ----
outcomes <- c("followup_chronic_pain", "followup_depression")

## Iterate through outcomes and exposures
for (exposure in exposures){
  for (outcome in outcomes){
    
    ## Extract balanced 
    extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_full_balanced")), "all", all = FALSE)
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    
    ## Formula controlling for matching variables as covariates and their interactions with the exposure
    model_formula = as.formula(paste0(outcome,
                                      "~",exposure,
                                      "*(",paste0(matching_variables, collapse="+"), ")"))
    
    ## Fit linear regression model
    fits <- lapply(extracted_balanced_data, function(d) {
      lm(model_formula, data = d)
    })
    
    model_fit = lapply(fits, function(fit){
      marginaleffects::avg_comparisons(fit, newdata = subset(fit$model,
                                                             get(exposure) == 1),
                                       variables = exposure)
    })
    
    ## Pool results over imputations
    model_fit_pooled = mice::pool(model_fit)
    ## Get CIs
    extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
    ## Format results
    results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
    ## Save results
    results_dataframe$exposure <- exposure
    results_dataframe$outcome <- outcome
    assign(paste0(exposure,"_",outcome, "_full_CP_Dep_results"), results_dataframe)
  }
}

## Compile results into single df
CP_Dep_results <- mget(ls(pattern="full_CP_Dep_results")) %>%
  bind_rows()

#### Male and female samples ----
for (sex in c("male", "female")){
  for (exposure in exposures){
    for (outcome in outcomes){
      
      ## Extract balanced 
      extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_", sex, "_balanced")), "all", all = FALSE)
      
      ## Get matching variables
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("sex", "f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
      
      ## Formula controlling for matching variables as covariates and their interactions with the exposure
      model_formula = as.formula(paste0(outcome,
                                        "~",exposure,
                                        "*(",paste0(matching_variables, collapse="+"), ")"))
      
      ## Fit linear regression model
      fits <- lapply(extracted_balanced_data, function(d) {
        lm(model_formula, data = d)
      })
      
      model_fit = lapply(fits, function(fit){
        marginaleffects::avg_comparisons(fit, newdata = subset(fit$model,
                                                               get(exposure) == 1),
                                         variables = exposure)
      })
      
      ## Pool results over imputations
      model_fit_pooled = mice::pool(model_fit)
      ## Get CIs
      extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
      ## Format results
      results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
      colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
      ## Save results
      results_dataframe$exposure <- exposure
      results_dataframe$outcome <- outcome
      assign(paste0(exposure,"_",outcome, "_", sex, "_CP_Dep_results"), results_dataframe)
    }
  }
  
  ## Compile results into single df
  assign(paste0("CP_Dep_", sex, "_results"), mget(ls(pattern=paste0(sex, "_CP_Dep_results"))) %>%
    bind_rows()
  )
}

### Multinominal logistic regression ----
#### Full sample ----
## Iterate through exposures
for (exposure in exposures){
    
  ## Extract balanced 
  extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_full_balanced")), "all", all = FALSE)
  
  ## Set reference as CP-Dep-
  for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
    extracted_balanced_data[[imputation]]$comorbid_CPDep <- relevel(as.factor(extracted_balanced_data[[imputation]]$comorbid_CPDep), ref = "0")
  }
  
  ## Get matching variables
  matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
  
  ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
  model_formula = as.formula(paste0("comorbid_CPDep ~",
                                    exposure,
                                    "*(",paste0(matching_variables, collapse="+"), ")"))
  
  ## Use for loop rather than lapply() as bug with fits[[i]]$call preventing marginaleffects from running 
  model_fits <- list()
  for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation

    ## Fit multinomial logistic regression model
    fit <- multinom(model_formula, data = extracted_balanced_data[[imputation]])
    
    ## Get marginal effects
    model_fit = marginaleffects::avg_comparisons(
      fit,newdata = subset(extracted_balanced_data[[imputation]],
                           get(exposure) == 1),
      variables = exposure,
      hypothesis = c("b2 - b1 = 0", ## CP-Dep- VS CP+Dep-
                     "b3 - b1 = 0", ## CP-Dep- VS CP-Dep+
                     "b4 - b1 = 0"))  ## CP-Dep- VS CP+Dep+
    
    model_fits[[imputation]] <- model_fit
    
  }
  
  ## Pool results over imputations
  model_fit_pooled = pool(as.mira(model_fits))
  ## Get CIs
  extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
  ## Format results
  results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
  colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
  ## Save results
  results_dataframe$exposure <- exposure
  assign(paste0(exposure,"_full_CPDep_results"), results_dataframe)
}

## Compile results into single df
CPDep_results <- mget(ls(pattern="full_CPDep_results")) %>%
  bind_rows()

## Recode to indicate comorbidity group
CPDep_results$Term <- recode(CPDep_results$Term,
       "b2-b1=0" = "CP+Dep-",
       "b3-b1=0" = "CP-Dep+",
       "b4-b1=0" = "CP+Dep+")

#### Male and females samples----
for (sex in c("male", "female")){
  for (exposure in exposures){
    
    ## Extract balanced 
    extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_", sex, "_balanced")), "all", all = FALSE)
    
    ## Set reference as CP-Dep-
    for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
      extracted_balanced_data[[imputation]]$comorbid_CPDep <- relevel(as.factor(extracted_balanced_data[[imputation]]$comorbid_CPDep), ref = "0")
    }
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("sex", "f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    
    ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
    model_formula = as.formula(paste0("comorbid_CPDep ~",
                                      exposure,
                                      "*(",paste0(matching_variables, collapse="+"), ")"))
    
    ## Use for loop rather than lapply() as bug with fits[[i]]$call preventing marginaleffects from running 
    model_fits <- list()
    for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
      
      ## Fit multinomial logistic regression model
      fit <- multinom(model_formula, data = extracted_balanced_data[[imputation]])
      
      ## Get marginal effects
      model_fit = marginaleffects::avg_comparisons(
        fit,newdata = subset(extracted_balanced_data[[imputation]],
                             get(exposure) == 1),
        variables = exposure,
        hypothesis = c("b2 - b1 = 0", ## CP-Dep- VS CP+Dep-
                       "b3 - b1 = 0", ## CP-Dep- VS CP-Dep+
                       "b4 - b1 = 0"))  ## CP-Dep- VS CP+Dep+
      
      model_fits[[imputation]] <- model_fit
      
    }
    
    ## Pool results over imputations
    model_fit_pooled = pool(as.mira(model_fits))
    ## Get CIs
    extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
    ## Format results
    results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
    ## Recode to indicate comorbidity group
    results_dataframe$Term <- recode(results_dataframe$Term,
                                 "b2-b1=0" = "CP+Dep-",
                                 "b3-b1=0" = "CP-Dep+",
                                 "b4-b1=0" = "CP+Dep+")
    ## Save results
    results_dataframe$exposure <- exposure
    assign(paste0(exposure, "_", sex, "_CPDep_results"), results_dataframe)

  }
  ## Compile results into single df
  assign(paste0("CPDep_",sex, "_results"), mget(ls(pattern=paste0(sex, "_CPDep_results"))) %>%
    bind_rows()
  )
}


## Adjust for multiple comparisons ----
## Get n total comparisons
n_comparisons <- (nrow(CPDep_results) + nrow(CP_Dep_results) * 3)

CP_Dep_results$p_adjust <- p.adjust(CP_Dep_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_results$p_adjust <- p.adjust(CPDep_results$`P-value`, method = "bonferroni", n = n_comparisons)

CP_Dep_male_results$p_adjust <- p.adjust(CP_Dep_male_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_male_results$p_adjust <- p.adjust(CPDep_male_results$`P-value`, method = "bonferroni", n = n_comparisons)

CP_Dep_female_results$p_adjust <- p.adjust(CP_Dep_female_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_female_results$p_adjust <- p.adjust(CPDep_female_results$`P-value`, method = "bonferroni", n = n_comparisons)



## Save results ----
write.csv(CP_Dep_full_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CP_Dep_full_results.csv", row.names = F)
write.csv(CPDep_full_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CPDep_full_results.csv", row.names = F)

write.csv(CP_Dep_male_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CP_Dep_male_results.csv", row.names = F)
write.csv(CPDep_male_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CPDep_male_results.csv", row.names = F)

write.csv(CP_Dep_female_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CP_Dep_female_results.csv", row.names = F)
write.csv(CPDep_female_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CPDep_female_results.csv", row.names = F)

write.csv(missingness_table, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data_missingness.csv", row.names = T)



