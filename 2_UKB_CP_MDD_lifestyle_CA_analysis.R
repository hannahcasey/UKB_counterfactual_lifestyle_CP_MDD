library(nnet)
library(lubridate)
library(mice)
library(nnet)
library(WeightIt)
library(ggplot2)
library(survey)
library(mitools)
library(dplyr)
set.seed(151097)

## Load in UKB data ----
data <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB.csv")
alcohol_exclude <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/alcohol_exclude.csv")

## Data Prep ----
### Remove ineligible ----
## Remove ineligible participants from analysis
data_eligible <- data[!is.na(data$initial_touchscreen_date) & !is.na(data$second_touchscreen_date) & !is.na(data$EOP_date),]
data_eligible <- data_eligible[!data_eligible$f.eid %in% alcohol_exclude$x,]

## Save eligible data
write.csv(data_eligible, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB_eligible.csv", row.names = F)

### Convert comorbidity followup to numeric (required format for MI)
data_eligible$comorbid_CPDep <- recode(data_eligible$comorbid_CPDep,
       `CP-Dep-` = 0,
       `CP+Dep-` = 1,
       `CP-Dep+` = 2,
       `CP+Dep+` = 3)

### Factorize categorical variables ----
categorical_cols <- c("depression_baseline", "chronic_pain_baseline", "followup_chronic_pain", "followup_depression",
                      "comorbid_CPDep", "PA_low", "insufficient_sleep", "lonely","smoking", 
                      "high_alcohol_consumption", "obese", "unhealthy_diet","PA_low_baseline", 
                      "insufficient_sleep_baseline", "lonely_baseline","smoking_baseline", "high_alcohol_consumption_baseline", 
                      "obese_baseline", "unhealthy_diet_baseline","sex", "employment", "general_health","living_with_partner")

data_eligible <- data_eligible %>%
  mutate_at(categorical_cols, as.factor)

### Get timings ----
## Get average date of questionnaires
average_initial_touchscreen_date <- mean(as.Date(data_eligible$initial_touchscreen_date), na.rm = T)
average_second_touchscreen_date <- mean(as.Date(data_eligible$second_touchscreen_date), na.rm = T)
average_EOP_date <- mean(as.Date(data_eligible$EOP_date))

## Get difference between dates
date_baseline_exposure_diff <- difftime(as.Date(data_eligible$second_touchscreen_date), as.Date(data_eligible$initial_touchscreen_date))
mean(date_baseline_exposure_diff, na.rm = T)
sd(date_baseline_exposure_diff, na.rm = T)

date_exposure_outcome_diff <- difftime(as.Date(data_eligible$EOP_date), as.Date(data_eligible$second_touchscreen_date))
mean(date_exposure_outcome_diff, na.rm = T)
sd(date_exposure_outcome_diff, na.rm = T)

data_eligible <- data_eligible %>%
  select(-c(initial_touchscreen_date, second_touchscreen_date, EOP_date))

## Deal with data missingness ----
## Get missingness percentage
missingness_table <- as.data.frame(colMeans(is.na(data_eligible))*100)
missingness_table <- round(missingness_table, 2)
names(missingness_table) <- "missingness (%)"

## Impute datasets in full dataset and sex-stratified datasets
### Full sample ----
data_eligible_full_imputed <- mice(data_eligible, m = 10, maxit = 20, method = "rf")
### Male sample ----
data_eligible_males_imputed <- mice(subset(data_eligible, sex == 0), m = 10, maxit = 20, method = "rf")
### Female sample ----
data_eligible_females_imputed <- mice(subset(data_eligible, sex == 1), m = 10, maxit = 20, method = "rf")

## Balance datasets ----
### Full sample ----
## Iterate through each exposure variable
exposures <- c("PA_low", "lonely", "smoking", "high_alcohol_consumption", "obese", "unhealthy_diet", "insufficient_sleep")
for (exposure in exposures){
  
  ## Get IDs of those with missing treatment -  these will be remove prior to matching
  ID_missing <- data_eligible$f.eid[is.na(data_eligible[,exposure])]
  ## Remove those with missing treatment from imputed data
  data_eligible_full_imputed_complete_treatment <- filter(data_eligible_full_imputed, !f.eid %in% ID_missing)
  
  ## Make formula
  ## Get matching variables
  matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposures, paste0(exposure, "_baseline"), "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
  
  ## Calculate weights
  assign(paste0(exposure, "_full_balanced"), 
         weightthem(as.formula(f), 
                    datasets = data_eligible_full_imputed_complete_treatment, 
                    method = "glm",
                    estimand = "ATE")
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
    
    ## Get IDs of those with missing treatment -  these will be remove prior to matching
    ID_missing <- data_eligible$f.eid[is.na(data_eligible[,exposure])]
    ## Remove those with missing treatment from imputed data
    data_eligible_full_imputed_complete_treatment <- filter(get(paste0("data_eligible_", sex, "s_imputed")), !f.eid %in% ID_missing)
    
    ## Make formula
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposures, paste0(exposure, "_baseline"), "sex", "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
    
    ## Calculate weights
    assign(paste0(exposure,"_", sex, "_balanced"), 
           weightthem(as.formula(f), 
                     datasets = data_eligible_full_imputed_complete_treatment, 
                     method = "glm",
                     estimand = "ATE")
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
### Logistic regression ----
#### Full sample ----
outcomes <- c("followup_chronic_pain", "followup_depression")

## Iterate through outcomes and exposures
for (exposure in exposures){
  for (outcome in outcomes){
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposures, paste0(exposure, "_baseline"), "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]

    model_formula = as.formula(paste0(outcome,
                                      "~", exposure,
                                      "+(",paste0(matching_variables, collapse="+"), ")"))
    
    ## Fit linear regression model
    wimp <- get(paste0(exposure, "_full_balanced"))
    fits <- lapply(seq_along(wimp$models), function(i) {
      data <- complete(wimp, i)
      W <- wimp$models[[i]]
      
      glm_weightit(model_formula,
                   data = data,
                   weightit = W,
                   family = binomial)
    })
    
    ## Get marginal effetcs
    comp.imp <- lapply(fits, function(fit) {
      marginaleffects::avg_comparisons(fit, 
                                       variables = exposure,
                                       comparison = "lnoravg",
                                       newdata = fit$data)
    })
    
    ## Pool results over imputations
    pooled.comp <- mice::pool(comp.imp, dfcom = Inf)
    
    ## Get ORs and CIs
    extracted_outcome_results = summary(pooled.comp, conf.int = TRUE, exponentiate = TRUE)
    ## Format results
    results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
    
    ## Save results
    results_dataframe$exposure <- exposure
    results_dataframe$outcome <- outcome
    assign(paste0(exposure,"_",outcome, "_full_CP_Dep_full_results"), results_dataframe)
  }
}

 ## Compile results into single df
CP_Dep_full_results <- mget(ls(pattern="_full_CP_Dep_full_results")) %>%
  bind_rows()

#### Male and female samples ----
for (sex in c("male", "female")){
  for (exposure in exposures){
    for (outcome in outcomes){

      ## Get matching variables
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposures, paste0(exposure, "_baseline"), "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
      
      ## Formula controlling for matching variables as covariates and their interactions with the exposure
      model_formula = as.formula(paste0(outcome,
                                        "~",exposure,
                                        "+(",paste0(matching_variables, collapse="+"), ")"))
      
      ## Fit linear regression model
      wimp <- get(paste0(exposure, "_", sex, "_balanced"))
      fits <- lapply(seq_along(wimp$models), function(i) {
        data <- complete(wimp, i)
        W <- wimp$models[[i]]
        
        glm_weightit(model_formula,
                     data = data,
                     weightit = W,
                     family = binomial)
      })
      
      ## Get marginal effetcs
      comp.imp <- lapply(fits, function(fit) {
        marginaleffects::avg_comparisons(fit,
                                         variables = exposure,
                                         comparison = "lnoravg",
                                         newdata = fit$data)
      })
      
      ## Pool results over imputations
      pooled.comp <- mice::pool(comp.imp, dfcom = Inf)
      
      ## Get ORs and CIs
      extracted_outcome_results = summary(pooled.comp, conf.int = TRUE, exponentiate = TRUE) 
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
    
  ## Get matching variables
  matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposures, paste0(exposure, "_baseline"), "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
  
  ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
  model_formula = as.formula(paste0("comorbid_CPDep ~",
                                    exposure,
                                    "+(",paste0(matching_variables, collapse="+"), ")"))
  
  ## Fit linear regression model
  wimp <- get(paste0(exposure, "_full_balanced"))
  fits <- lapply(seq_along(wimp$models), function(i) {
    data <- complete(wimp, i)
    W <- wimp$models[[i]]
    
    multinom_weightit(model_formula,
                 data = data,
                 weightit = W)
  })
  
  ## Get marginal effects
  comp.imp <- lapply(fits, function(fit) {
    marginaleffects::avg_comparisons(fit, 
                                     variables = exposure,
                                     comparison = "lnoravg",
                                     newdata = fit$data)
  })
  
  
  ## Otherwise comorbidity groups are combined when results pooled
  comp.imp <- lapply(comp.imp, FUN = function(x) {
    x[,"term"] <- x[,"group"]
    x
    })
  
  ## Pool results over imputations
  pooled.comp <- mice::pool(comp.imp, dfcom = Inf, )
  
  ## Get ORs and CIs
  extracted_outcome_results = summary(pooled.comp, conf.int = TRUE, exponentiate = TRUE) 
  ## Format results
  results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
  colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
  ## Save results
  results_dataframe$exposure <- exposure
  assign(paste0(exposure,"_full_CPDep_results"), results_dataframe)
}

## Compile results into single df
CPDep_full_results <- mget(ls(pattern="full_CPDep_results")) %>%
  bind_rows()

## Recode to indicate comorbidity group
CPDep_full_results$Term <- recode(CPDep_full_results$Term,
       `0` = "CP-Dep-",
       `1` = "CP+Dep-",
       `2` = "CP-Dep+",
       `3` = "CP+Dep+")

#### Male and females samples----
for (sex in c("male", "female")){
  for (exposure in exposures){
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposures, paste0(exposure, "_baseline"), "followup_chronic_pain", "followup_depression", "comorbid_CPDep")]
    
    ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
    model_formula = as.formula(paste0("comorbid_CPDep ~",
                                      exposure,
                                      "+(",paste0(matching_variables, collapse="+"), ")"))
    
    ## Fit linear regression model
    wimp <- get(paste0(exposure, "_", sex, "_balanced"))
    fits <- lapply(seq_along(wimp$models), function(i) {
      data <- complete(wimp, i)
      W <- wimp$models[[i]]
      
      multinom_weightit(model_formula,
                        data = data,
                        weightit = W)
    })
    
    ## Get marginal effetcs
    comp.imp <- lapply(fits, function(fit) {
      marginaleffects::avg_comparisons(fit,
                                       variables = exposure,
                                       comparison = "lnoravg",
                                       newdata = fit$data)
    })
    
    
    ## Otherwise comorbidity groups are combined when results pooled
    comp.imp <- lapply(comp.imp, FUN = function(x) {
      x[,"term"] <- x[,"group"]
      x
    })
    
    ## Pool results over imputations
    pooled.comp <- mice::pool(comp.imp, dfcom = Inf, )
    
    ## Get ORs and CIs
    extracted_outcome_results = summary(pooled.comp, conf.int = TRUE, exponentiate = TRUE) 
    ## Format results
    results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
    colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
    ## Recode to indicate comorbidity group
    results_dataframe$Term <- recode(results_dataframe$Term,
                                     `0` = "CP-Dep-",
                                     `1` = "CP+Dep-",
                                     `2` = "CP-Dep+",
                                     `3` = "CP+Dep+")
    
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
n_comparisons <- (nrow(CPDep_full_results) + nrow(CP_Dep_full_results))

CP_Dep_full_results$p_adjust <- p.adjust(CP_Dep_full_results$`P.value`, method = "bonferroni", n = n_comparisons)
CPDep_full_results$p_adjust <- p.adjust(CPDep_full_results$`P.value`, method = "bonferroni", n = n_comparisons)

CP_Dep_male_results$p_adjust <- p.adjust(CP_Dep_male_results$`P.value`, method = "bonferroni", n = n_comparisons)
CPDep_male_results$p_adjust <- p.adjust(CPDep_male_results$`P.value`, method = "bonferroni", n = n_comparisons)

CP_Dep_female_results$p_adjust <- p.adjust(CP_Dep_female_results$`P.value`, method = "bonferroni", n = n_comparisons)
CPDep_female_results$p_adjust <- p.adjust(CPDep_female_results$`P.value`, method = "bonferroni", n = n_comparisons)


## Save results ----
write.csv(CP_Dep_full_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CP_Dep_full_results.csv", row.names = F)
write.csv(CPDep_full_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CPDep_full_results.csv", row.names = F)

write.csv(CP_Dep_male_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CP_Dep_male_results.csv", row.names = F)
write.csv(CPDep_male_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CPDep_male_results.csv", row.names = F)

write.csv(CP_Dep_female_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CP_Dep_female_results.csv", row.names = F)
write.csv(CPDep_female_results, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/CPDep_female_results.csv", row.names = F)

write.csv(missingness_table, "~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/data_missingness.csv", row.names = T)

for (plot in ls(pattern="love_plot")){
  
  jpeg(file = paste0("~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/balancing/", plot, ".jpg"), width = 700, height = 700)
  print(get(plot))
  dev.off()
}

for (table in ls(pattern="bal_table|observation_table")){
  write.csv(get(table), paste0("~/Desktop/PhD/projects/UKB_CP_MDD_lifestyle_counterfactual/output/balancing/", table, ".csv"))
}

