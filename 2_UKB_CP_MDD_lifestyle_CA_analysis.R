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
data <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB.csv")
EOP_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/EOP_keep.csv")
alcohol_exclude <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/alcohol_exclude.csv")
british_irish_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/british_irish_keep.csv")


## Data Prep ----
### Remove ineligible ----
## Remove ineligible participants from analysis
data_british_irish_EOP <- data[data$f.eid %in% intersect(british_irish_keep$x, EOP_keep$x),]
data_eligible <- data_british_irish_EOP[!data_british_irish_EOP$f.eid %in% alcohol_exclude$x,]

## Save eligible data
write.csv(data_eligible, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB_eligible.csv", row.names = F)

### Convert comorbidity followup to numeric (required format for MI)
data_eligible$comorbid_CPDep <- recode(data_eligible$comorbid_CPDep,
       `CP-Dep-` = 0,
       `CP+Dep-` = 1,
       `CP-Dep+` = 2,
       `CP+Dep+` = 3)

### Factorize categorical variables ----
categorical_cols <- c("baseline_depression", "baseline_chronic_pain", "followup_chronic_pain", "followup_depression",
                      "comorbid_CPDep", "PA_low", "too_much_sleep", "too_little_sleep", "lonely","smoking", 
                      "high_alcohol_consumption", "obese", "unhealthy_diet", "sex", "employment", "general_health",
                      "living_with_partner")

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
#data_eligible <- data_eligible[1:3000,] ## Reduce for now

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
exposures <- c("PA_low", "lonely", "smoking", "high_alcohol_consumption", "obese", "unhealthy_diet", "too_much_sleep", "too_little_sleep")
for (exposure in exposures){
  
  ## Get IDs of those with missing treatment -  these will be remove prior to matching
  ID_missing <- data_eligible$f.eid[is.na(data_eligible[,exposure])]
  ## Remove those with missing treatment from imputed data
  data_eligible_full_imputed_complete_treatment <- filter(data_eligible_full_imputed, !f.eid %in% ID_missing)
  
  ## Make formula
  if (exposure == "too_much_sleep"){
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_little_sleep")]
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
  } else if(exposure == "too_little_sleep"){
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_much_sleep")]
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
  } else{
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
    f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
  }
  
  ## Match treatment groups
  assign(paste0(exposure, "_full_balanced"), 
         matchthem(as.formula(f), 
                   datasets = data_eligible_full_imputed_complete_treatment, 
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
  
  ## Sensitivity analysis - don't include obesity as matching var with low PA exposure
  if(exposure == "PA_low"){
    
    ## Make formula
    f_sensitivity = paste0(exposure," ~ ",paste0(matching_variables[matching_variables !="obese"], collapse=" + "))
    
    ## Match treatment groups
    sensitivity_PA_low_full_balanced <- matchthem(as.formula(f_sensitivity), 
                     datasets = data_eligible_full_imputed_complete_treatment, 
                     approach = "within",
                     method = "nearest",
                     ratio = 1,
                     caliper = 0.2,
                     distance = "glm")
    
    ## Love plot
    sensitivity_PA_low_full_love_plot <- cobalt::love.plot(sensitivity_PA_low_full_balanced)
    ## Balance table
    sensitivity_PA_low_full_bal_table <- as.data.frame(cobalt::bal.tab(sensitivity_PA_low_full_balanced,
                                         un = TRUE)[[which(grepl("^Balance", names(cobalt::bal.tab(sensitivity_PA_low_full_balanced, un = TRUE))))]])
    
    ## Observation table
    sensitivity_PA_low_full_observation_table <- as.data.frame(cobalt::bal.tab(sensitivity_PA_low_full_balanced)[["Observations"]])
  }
}

### Male and female samples ----
for (sex in c("male", "female")){
  for (exposure in exposures){
    
    ## Get IDs of those with missing treatment -  these will be remove prior to matching
    ID_missing <- data_eligible$f.eid[is.na(data_eligible[,exposure])]
    ## Remove those with missing treatment from imputed data
    data_eligible_full_imputed_complete_treatment <- filter(get(paste0("data_eligible_", sex, "s_imputed")), !f.eid %in% ID_missing)
    
    ## Make formula
    if (exposure == "too_much_sleep"){
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_little_sleep")]
      f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
    } else if(exposure == "too_much_sleep"){
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_much_sleep")]
      f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
    } else{
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
      f = paste0(exposure," ~ ",paste0(matching_variables, collapse=" + "))
    }
    
    assign(paste0(exposure,"_", sex, "_balanced"), 
           matchthem(as.formula(f), 
                     datasets = data_eligible_full_imputed_complete_treatment, 
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
    
    ## Sensitivity analysis - don't include obesity as matching var with low PA exposure
    if(exposure == "PA_low"){
      
      ## Make formula
      f_sensitivity = paste0(exposure," ~ ",paste0(matching_variables[matching_variables !="obese"], collapse=" + "))
      
      ## Match treatment groups
      assign(paste0("sensitivity_PA_low_", sex, "_balanced"),
             matchthem(as.formula(f_sensitivity), 
                       datasets = data_eligible_full_imputed_complete_treatment, 
                       approach = "within",
                       method = "nearest",
                       ratio = 1,
                       caliper = 0.2,
                       distance = "glm")
      )
      
      ## Love plot
      assign(paste0("sensitivity_PA_low_", sex, "_love_plot"),
             cobalt::love.plot(get(paste0("sensitivity_PA_low_", sex, "_balanced"))))
      ## Balance table
      assign(paste0("sensitivity_PA_low_", sex, "_bal_table"), as.data.frame(cobalt::bal.tab(get(paste0("sensitivity_PA_low_", sex, "_balanced")),
                                                                         un = TRUE)[[which(grepl("^Balance", names(cobalt::bal.tab(get(paste0("sensitivity_PA_low_", sex, "_balanced")), un = TRUE))))]]))
      
      ## Observation table
      assign(paste0("sensitivity_PA_low_", sex, "_observation_table"),
             as.data.frame(cobalt::bal.tab(get(paste0("sensitivity_PA_low_", sex, "_balanced")))[["Observations"]]))
    }
  }
}

## Outcome model ----
### Logistic regression ----
#### Full sample ----
outcomes <- c("followup_chronic_pain", "followup_depression")

## Iterate through outcomes and exposures
for (exposure in c(exposures, "sensitivity_PA_low")){
  for (outcome in outcomes){
    
    ## Extract balanced 
    extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_full_balanced")), "all", all = FALSE)
    
    if(exposure != "sensitivity_PA_low"){
      
      ## Get matching variables
      if (exposure == "too_much_sleep"){
        matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_little_sleep")]
      } else if(exposure == "too_little_sleep"){
        matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_much_sleep")]
      } else{
        matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
      }

      model_formula = as.formula(paste0(outcome,
                                        "~", exposure,
                                        "*(",paste0(matching_variables, collapse="+"), ")"))
      
      ## Fit linear regression model
      fits <- lapply(extracted_balanced_data, function(d) {
        glm(model_formula, data = d, family = binomial)
      })
      
      ## Get marginal effetcs
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
      
      ## Sensitivity analysis - low PA not matched on obesity
    } else{
      
      ## Get matching variables"
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("obese", "f.eid", "PA_low", "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
      
      ## Formula controlling for matching variables as covariates and their interactions with the exposure
      model_formula = as.formula(paste0(outcome,
                                        "~ PA_low",
                                        "*(",paste0(matching_variables, collapse="+"), ")"))
      
      ## Fit linear regression model
      fits <- lapply(extracted_balanced_data, function(d) {
        lm(model_formula, data = d)
      })
      
      ## Get marginal effetcs
      model_fit = lapply(fits, function(fit){
        marginaleffects::avg_comparisons(fit, newdata = subset(fit$model,
                                                               PA_low == 1),
                                         variables = "PA_low")
      })
      
      ## Pool results over imputations
      model_fit_pooled = mice::pool(model_fit)
      ## Get CIs
      extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
      ## Format results
      results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
      results_dataframe$term <- "sensitivity_PA_low"
      colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
    }
    
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
  for (exposure in c(exposures, "sensitivity_PA_low")){
    for (outcome in outcomes){
      
      ## Extract balanced 
      extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_", sex, "_balanced")), "all", all = FALSE)
      
      if(exposure != "sensitivity_PA_low"){
        
        ## Get matching variables
        if (exposure == "too_much_sleep"){
          matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_little_sleep")]
        } else if(exposure == "too_little_sleep"){
          matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep", "too_much_sleep")]
        } else{
          matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", "sex", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
        }
        
        ## Formula controlling for matching variables as covariates and their interactions with the exposure
        model_formula = as.formula(paste0(outcome,
                                          "~",exposure,
                                          "*(",paste0(matching_variables, collapse="+"), ")"))
        
        ## Fit linear regression model
        fits <- lapply(extracted_balanced_data, function(d) {
          lm(model_formula, data = d)
        })
        
        ## Get marginal effects
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
      
        ## Sensitivity analysis - low PA not matched on obesity
      } else{
        
        ## Get matching variables
        matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("obese", "sex", "f.eid", "PA_low", "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
        
        ## Formula controlling for matching variables as covariates and their interactions with the exposure
        model_formula = as.formula(paste0(outcome,
                                          "~ PA_low",
                                          "*(",paste0(matching_variables, collapse="+"), ")"))
        
        ## Fit linear regression model
        fits <- lapply(extracted_balanced_data, function(d) {
          lm(model_formula, data = d)
        })
        
        ## Get marginal effects
        model_fit = lapply(fits, function(fit){
          marginaleffects::avg_comparisons(fit, newdata = subset(fit$model,
                                                                 PA_low == 1),
                                           variables = "PA_low")
        })
        
        ## Pool results over imputations
        model_fit_pooled = mice::pool(model_fit)
        ## Get CIs
        extracted_outcome_results = summary(model_fit_pooled, conf.int = TRUE)
        ## Format results
        results_dataframe <- extracted_outcome_results[,c("term", "estimate", "std.error", "p.value", "2.5 %", "97.5 %")]
        results_dataframe$term <- "sensitivity_PA_low"
        colnames(results_dataframe) <- c("Term","Coefficient Estimate", "Standard Error", "P-value", "Lower_95CI", "Upper_95CI")
      }
      
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
for (exposure in c(exposures, "sensitivity_PA_low")){
    
  ## Extract balanced 
  extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_full_balanced")), "all", all = FALSE)
  
  ## Set reference as CP-Dep-
  for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
    extracted_balanced_data[[imputation]]$comorbid_CPDep <- relevel(as.factor(extracted_balanced_data[[imputation]]$comorbid_CPDep), ref = "0")
  }
  
  if(exposure != "sensitivity_PA_low"){
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
    
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
        variables = exposure)
      
      model_fits[[imputation]] <- model_fit
      
      model_fits[[imputation]]$term <- model_fits[[imputation]]$group ## Otherwise comorbidity groups are combined when results pooled
    }
    
  }## Sensitivity analysis - low PA not matched on obesity
  else{
    
    ## Get matching variables
    matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("obese", "f.eid", "PA_low", "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
    
    ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
    model_formula = as.formula(paste0("comorbid_CPDep ~ PA_low",
                                      "*(",paste0(matching_variables, collapse="+"), ")"))
    
    ## Use for loop rather than lapply() as bug with fits[[i]]$call preventing marginaleffects from running 
    model_fits <- list()
    for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
      
      ## Fit multinomial logistic regression model
      fit <- multinom(model_formula, data = extracted_balanced_data[[imputation]])
      
      ## Get marginal effects
      model_fit = marginaleffects::avg_comparisons(
        fit,newdata = subset(extracted_balanced_data[[imputation]],
                             PA_low == 1),
        variables = "PA_low")
      
      model_fits[[imputation]] <- model_fit
      
      model_fits[[imputation]]$term <- model_fits[[imputation]]$group ## Otherwise comorbidity groups are combined when results pooled
      }
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
  for (exposure in c(exposures, "sensitivity_PA_low")){
    
    ## Extract balanced 
    extracted_balanced_data <-  MatchThem::complete(get(paste0(exposure, "_", sex, "_balanced")), "all", all = FALSE)
    
    ## Set reference as CP-Dep-
    for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
      extracted_balanced_data[[imputation]]$comorbid_CPDep <- relevel(as.factor(extracted_balanced_data[[imputation]]$comorbid_CPDep), ref = "0")
    }
    
    if(exposure != "sensitivity_PA_low"){
      
      ## Get matching variables
      matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("sex", "f.eid", exposure, "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
      
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
          variables = exposure)
        
        model_fits[[imputation]] <- model_fit
        model_fits[[imputation]]$term <- model_fits[[imputation]]$group ## Otherwise comorbidity groups are combined when results pooled
        }## Sensitivity analysis - low PA not matched on obesity  
      }else{
      
        ## Get matching variables
        matching_variables <- names(data_eligible)[!names(data_eligible) %in% c("obese", "f.eid", "PA_low", "followup_chronic_pain", "followup_depression", "comorbid_CPDep", "bad_sleep")]
        
        ## Formula controlling for matching variables as covariates and their interactions with comorbidity groups
        model_formula = as.formula(paste0("comorbid_CPDep ~ PA_low",
                                          "*(",paste0(matching_variables, collapse="+"), ")"))
        
        ## Use for loop rather than lapply() as bug with fits[[i]]$call preventing marginaleffects from running 
        model_fits <- list()
        for (imputation in 1:length(extracted_balanced_data)){ ## Iterate through each imputation
          
          ## Fit multinomial logistic regression model
          fit <- multinom(model_formula, data = extracted_balanced_data[[imputation]])
          
          ## Get marginal effects
          model_fit = marginaleffects::avg_comparisons(
            fit,newdata = subset(extracted_balanced_data[[imputation]],
                                 PA_low == 1),
            variables = "PA_low")
          
          model_fits[[imputation]] <- model_fit
          model_fits[[imputation]]$term <- model_fits[[imputation]]$group ## Otherwise comorbidity groups are combined when results pooled
        }
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
n_comparisons <- (nrow(CPDep_full_results) + nrow(CP_Dep_full_results) * 3)

CP_Dep_full_results$p_adjust <- p.adjust(CP_Dep_full_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_full_results$p_adjust <- p.adjust(CPDep_full_results$`P-value`, method = "bonferroni", n = n_comparisons)

CP_Dep_male_results$p_adjust <- p.adjust(CP_Dep_male_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_male_results$p_adjust <- p.adjust(CPDep_male_results$`P-value`, method = "bonferroni", n = n_comparisons)

CP_Dep_female_results$p_adjust <- p.adjust(CP_Dep_female_results$`P-value`, method = "bonferroni", n = n_comparisons)
CPDep_female_results$p_adjust <- p.adjust(CPDep_female_results$`P-value`, method = "bonferroni", n = n_comparisons)


## Save results ----
write.csv(CP_Dep_full_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_full_results.csv", row.names = F)
write.csv(CPDep_full_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_full_results.csv", row.names = F)

write.csv(CP_Dep_male_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_male_results.csv", row.names = F)
write.csv(CPDep_male_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_male_results.csv", row.names = F)

write.csv(CP_Dep_female_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_female_results.csv", row.names = F)
write.csv(CPDep_female_results, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_female_results.csv", row.names = F)

write.csv(missingness_table, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/data_missingness.csv", row.names = T)

for (plot in ls(pattern="love_plot")){
  
  jpeg(file = paste0("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", plot, ".jpg"), width = 700, height = 700)
  print(get(plot))
  dev.off()
}

for (table in ls(pattern="bal_table|observation_table")){
  write.csv(get(table), paste0("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", table, ".csv"))
}

