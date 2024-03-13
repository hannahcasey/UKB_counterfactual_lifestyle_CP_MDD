library(ggplot2)
library(dplyr)
library(ggh4x)

## Load in data and results ----
CP_Dep_full_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CP_Dep_full_results.csv")
CPDep_full_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/CPDep_full_results.csv")
data_eligible <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/UKB_eligible.csv")

CP_Dep_female_results <- CP_Dep_full_results
CP_Dep_male_results <- CP_Dep_full_results
CPDep_female_results <- CPDep_full_results
CPDep_male_results <- CPDep_full_results

## Add column indicating sample
CP_Dep_full_results$sample <- "Full"
CP_Dep_female_results$sample <- "Female"
CP_Dep_male_results$sample <- "Male"

CPDep_full_results$sample <- "Full"
CPDep_female_results$sample <- "Female"
CPDep_male_results$sample <- "Male"

## Combine all results
CP_Dep_results <- do.call("rbind", list(CP_Dep_full_results, CP_Dep_female_results, CP_Dep_male_results))
CPDep_results <- do.call("rbind", list(CPDep_full_results, CPDep_female_results, CPDep_male_results))


## Format results ----
CP_Dep_results$exposure <- recode(CP_Dep_results$exposure,
                                  "bad_sleep" = "Abberant sleep duration",
                                  "high_alcohol_consumption" = "High alcohol consumption",
                                  "lonely" = "Loneliness",
                                  "obese" = "Obesity",
                                  "PA_low" = "Low physical activity",
                                  "smoking" = "Smoking status",
                                  "unhealthy_diet" = "Unhealthy diet")


CPDep_results$exposure <- recode(CPDep_results$exposure,
                                  "bad_sleep" = "Abberant sleep duration",
                                  "high_alcohol_consumption" = "High alcohol consumption",
                                  "lonely" = "Loneliness",
                                  "obese" = "Obesity",
                                  "PA_low" = "Low physical activity",
                                  "smoking" = "Smoking status",
                                  "unhealthy_diet" = "Unhealthy diet")

CP_Dep_results$outcome <- recode(CP_Dep_results$outcome,
                                 "followup_chronic_pain" = "Chronic Pain",
                                 "followup_depression" = "Depression")

CPDep_results$Term <- recode(CPDep_results$Term,
                             "CP+Dep-" = "Chronic Pain + No Depression", 
                             "CP-Dep+" = "No Chronic Pain + Depression",
                             "CP+Dep+" = "Chronic Pain + Depression")

## Descriptive statistics of age and sex in outcomes ----
## Chronic Pain
n_CP <- sum(data_eligible$followup_chronic_pain>0, na.rm = T)
prop_CP <- round(n_CP/sum(data_eligible$followup_chronic_pain>=0, na.rm = T), 4) *100
age_CP <- round(mean(data_eligible$age[data_eligible$followup_chronic_pain>0], na.rm = T), 2)
n_females_CP <- sum(data_eligible$followup_chronic_pain>0 & data_eligible$sex == 1, na.rm = T)
prop_females_CP <- round(n_females_CP/sum(data_eligible$followup_chronic_pain>0 & data_eligible$sex >= 0, na.rm = T), 4) *100
## Depression
n_Dep <- sum(data_eligible$followup_depression>2, na.rm = T)
prop_Dep <- round(n_Dep/sum(data_eligible$followup_depression>=0, na.rm = T), 4) *100
age_Dep <- round(mean(data_eligible$age[data_eligible$followup_depression>2], na.rm = T), 2)
n_females_Dep <- sum(data_eligible$followup_depression>2 & data_eligible$sex == 1, na.rm = T)
prop_females_Dep <- round(n_females_Dep/sum(data_eligible$followup_depression>2 & data_eligible$sex >= 0, na.rm = T), 4) *100
## CP-Dep-
n_noCPnoDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep-", na.rm = T)
prop_noCPnoDep <- round(n_noCPnoDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_noCPnoDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP-Dep-"], na.rm = T), 2)
n_females_noCPnoDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep-" & data_eligible$sex == 1, na.rm = T)
prop_females_noCPnoDep <- round(n_females_noCPnoDep/sum(data_eligible$comorbid_CPDep == "CP-Dep-" & data_eligible$sex >= 0, na.rm = T), 4) *100
## CP+Dep-
n_CPnoDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep-", na.rm = T)
prop_CPnoDep <- round(n_CPnoDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_CPnoDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP+Dep-"], na.rm = T), 2)
n_females_CPnoDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep-" & data_eligible$sex == 1, na.rm = T)
prop_females_CPnoDep <- round(n_females_CPnoDep/sum(data_eligible$comorbid_CPDep == "CP+Dep-" & data_eligible$sex >= 0, na.rm = T), 4) *100
## CP-Dep+
n_noCPDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep+", na.rm = T)
prop_noCPDep <- round(n_noCPDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_noCPDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP-Dep+"], na.rm = T), 2)
n_females_noCPDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep+" & data_eligible$sex == 1, na.rm = T)
prop_females_noCPDep <- round(n_females_noCPDep/sum(data_eligible$comorbid_CPDep == "CP-Dep+" & data_eligible$sex >= 0, na.rm = T), 4) *100
## CP+Dep+
n_CPDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep+", na.rm = T)
prop_CPDep <- round(n_CPDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_CPDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP+Dep+"], na.rm = T), 2)
n_females_CPDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep+" & data_eligible$sex == 1, na.rm = T)
prop_females_CPDep <- round(n_females_CPDep/sum(data_eligible$comorbid_CPDep == "CP+Dep+" & data_eligible$sex >= 0, na.rm = T), 4) *100

## Combine descriptive statistics
descriptive_statistics <- data_frame(Outcome = c("Chonic Pain*", "Depression*", "No Chronic Pain + No Depression",
                       "Chronic Pain + No Depression", "No Chronic Pain + Depression",
                       "Chronic Pain + Depression"),
           "N (%)" = c(paste0(prettyNum(n_CP, big.mark = ","), " (", prop_CP, "%)"),
                      paste0(prettyNum(n_Dep, big.mark = ","), " (", prop_Dep, "%)"),
                      paste0(prettyNum(n_noCPnoDep, big.mark = ","), " (", prop_noCPnoDep, "%)"),
                      paste0(prettyNum(n_noCPDep, big.mark = ","), " (", prop_noCPDep, "%)"),
                      paste0(prettyNum(n_CPnoDep, big.mark = ","), " (", prop_CPnoDep, "%)"),
                      paste0(prettyNum(n_CPDep, big.mark = ","), " (", prop_CPDep, "%)")),
           "Mean Age" = c(age_CP, age_Dep, age_noCPnoDep, age_CPnoDep, age_noCPDep, age_CPDep),
           "N females (%)" = c(paste0(prettyNum(n_females_CP, big.mark = ","), " (", prop_females_CP, "%)"),
                               paste0(prettyNum(n_females_Dep, big.mark = ","), " (", prop_females_Dep, "%)"),
                               paste0(prettyNum(n_females_noCPnoDep, big.mark = ","), " (", prop_females_noCPnoDep, "%)"),
                               paste0(prettyNum(n_females_noCPDep, big.mark = ","), " (", prop_females_noCPDep, "%)"),
                               paste0(prettyNum(n_females_CPnoDep, big.mark = ","), " (", prop_females_CPnoDep, "%)"),
                               paste0(prettyNum(n_females_CPDep, big.mark = ","), " (", prop_females_CPDep, "%)"))
           )


## Add column indicating statistical significance ----
CP_Dep_results <- CP_Dep_results %>%
  mutate(significant = ifelse(CP_Dep_results$p_adjust < 0.05,
                                             "Yes", "No"))

CPDep_results <- CPDep_results %>%
  mutate(significant = ifelse(CPDep_results$p_adjust < 0.05,
                                            "Yes", "No"))

## Plot chronic pain and depression ----
CP_Dep_results_plot <- ggplot(CP_Dep_results, aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(outcome))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, alpha = 0.5, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, alpha = 0.5, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines")) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = "Adjusted p-value < 0.05:")


## Plot comobidity groups
CPDep_results_plot <- ggplot(CPDep_results, aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(Term))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, alpha = 0.5, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, alpha = 0.5, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines")) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = "Adjusted p-value < 0.05:")

## Save descriptive stats and results plots
write.csv(descriptive_statistics, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/descriptive_statistics.csv", row.names = F)

jpeg("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_results_plot.jpg", width = 700, height = 700)
CP_Dep_results_plot
dev.off()

jpeg("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_results_plot.jpg", width = 700, height = 700)
CPDep_results_plot
dev.off()
