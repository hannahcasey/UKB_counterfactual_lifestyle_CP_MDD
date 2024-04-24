#install.packages("Gmisc")
library(Gmisc, quietly = TRUE) 
library(glue)
library(grid)
library(ggplot2)
library(dplyr)
library(ggh4x)

## Load in data and results ----
CP_Dep_full_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_full_results.csv")
CPDep_full_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_full_results.csv")

CP_Dep_male_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_male_results.csv")
CPDep_male_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_male_results.csv")

CP_Dep_female_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_female_results.csv")
CPDep_female_results <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_female_results.csv")

data_eligible <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB_eligible.csv")
data <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB.csv")
EOP_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/EOP_keep.csv")
alcohol_exclude <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/alcohol_exclude.csv")
british_irish_keep <- read.csv("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/british_irish_keep.csv")

## Load in observation tables
observation_table_files <- list.files(path = "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", pattern = "observation_table")
x_coord <- 0.1
box_position <- "up"

for (observation_table in sort(observation_table_files[!grepl("sensitivity_PA_low", observation_table_files)])){
  
  table <- read.csv(paste0("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", observation_table ))
  
  ## Get sample
  if (grepl("female", observation_table)){
    sample <- "female sample"
  } else if (grepl("male", observation_table)) {
    sample <- "male sample"
  } else if (grepl("full", observation_table)) {
    sample <- "full sample"
  }
  
  ## Get exposure
  if (grepl("PA_low_", observation_table)){
    exposure <- "Pysical activity" 
    box_name <- "low_PA_box" 
  } else if (grepl("bad_sleep", observation_table)){
    exposure <- "Aberrant sleep duration" 
    box_name <- "bad_sleep_box" 
  } else if (grepl("high_alcohol_consumption", observation_table)){
    exposure <- "High Alcohol Consumption" 
    box_name <- "high_alcohol_consumption_box" 
  } else if (grepl("smoking", observation_table)){
    exposure <- "Smoking" 
    box_name <- "smoking_box" 
  } else if (grepl("obese", observation_table)){
    exposure <- "Obese" 
    box_name <- "obese_box" 
  } else if (grepl("lonely_", observation_table)){
    exposure <- "Loneliness" 
    box_name <- "loneliness_box" 
  } else if (grepl("unhealthy_diet", observation_table)){
    exposure <- "Unhealthy Diet" 
    box_name <- "unhealthy_diet_box" 
  }
  
  ## Get sample sizes in each sample
  if (grepl("full", observation_table)){
    full_n_unmatched_exposed <- table[1, "X1"]
    full_n_unmatched_unexposed <- table[1, "X0"]
    full_n_matched_exposed <- table[4, "X1"]
    full_n_matched_unexposed <- table[4, "X0"]
  } else if (grepl("female", observation_table)){
    female_n_unmatched_exposed <- table[1, "X1"]
    female_n_unmatched_unexposed <- table[1, "X0"]
    female_n_matched_exposed <- table[4, "X1"]
    female_n_matched_unexposed <- table[4, "X0"]
  } else if (grepl("_male", observation_table)){
    male_n_unmatched_exposed <- table[1, "X1"]
    male_n_unmatched_unexposed <- table[1, "X0"]
    male_n_matched_exposed <- table[4, "X1"]
    male_n_matched_unexposed <- table[4, "X0"]
  }
  
  if  (grepl("_male", observation_table)){ ## Male sample observation table read last for each exposure, only run when all tables have been read
    ## Create info flowchart box for each exposure with unmateched and matched sample sizes 
    x_coord <- x_coord + 0.1 ## Shift each box further right
    
    if (box_position == "up"){
      y_coord <- 0.45
      box_position <- "down"
    } else if (box_position == "down"){
      box_position <- "up"
      y_coord <- 0.15
    }
    
    assign(box_name, boxGrob(glue(paste(exposure, "Sample Size"),
                            "",
                            "Full sample exposed/unexposed",
                            "Unmatched: {full_n_unmatched_exposed}/{full_n_unmatched_unexposed}",
                            "Matched: {full_n_matched_exposed}/{full_n_matched_unexposed}",
                            "",
                            "Female sample exposed/unexposed:",
                            "Unmatched: {female_n_unmatched_exposed}/{female_n_unmatched_unexposed}",
                            "Matched: {female_n_matched_exposed}/{female_n_matched_unexposed}",
                            "",
                            "Male sample exposed/unexposed:",
                            "Unmatched: {male_n_unmatched_exposed}/{male_n_unmatched_unexposed}",
                            "Matched: {male_n_matched_exposed}/{male_n_matched_unexposed}",
                            full_n_unmatched_exposed = round(full_n_unmatched_exposed, 2),
                            full_n_unmatched_unexposed = round(full_n_unmatched_unexposed, 2),
                            full_n_matched_exposed = round(full_n_matched_exposed, 2),
                            full_n_matched_unexposed = round(full_n_matched_unexposed, 2),
                            female_n_unmatched_exposed = round(female_n_unmatched_exposed, 2),
                            female_n_unmatched_unexposed = round(female_n_unmatched_unexposed, 2),
                            female_n_matched_exposed = round(female_n_matched_exposed, 2),
                            female_n_matched_unexposed = round(female_n_matched_unexposed, 2),
                            male_n_unmatched_exposed = round(male_n_unmatched_exposed, 2),
                            male_n_unmatched_unexposed = round(male_n_unmatched_unexposed, 2),
                            male_n_matched_exposed = round(male_n_matched_exposed, 2),
                            male_n_matched_unexposed = round(male_n_matched_unexposed, 2),
                            .sep = "\n"), 
                       y = y_coord, x = x_coord, bjust = c(0.5, 0.5),
                       just = "left",
                       txt_gp = gpar(fontsize = 10))
    )
  }
}

## Sample flow chart ----
## Get overlap of participants with EOP data who also drink and are british/irish
EOP_alcohol_keep <- EOP_keep$x[!EOP_keep$x %in% alcohol_exclude$x]
eligible_keep <- EOP_alcohol_keep[EOP_alcohol_keep %in% british_irish_keep$x]

UKB_pop <- boxGrob(glue("UK Biobank Sample",
                        "n = {pop}",
                        pop = txtInt(nrow(data)),
                        .sep = "\n"), 
                   y = 0.9, x = 0.5, bjust = c(0.5, 0.5),
                   just = "centre")

eligible_pop <- boxGrob(glue("Eligible",
                             "n = {pop}",
                             pop = txtInt(length(eligible_keep)),
                             .sep = "\n"), 
                        y = 0.7, x = 0.5, bjust = c(0.5, 0.5),
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
                       y = 0.8, x = 0.75, bjust = c(0.5, 0.5),
                       just = "left")

grid.newpage()

UKB_pop
eligible_pop
exclude_pop

loneliness_box
low_PA_box
smoking_box
high_alcohol_consumption_box
unhealthy_diet_box
bad_sleep_box
obese_box

connectGrob(UKB_pop, eligible_pop, "N")
connectGrob(UKB_pop, exclude_pop, "L")

connectGrob(eligible_pop, loneliness_box, "N")
connectGrob(eligible_pop, obese_box, "N")
connectGrob(eligible_pop, bad_sleep_box, "N")
connectGrob(eligible_pop, low_PA_box, "N")
connectGrob(eligible_pop, unhealthy_diet_box, "N")
# connectGrob(eligible_pop, smoking_box, "N")
# connectGrob(eligible_pop, high_alcohol_consumption_box, "N")

## Format results ----
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

CP_Dep_results$exposure <- recode(CP_Dep_results$exposure,
                                  "bad_sleep" = "Aberrant sleep duration",
                                  "high_alcohol_consumption" = "High alcohol consumption",
                                  "lonely" = "Loneliness",
                                  "obese" = "Obesity",
                                  "PA_low" = "Low physical activity",
                                  "smoking" = "Smoking status",
                                  "unhealthy_diet" = "Unhealthy diet")


CPDep_results$exposure <- recode(CPDep_results$exposure,
                                  "bad_sleep" = "Aberrant sleep duration",
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
                             "CP+Dep+" = "Chronic Pain + Depression",
                             "CP-Dep-" = "No Chronic Pain + No Depression")

## Max distance between covariates after balancing----

## Load in observation tables
balancing_table_files <- list.files(path = "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", pattern = "bal_table")
max_diff <- NULL

for (balancing_table in sort(balancing_table_files)){
  table <- read.csv(paste0("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/balancing/", balancing_table))
  if (abs(max(table$Max.Diff.Adj[-1])) > 0.1){
    max_diff <- c(max_diff, balancing_table)
  }
}


## Descriptive statistics of age and sex in outcomes ----
## Entire sample
n_full <- nrow(data_eligible)
age_full <- round(mean(data_eligible$age, na.rm = T), 2)
n_females_full <- sum(data_eligible$sex == 1, na.rm = T)
prop_females_full <- round(n_females_full/nrow(data_eligible), 4) *100
## Chronic Pain
n_CP <- sum(data_eligible$followup_chronic_pain>0, na.rm = T)
prop_CP <- round(n_CP/sum(data_eligible$followup_chronic_pain>=0, na.rm = T), 4) *100
age_CP <- round(mean(data_eligible$age[data_eligible$followup_chronic_pain>0], na.rm = T), 2)
n_females_CP <- sum(data_eligible$followup_chronic_pain>0 & data_eligible$sex == 1, na.rm = T)
prop_females_CP <- round(n_females_CP/sum(data_eligible$followup_chronic_pain>0, na.rm = T), 4) *100
## Depression
n_Dep <- sum(data_eligible$followup_depression>2, na.rm = T)
prop_Dep <- round(n_Dep/sum(data_eligible$followup_depression>=0, na.rm = T), 4) *100
age_Dep <- round(mean(data_eligible$age[data_eligible$followup_depression>2], na.rm = T), 2)
n_females_Dep <- sum(data_eligible$followup_depression>2 & data_eligible$sex == 1, na.rm = T)
prop_females_Dep <- round(n_females_Dep/sum(data_eligible$followup_depression>2, na.rm = T), 4) *100
## CP-Dep-
n_noCPnoDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep-", na.rm = T)
prop_noCPnoDep <- round(n_noCPnoDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_noCPnoDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP-Dep-"], na.rm = T), 2)
n_females_noCPnoDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep-" & data_eligible$sex == 1, na.rm = T)
prop_females_noCPnoDep <- round(n_females_noCPnoDep/sum(data_eligible$comorbid_CPDep == "CP-Dep-", na.rm = T), 4) *100
## CP+Dep-
n_CPnoDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep-", na.rm = T)
prop_CPnoDep <- round(n_CPnoDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_CPnoDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP+Dep-"], na.rm = T), 2)
n_females_CPnoDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep-" & data_eligible$sex == 1, na.rm = T)
prop_females_CPnoDep <- round(n_females_CPnoDep/sum(data_eligible$comorbid_CPDep == "CP+Dep-", na.rm = T), 4) *100
## CP-Dep+
n_noCPDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep+", na.rm = T)
prop_noCPDep <- round(n_noCPDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_noCPDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP-Dep+"], na.rm = T), 2)
n_females_noCPDep <- sum(data_eligible$comorbid_CPDep == "CP-Dep+" & data_eligible$sex == 1, na.rm = T)
prop_females_noCPDep <- round(n_females_noCPDep/sum(data_eligible$comorbid_CPDep == "CP-Dep+", na.rm = T), 4) *100
## CP+Dep+
n_CPDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep+", na.rm = T)
prop_CPDep <- round(n_CPDep/length(na.omit(data_eligible$comorbid_CPDep)), 4) *100
age_CPDep <- round(mean(data_eligible$age[data_eligible$comorbid_CPDep == "CP+Dep+"], na.rm = T), 2)
n_females_CPDep <- sum(data_eligible$comorbid_CPDep == "CP+Dep+" & data_eligible$sex == 1, na.rm = T)
prop_females_CPDep <- round(n_females_CPDep/sum(data_eligible$comorbid_CPDep == "CP+Dep+", na.rm = T), 4) *100

## Combine descriptive statistics
descriptive_statistics <- data_frame(Outcome = c("Full Sample", "Chronic Pain*", "Depression*", "No Chronic Pain + No Depression",
                       "No Chronic Pain + Depression",  "Chronic Pain + No Depression",
                       "Chronic Pain + Depression"),
           "N (% of full sample)" = c(
             paste0(prettyNum(n_full, big.mark = ","), " (100%)"),
             paste0(prettyNum(n_CP, big.mark = ","), " (", prop_CP, "%)"),
             paste0(prettyNum(n_Dep, big.mark = ","), " (", prop_Dep, "%)"),
             paste0(prettyNum(n_noCPnoDep, big.mark = ","), " (", prop_noCPnoDep, "%)"),
             paste0(prettyNum(n_noCPDep, big.mark = ","), " (", prop_noCPDep, "%)"),
             paste0(prettyNum(n_CPnoDep, big.mark = ","), " (", prop_CPnoDep, "%)"),
             paste0(prettyNum(n_CPDep, big.mark = ","), " (", prop_CPDep, "%)")),
           "Mean Age" = c(age_full, age_CP, age_Dep, age_noCPnoDep, age_noCPDep, age_CPnoDep, age_CPDep),
           "N females (% of cases)" = c(
             paste0(prettyNum(n_females_full, big.mark = ","), " (", prop_females_full, "%)"),
             paste0(prettyNum(n_females_CP, big.mark = ","), " (", prop_females_CP, "%)"),
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
## Order exposures based on estimate in full sample
CP_Dep_results$exposure <- reorder(CP_Dep_results$exposure, -CP_Dep_results$Coefficient.Estimate)
CP_Dep_results$sample <- factor(CP_Dep_results$sample , levels = c("Full", "Female", "Male"))

CP_Dep_results_plot <- ggplot(CP_Dep_results[CP_Dep_results$exposure != "sensitivity_PA_low",], aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(outcome))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"), 
        text = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = expression("P"[Adjusted] ~ " < 0.05:"))


## Plot comobidity groups
## Order exposures based on estimate in full sample
CPDep_results$exposure <- factor(as.character(CPDep_results$exposure), levels = c("Low physical activity", "Unhealthy diet",
                                                                                  "High alcohol consumption", "Smoking status",
                                                                                  "Loneliness", "Aberrant sleep duration", "Obesity", "sensitivity_PA_low"))
CPDep_results$sample <- factor(CPDep_results$sample , levels = c("Full", "Female", "Male"))

CPDep_results_plot <- ggplot(CPDep_results[CPDep_results$exposure != "sensitivity_PA_low",], aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(Term))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"), 
        text = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = expression("P"[Adjusted] ~ " < 0.05:"))

## Plot sensitivity ----
CP_Dep_results_sensitivity_plot <- ggplot(CP_Dep_results[grepl("PA_low|Low physical activity", CP_Dep_results$exposure),], aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(outcome))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"), 
        text = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = expression("P"[Adjusted] ~ " < 0.05:"))


CPDep_results_sensitivity_plot <- ggplot(CPDep_results[grepl("PA_low|Low physical activity", CPDep_results$exposure),], aes(x=exposure, y=`Coefficient.Estimate`, shape=significant,colour=as.factor(Term))) +
  geom_point(aes(y=`Coefficient.Estimate`),size=3, position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=`Lower_95CI`, ymax=`Upper_95CI`), width=0, position = position_dodge(0.3)) +
  theme_minimal() +
  theme(panel.spacing = unit(3, "lines"), 
        text = element_text(size = 15),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) + 
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  coord_flip() +
  facet_wrap2(vars(sample)) +
  labs(x = "",
       y = "Coefficient Estimate",
       title = "",
       colour = "Outcome:",
       shape = expression("P"[Adjusted] ~ " < 0.05:"))

## Save descriptive stats and results plots ----
write.csv(descriptive_statistics, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/descriptive_statistics.csv", row.names = F)

jpeg("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CP_Dep_results_plot.jpg", width=30,height=20,units="cm",res=1000)
CP_Dep_results_plot
dev.off()

jpeg("/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/output/CPDep_results_plot.jpg",width=30,height=20,units="cm",res=1000)
CPDep_results_plot
dev.off()

