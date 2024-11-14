library(dplyr)
library(lubridate)


## Get data ----
### Touchscreen ----
touchscreen <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/Touchscreen.rds")
## Qualification (6138)
## Employment status (6142)
## Living with partner (709, 6141)
## General health (2178)
## Loneliness (2020)
## Alcohol consumption (1558, 1568, 1578, 1588, 1598, 1608)
## Smoking (1239)
## Sleep (1160)
## Diet (1309, 1319, 1289, 1299, 1329, 1339, 1349, 1369, 1379, 1389, 1438, 1448, 1458, 1468)
## Ethnicity (21000)
## Baseline chronic pain sites
  ## Pain types (6159)
  ## Widespread (2956)
  ## Head (3799)
  ## Face (4067)
  ## Neck/shoulder (3404)
  ## Back (3571)
  ## Stomach/Abdominal (3741)
  ## Hip (3414)
  ## Knee (3773)
## Physical Activity (to calculate MET score)
  ## Duration of walks (874)
  ## Number of days/week of moderate physical activity 10+ minutes (864)
  ## Duration of moderate activity (894)
  ## Number of days/week of moderate physical activity 10+ minutes (884)
  ## Duration of vigorous activity (914)
  ## Number of days/week of vigorous physical activity 10+ minutes (904)
## Baseline depression (PHQ-2)
  ## Frequency of depressed mood in last 2 weeks (2050)
  ## Frequency of unenthusiasm / disinterest in last 2 weeks (2060)

touchscreen_sm <- touchscreen[ ,grepl("f.eid", names(touchscreen)) |
                                 grepl("f.6138.0.", names(touchscreen))|
                                 grepl("f.6142.0.", names(touchscreen))|
                                 grepl("f.709.0.", names(touchscreen))|
                                 grepl("f.6141.0.", names(touchscreen))|
                                 grepl("f.24506.0.", names(touchscreen))|
                                 grepl("f.2178.0.", names(touchscreen))|
                                 grepl("f.2020.0.", names(touchscreen))|
                                 grepl("f.1558.0.", names(touchscreen))|
                                 grepl("f.1568.0.", names(touchscreen))|
                                 grepl("f.1578.0.", names(touchscreen))|
                                 grepl("f.1588.0.", names(touchscreen))|
                                 grepl("f.1598.0.", names(touchscreen))|
                                 grepl("f.1608.0.", names(touchscreen))|
                                 grepl("f.1239.0.", names(touchscreen))|
                                 grepl("f.1160.0.", names(touchscreen))|
                                 grepl("f.1309.0.", names(touchscreen))|
                                 grepl("f.1319.0.", names(touchscreen))|
                                 grepl("f.1289.0.", names(touchscreen))|
                                 grepl("f.1299.0.", names(touchscreen))|
                                 grepl("f.1329.0.", names(touchscreen))|
                                 grepl("f.1339.0.", names(touchscreen))|
                                 grepl("f.1349.0.", names(touchscreen))|
                                 grepl("f.1369.0.", names(touchscreen))|
                                 grepl("f.1379.0.", names(touchscreen))|
                                 grepl("f.1389.0.", names(touchscreen))|
                                 grepl("f.1438.0.", names(touchscreen))|
                                 grepl("f.1448.0.", names(touchscreen))|
                                 grepl("f.1458.0.", names(touchscreen))|
                                 grepl("f.1468.0.", names(touchscreen))|
                                 grepl("f.21000.0.", names(touchscreen))|
                                 grepl("f.6159.0.", names(touchscreen))|
                                 grepl("f.2956.0.", names(touchscreen))|
                                 grepl("f.3799.0.", names(touchscreen))|
                                 grepl("f.4067.0.", names(touchscreen))|
                                 grepl("f.3404.0.", names(touchscreen))|
                                 grepl("f.3571.0.", names(touchscreen))|
                                 grepl("f.3741.0.", names(touchscreen))|
                                 grepl("f.3414.0.", names(touchscreen))|
                                 grepl("f.3773.0.", names(touchscreen))|
                                 grepl("f.874.0.", names(touchscreen))|
                                 grepl("f.864.0.", names(touchscreen))|
                                 grepl("f.894.0.", names(touchscreen))|
                                 grepl("f.884.0.", names(touchscreen))|
                                 grepl("f.914.0.", names(touchscreen))|
                                 grepl("f.904.0.", names(touchscreen))|
                                 grepl("f.2050.0.", names(touchscreen))|
                                 grepl("f.2060.0.", names(touchscreen))]

### Procedural metrics ----
procedural_metrics <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2020-09-phenotypes-ukb43743/ProceduralMetrics.rds")
## Touchscreen questionnaire date (21822)

procedural_metrics_sm <- procedural_metrics[ ,grepl("f.eid", names(procedural_metrics)) |
                                                           grepl("f.21822.0.", names(procedural_metrics))]
### Baseline characteristics ----
baseline_characteristics <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/BaselineCharacteristics.rds")
## Sex (31)
## Deprivation (189)

baseline_characteristics_sm <- baseline_characteristics[ ,grepl("f.eid", names(baseline_characteristics)) |
                                                           grepl("f.31.", names(baseline_characteristics)) |
                                                           grepl("f.189.", names(baseline_characteristics))]
### Recruitment ----
recruitment <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/Recruitment.rds")
## Age (21003)

recruitment_sm <- recruitment[ ,grepl("f.eid", names(recruitment)) |
                                 grepl("f.21003.0.", names(recruitment))]
### Physical measures ----
physical_measures <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/PhysicalMeasures.rds")
## BMI (21001)

physical_measures_sm <- physical_measures[ ,grepl("f.eid", names(physical_measures)) |
                                             grepl("f.21001.0.", names(physical_measures))]

### Experience of pain ----
EOP <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-chronicpain-ukb45596/ExperienceOfPain.rds")
## PHQ-2 (120104, 120105)
  ## Little interest or pleasure in doing things over the last two weeks (120104)
  ## Feeling down, depressed, or hopeless over the last two weeks (120105)
## Chronic pain sites
  ## Pain 3+ months (120019)
  ## Widespread (120021)
  ## Head (120023)
  ## Face (120024)
  ## Neck/shoulder (120025)
  ## Back (120026)
  ## Stomach/Abdominal (120027)
  ## Hip (120028)
  ## Knee (120029)
## Date EOP completed (120128)

EOP_sm <- EOP[ ,grepl("f.eid", names(EOP)) |
                 grepl("f.120104.", names(EOP)) |
                 grepl("f.120105.", names(EOP)) |
                 grepl("f.120019.", names(EOP)) |
                 grepl("f.120021.", names(EOP)) |
                 grepl("f.120023.", names(EOP)) |
                 grepl("f.120024.", names(EOP)) |
                 grepl("f.120025.", names(EOP)) |
                 grepl("f.120026.", names(EOP)) |
                 grepl("f.120027.", names(EOP)) |
                 grepl("f.120028.", names(EOP)) |
                 grepl("f.120029.", names(EOP)) |
                 grepl("f.120128.", names(EOP))]


## Combine data ----
data <- touchscreen_sm %>%
  full_join(baseline_characteristics_sm, by='f.eid') %>%
  full_join(procedural_metrics_sm, by='f.eid') %>%
  full_join(recruitment_sm, by='f.eid') %>%
  full_join(physical_measures_sm, by='f.eid') %>%
  full_join(EOP_sm, by='f.eid')

## Remove large datasets

## Outcomes ----

### Follow-up chronic pain ----
## Number of chronic pain sites at follow-up
data$followup_chronic_pain <- NA
data$followup_chronic_pain[data$f.120019.0.0 == "No"] <- 0
data$followup_chronic_pain[data$f.120019.0.0 == "Do not know"] <- NA
data$followup_chronic_pain[data$f.120019.0.0 == "Prefer not to answer"] <- NA

## Record if general pain has been experienced for 3+ months
data$followup_chronic_widespread_pain <- data$followup_chronic_pain
data$followup_chronic_widespread_pain[data$f.120021.0.0 == "Yes"] <- 1 ## Record as having general chronic pain 
data$followup_chronic_widespread_pain[data$f.120021.0.0 == "Prefer not to answer" | data$f.120021.0.0 == "Do not know"] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA
data$followup_chronic_widespread_pain[data$f.120021.0.0 == "No"] <- 0 ## No general chronic pain 

## Record if headache has been experienced for 3+ months
data$followup_chronic_headache <- NA
data$followup_chronic_headache[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120023.0.0 == -100 | data$f.120023.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no headache or discomfort (-100), record no chronic headache
data$followup_chronic_headache[data$f.120023.0.0 > 0 | data$f.120023.0.0 == -99] <- 1 ## Record as having chronic pain if headache 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_headache[data$f.120023.0.0 == -121 | data$f.120023.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if facial pain has been experienced for 3+ months
data$followup_chronic_facial_pain <- NA
data$followup_chronic_facial_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120024.0.0 == -100 | data$f.120024.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no facial pain or discomfort (-100), record no chronic facial pain
data$followup_chronic_facial_pain[data$f.120024.0.0 > 0 | data$f.120024.0.0 == -99] <- 1 ## Record as having chronic pain if facial pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_facial_pain[data$f.120024.0.0 == -121 | data$f.120024.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if Neck/shoulder pain has been experienced for 3+ months
data$followup_chronic_neck_shoulder_pain <- NA
data$followup_chronic_neck_shoulder_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120025.0.0 == -100 | data$f.120025.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no neck/shoulder pain or discomfort (-100), record no chronic neck/shoulder pain
data$followup_chronic_neck_shoulder_pain[data$f.120025.0.0 > 0 | data$f.120025.0.0 == -99] <- 1 ## Record as having chronic pain if neck/shoulder pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_neck_shoulder_pain[data$f.120025.0.0 == -121 | data$f.120025.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if back pain has been experienced for 3+ months
data$followup_chronic_back_pain <- NA
data$followup_chronic_back_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120026.0.0 == -100 | data$f.120026.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no back pain or discomfort (-100), record no chronic back pain
data$followup_chronic_back_pain[data$f.120026.0.0 > 0 | data$f.120026.0.0 == -99] <- 1 ## Record as having chronic pain if back pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_back_pain[data$f.120026.0.0 == -121 | data$f.120026.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if stomach/abdominal pain has been experienced for 3+ months
data$followup_chronic_stomach_abdominal_pain <- NA
data$followup_chronic_stomach_abdominal_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120027.0.0 == -100 | data$f.120027.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no stomach/abdominal pain or discomfort (-100), record no chronic stomach/abdominal pain
data$followup_chronic_stomach_abdominal_pain[data$f.120027.0.0 > 0 | data$f.120027.0.0 == -99] <- 1 ## Record as having chronic pain if stomach/abdominal pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_stomach_abdominal_pain[data$f.120027.0.0 == -121 | data$f.120027.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if hip pain has been experienced for 3+ months
data$followup_chronic_hip_pain <- NA
data$followup_chronic_hip_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120028.0.0 == -100 | data$f.120028.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no hip pain or discomfort (-100), record no chronic hip pain
data$followup_chronic_hip_pain[data$f.120028.0.0 > 0 | data$f.120028.0.0 == -99] <- 1 ## Record as having chronic pain if hip pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_hip_pain[data$f.120028.0.0 == -121 | data$f.120028.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Record if knee pain has been experienced for 3+ months
data$followup_chronic_knee_pain <- NA
data$followup_chronic_knee_pain[data$followup_chronic_pain == 0 | data$followup_chronic_widespread_pain == 1 | data$f.120029.0.0 == -100 | data$f.120029.0.0 == 0] <- 0 ## If No chronic pain, widespread chronic pain already reported or no knee pain or discomfort (-100), record no chronic knee pain
data$followup_chronic_knee_pain[data$f.120029.0.0 > 0 | data$f.120029.0.0 == -99] <- 1 ## Record as having chronic pain if knee pain 3+ months (include "Pain with unspecified rating") 
data$followup_chronic_knee_pain[data$f.120029.0.0 == -121 | data$f.120029.0.0 == -818] <- NA ## If "Do not know" or "Prefer not to answer", mark as NA

## Sum chronic pain sites
data$followup_chronic_pain <- data$followup_chronic_headache + data$followup_chronic_facial_pain + data$followup_chronic_neck_shoulder_pain +
  data$followup_chronic_back_pain + data$followup_chronic_stomach_abdominal_pain + data$followup_chronic_hip_pain +
  data$followup_chronic_knee_pain

## If widespread chronic pain recorded, assign 8 chronic pain sites
data$followup_chronic_pain[data$followup_chronic_widespread_pain == 1] <- 8

## Get list of participants with "Experience of Pain" quesitonnaire data
EOP_keep <- data$f.eid[!is.na(data$f.120128.0.0)]

### Follow-up depression ----

## PHQ-2 (120104, 120105)
## Little interest or pleasure in doing things over the last two weeks (120104)
## Feeling down, depressed, or hopeless over the last two weeks (120105)

data$f.120104

## PHQ-2 score
data$followup_depressed_mood <- recode(data$f.120104.0.0,
                                `-521` = 0,
                                `-522` = 1,
                                `-523` = 2,
                                `-524` = 3,
                                `-818` = NaN)

data$followup_anhedonia <- recode(data$f.120105.0.0,
                                `-521` = 0,
                                `-522` = 1,
                                `-523` = 2,
                                `-524` = 3,
                                `-818` = NaN)

data$followup_depression <- data$followup_depressed_mood + data$followup_anhedonia


### Follow-up comorbid chronic pain and dperession ----
data$comorbid_CPDep <- NA
data$comorbid_CPDep[data$followup_depression > 2 & data$followup_chronic_pain > 0] <- "CP+Dep+"
data$comorbid_CPDep[data$followup_depression > 2 & data$followup_chronic_pain == 0] <- "CP-Dep+"
data$comorbid_CPDep[data$followup_depression < 3 & data$followup_chronic_pain > 0] <- "CP+Dep-"
data$comorbid_CPDep[data$followup_depression < 3 & data$followup_chronic_pain == 0] <- "CP-Dep-"
table(data$comorbid_CPDep)

## Treatments ----

### Low physcial activity ----
## International Physical Activity Questionnaire short form:
  ## Physical activity guidelines of 150 minutes of walking or moderate activity per week or 75 minutes of vigorous activity, or combination

## Where no days spent doing activity, indicate time spent as 0
data$f.874.0.0[data$f.864.0.0 == 0] <- 0
data$f.894.0.0[data$f.884.0.0 == 0] <- 0
data$f.914.0.0[data$f.904.0.0 == 0] <- 0

## Replace -1 and -3 with NA
data$f.874.0.0[data$f.874.0.0 == -1 | data$f.874.0.0 == -3] <- NA
data$f.894.0.0[data$f.894.0.0 == -1 | data$f.894.0.0 == -3] <- NA
data$f.914.0.0[data$f.914.0.0 == -1 | data$f.914.0.0 == -3] <- NA

## Indicate if participants get recommended PA from walking, moderate and vigorous exercise
data$PA_low <- NA
data$PA_low[(data$f.874.0.0 + data$f.894.0.0 + (data$f.914.0.0 * 2) * 7) > 150] <- 0
data$PA_low[(data$f.874.0.0 + data$f.894.0.0 + (data$f.914.0.0 * 2) * 7) < 150] <- 1
table(data$PA_low)

### Aberrant sleep duration ---- 
## Poor sleep defined as less than 7 or more than 9 hours
data$bad_sleep <- NA
data$bad_sleep[data$f.1160.0.0 < 7 | data$f.1160.0.0 > 9] <- 1
data$bad_sleep[data$f.1160.0.0 >= 7 & data$f.1160.0.0 <= 9] <- 0
table(data$bad_sleep)

## Too much sleep defined as more than 9 hours of sleep
data$too_much_sleep <- NA
data$too_much_sleep[data$f.1160.0.0 > 9] <- 1
data$too_much_sleep[data$f.1160.0.0 <= 9] <- 0
table(data$too_much_sleep)

## Too little sleep defined as more than 9 hours of sleep
data$too_little_sleep <- NA
data$too_little_sleep[data$f.1160.0.0 < 7] <- 1
data$too_little_sleep[data$f.1160.0.0 >= 7] <- 0
table(data$too_little_sleep)

### Loneliness ----
## Answered "yes" to "Do you often feel lonely?"
data$lonely <- NA
data$lonely[data$f.2020.0.0 == "Yes"] <- 1
data$lonely[data$f.2020.0.0 == "No"] <- 0
table(data$lonely)

### Smoking ----
## Answered "yes", "on most or all days" or "only occasionally"
data$smoking <- NA
data$smoking[data$f.1239.0.0 == "Yes, on most or all days" | data$f.1239.0.0 == "Only occasionally"] <- 1
data$smoking[data$f.1239.0.0 == "No"] <- 0
table(data$smoking)

### High alcohol consumption ----
## Sum total number of units per month
## Alcohol intake frequency (1558)
## Glasses of red wine per week (1568) - 2.1 units
## Glasses of white wine/champagne per week (1578)- 2.1 units
## Pints of cider/beer per week (1588)- 2 units
## Measures of spirits per week (1598)- 1 unit
## Glasses of fortified wine per week (1608) - 2.1 units
## High alcohol consumption > 14 units/week

data$high_alcohol_consumption <- NA
## Only participants reporting alcohol intake more than 1-3 times a month asked about unit consumption, for those who seldom drink indicate no high alcohol consumption status
data$high_alcohol_consumption[data$f.1558.0.0 == "One to three times a month" | data$f.1558.0.0 == "Special occasions only"] <- 0
data$high_alcohol_consumption[((data$f.1568.0.0 * 2.1) + (data$f.1578.0.0 * 2.1) + (data$f.1588.0.0 * 2) + data$f.1598.0.0 + (data$f.1608.0.0 * 2.1)) > 14] <- 1
data$high_alcohol_consumption[((data$f.1568.0.0 * 2.1) + (data$f.1578.0.0 * 2.1) + (data$f.1588.0.0 * 2) + data$f.1598.0.0 + (data$f.1608.0.0 * 2.1)) <= 14] <- 0
table(data$high_alcohol_consumption)

## Create exclude list of those who reported never drinking
alcohol_exclude <- na.omit(data$f.eid[data$f.1558.0.0 == "Never"])

### Obesity ----
## BMI greater than 30
data$obese <- NA
data$obese[data$f.21001.0.0 >= 30] <- 1
data$obese[data$f.21001.0.0 < 30] <- 0
table(data$obese)

### Unhealthy Diet ----
## Less than 4 of the following 7 food groups:
## Fruits: ≥ 3 servings/day (1309, 1319)
## Vegetables: ≥ 3 servings/day (1289, 1299)
## Fish: ≥2 servings/week (1329, 1339)
## Processed meats: ≤ 1 serving/week (1349)
## Unprocessed red meats: ≤ 1.5 servings/week (1369, 1379, 1389)
## Whole grains: ≥ 3servings/day (based on bread and cereal intake: 1438, 1458 and grain type: 1448, 1468)
## Refined grains: ≤1.5servings/day (based on bread and cereal intake: 1438, 1458 and grain type: 1448, 1468)

## Estimate fruit intake
data$fresh_fruit <- data$f.1309.0.0
data$fresh_fruit[data$fresh_fruit < 0] <- NA
data$dried_fruit <- data$f.1319.0.0
data$dried_fruit[data$dried_fruit < 0] <- NA
data$fruit <- ifelse((data$fresh_fruit + data$dried_fruit) >= 3, 1,0) ## Fruit intake at least 3

## Estimate veg intake
data$cooked_veg <- data$f.1289.0.0
data$cooked_veg[data$cooked_veg < 0] <- NA
data$raw_veg <- data$f.1299.0.0
data$raw_veg[data$raw_veg < 0] <- NA
data$veg <- ifelse((data$cooked_veg + data$raw_veg) >= 3, 1,0) ## Veg intake at least 3

## Estimate fish intake
data$oily_fish <-recode(data$f.1329.0.0,
                        'Do not know' = NaN,
                        'Prefer not to answer' = NaN,
                        'Never' = 0,
                        'Less than once a week' = 0,
                        'Once a week' = 1,
                        '2-4 times a week' = 3,
                        '5-6 times a week' = 5.5,
                        'Once or more daily' = 7)

data$non_oily_fish <-recode(data$f.1339.0.0,
                        'Do not know' = NaN,
                        'Prefer not to answer' = NaN,
                        'Never' = 0,
                        'Less than once a week' = 0,
                        'Once a week' = 1,
                        '2-4 times a week' = 3,
                        '5-6 times a week' = 5.5,
                        'Once or more daily' = 7)

data$fish <- ifelse((data$oily_fish + data$non_oily_fish) >= 2, 1,0) ## Fish intake at least 2

## Estimate processed meat intake
data$processed_meat <- NA
data$processed_meat[data$f.1349.0.0 == "Never" | data$f.1349.0.0 == "Less than once a week" | data$f.1349.0.0 == "Once a week"] <- 1
data$processed_meat[data$f.1349.0.0 == "2-4 times a week" | data$f.1349.0.0 == "5-6 times a week" | data$f.1349.0.0 == "Once or more daily"] <- 0

## Estimate unprocessed meat intake
data$beef <-recode(data$f.1369.0.0,
                        'Do not know' = NaN,
                        'Prefer not to answer' = NaN,
                        'Never' = 0,
                        'Less than once a week' = 0,
                        'Once a week' = 1,
                        '2-4 times a week' = 3,
                        '5-6 times a week' = 5.5,
                        'Once or more daily' = 7)

data$lamb <-recode(data$f.1379.0.0,
                   'Do not know' = NaN,
                   'Prefer not to answer' = NaN,
                   'Never' = 0,
                   'Less than once a week' = 0,
                   'Once a week' = 1,
                   '2-4 times a week' = 3,
                   '5-6 times a week' = 5.5,
                   'Once or more daily' = 7)

data$pork <-recode(data$f.1389.0.0,
                   'Do not know' = NaN,
                   'Prefer not to answer' = NaN,
                   'Never' = 0,
                   'Less than once a week' = 0,
                   'Once a week' = 1,
                   '2-4 times a week' = 3,
                   '5-6 times a week' = 5.5,
                   'Once or more daily' = 7)

data$unprocessed_meat <- ifelse((data$beef + data$lamb + data$pork) <= 1.5, 1,0) ## unprocessed meat less than 1.5

## Estimate daily whole grain intake
## Bread
data$whole_grain_bread <- data$f.1438.0.0
## Replace negative values with NA
data$whole_grain_bread[data$whole_grain_bread < 0] <- NA
## Replace with 0 if non-wholegrain
data$whole_grain_bread[data$f.1448.0.0 == "White" | data$f.1448.0.0 == "Other type of bread"] <- 0
## Replace unknown with NA  
data$whole_grain_bread[data$f.1448.0.0 == "Prefer not to answer" |
                         data$f.1448.0.0 == "Do not know" |
                         is.na(data$f.1448.0.0 )] <- NA
 
## Cereal
data$whole_grain_cereal <- data$f.1458.0.0
## Replace negative values with NA
data$whole_grain_cereal[data$whole_grain_cereal < 0] <- NA
## Replace with 0 if non-wholegrain
data$whole_grain_cereal[data$f.1468.0.0 == "Biscuit cereal (e.g. Weetabix)" |
                          data$f.1468.0.0 == "Other (e.g. Cornflakes, Frosties)" |
                          data$f.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)"] <- 0
## Replace unknown with NA  
data$whole_grain_cereal[data$f.1468.0.0 == "Prefer not to answer" |
                          data$f.1468.0.0 == "Do not know" |
                          is.na(data$f.1468.0.0 )] <- NA

data$whole_grain <- ifelse((data$whole_grain_bread + data$whole_grain_cereal) / 7 >= 3, 1,0) ## wholegrain greater than 3

## Estimate daily refined grain intake
## Bread
data$refined_grain_bread <- data$f.1438.0.0
## Replace negative values with NA
data$refined_grain_bread[data$refined_grain_bread < 0] <- NA

## Replace with 0 if non-refined grain
data$refined_grain_bread[data$f.1448.0.0 == "Wholemeal or wholegrain" | data$f.1448.0.0 == "Brown"] <- 0
## Replace unknown with NA  
data$refined_grain_bread[data$f.1448.0.0 == "Prefer not to answer" |
                         data$f.1448.0.0 == "Do not know" |
                         is.na(data$f.1448.0.0 )] <- NA

## Cereal
data$refined_grain_cereal <- data$f.1458.0.0
## Replace negative values with NA
data$refined_grain_cereal[data$refined_grain_cereal < 0] <- NA
## Replace with 0 if non-refined grain
data$refined_grain_cereal[data$f.1468.0.0 == "Bran cereal (e.g. All Bran, Branflakes)" |
                          data$f.1468.0.0 == "Oat cereal (e.g. Ready Brek, porridge)" |
                          data$f.1468.0.0 == "Muesli"] <- 0
## Replace unknown with NA  
data$refined_grain_cereal[data$f.1468.0.0 == "Prefer not to answer" |
                          data$f.1468.0.0 == "Do not know" |
                          is.na(data$f.1468.0.0 )] <- NA


data$refined_grain <- ifelse((data$refined_grain_bread + data$refined_grain_cereal) / 7 <= 1.5, 1,0)  ## refined grain intake less than 1.5

## Calculate healthy diet
data$unhealthy_diet <- ifelse((data$fruit + data$veg + data$fish + data$unprocessed_meat + data$processed_meat + data$whole_grain + data$refined_grain) < 4, 1,0) ## healthy diet score less than 4
table(data$unhealthy_diet)

## Matching variables ----

### Age ----
data$age <- data$f.21003.0.0

### Sex ----
data$sex <- recode(data$f.31.0.0,
                   'Male' = 0,
                   'Female' = 1)

### Education ----
## Get number of years in education based on highest qualification
data$education <- NA

## Record highest qualification as correspanding years in education
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "None of the above"))] <- 7
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "CSEs or equivalent"))] <- 10
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "O levels/GCSEs or equivalent"))] <- 10
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "A levels/AS levels or equivalent"))] <- 13
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "Other professional qualifications eg: nursing, teaching"))] <- 15
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "NVQ or HND or HNC or equivalent"))] <- 19
data$education[apply(data[,grepl("f.6138.0.", names(data))], 1, function(r) any(r == "College or University degree"))] <- 20


### Employment status ----
data$employment <- NA

data$employment[apply(data[,grepl("f.6142.0.", names(data))], 1, function(r) any(r %in%  c("Retired",
                                                                                           "Looking after home and/or family",
                                                                                           "Unable to work because of sickness or disability",
                                                                                           "Unemployed",
                                                                                           "Doing unpaid or voluntary work",
                                                                                           "Full or part-time student")))] <- 0

data$employment[apply(data[,grepl("f.6142.0.", names(data))], 1, function(r) any(r == "In paid employment or self-employed"))] <- 1


### Deprivation ----
data$deprivation <- data$f.189.0.0


### Living with partner ----
## Number in household (709)
## How are people in household related to participant (6141)
data$living_with_partner <- NA
data$living_with_partner[data$f.709.0.0 == 1] <- 0

data$living_with_partner[apply(data[,grepl("f.6141.0.", names(data))], 1, function(r) any(r %in%  c("Other unrelated",
                                                                                           "Son and/or daughter (include step-children)",
                                                                                           "Mother and/or father",
                                                                                           "Brother and/or sister",
                                                                                           "Other related",
                                                                                           "Grandparent",
                                                                                           "Grandchild")))] <- 0

data$living_with_partner[apply(data[,grepl("f.6141.0.", names(data))], 1, function(r) any(r == "Husband, wife or partner"))] <- 1
table(data$living_with_partner)
### General health score ----
data$general_health <- NA
data$general_health[data$f.2178.0.0 == "Excellent"] <- 1
data$general_health[data$f.2178.0.0 == "Good"] <- 2
data$general_health[data$f.2178.0.0 == "Fair"] <- 3
data$general_health[data$f.2178.0.0 == "Poor"] <- 4


### Baseline chronic pain ----
## Number of chronic pain  body sites
data$baseline_chronic_pain <- 0
data$baseline_chronic_pain[data$f.6159.0.0 == "Prefer not to answer"] <- NA

data$baseline_chronic_headache <- data$baseline_chronic_pain
data$baseline_chronic_facial_pain <- data$baseline_chronic_pain
data$baseline_chronic_neck_shoulder_pain <- data$baseline_chronic_pain
data$baseline_chronic_back_pain <- data$baseline_chronic_pain
data$baseline_chronic_stomach_abdominal_pain <- data$baseline_chronic_pain
data$baseline_chronic_hip_pain <- data$baseline_chronic_pain
data$baseline_chronic_knee_pain <- data$baseline_chronic_pain
data$baseline_chronic_widespread_pain <- data$baseline_chronic_pain

data$baseline_chronic_headache[data$f.3799.0.0 == "Yes"] <- 1 ## Record as having chronic pain if headache 3+ months
data$baseline_chronic_headache[data$f.3799.0.0 == "No"] <- 0
data$baseline_chronic_headache[data$f.3799.0.0 == "Do not know" | data$f.3799.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_facial_pain[data$f.4067.0.0 == "Yes"] <- 1 ## Record as having chronic pain if facial pain 3+ months
data$baseline_chronic_facial_pain[data$f.4067.0.0 == "No"] <- 0
data$baseline_chronic_facial_pain[data$f.4067.0.0 == "Do not know" | data$f.4067.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_neck_shoulder_pain[data$f.3404.0.0 == "Yes"] <- 1 ## Record as having chronic pain if neck/shoulder pain 3+ months
data$baseline_chronic_neck_shoulder_pain[data$f.3404.0.0 == "No"] <- 0
data$baseline_chronic_neck_shoulder_pain[data$f.3404.0.0 == "Do not know" | data$f.3404.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_back_pain[data$f.3571.0.0 == "Yes"] <- 1 ## Record as having chronic pain if back pain 3+ months
data$baseline_chronic_back_pain[data$f.3571.0.0 == "No"] <- 0
data$baseline_chronic_back_pain[data$f.3571.0.0 == "Do not know" | data$f.3571.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_stomach_abdominal_pain[data$f.3741.0.0 == "Yes"] <- 1 ## Record as having chronic pain if stomach/abdominal pain 3+ months
data$baseline_chronic_stomach_abdominal_pain[data$f.3741.0.0 == "No"] <- 0
data$baseline_chronic_stomach_abdominal_pain[data$f.3741.0.0 == "Do not know" | data$f.3741.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_hip_pain[data$f.3414.0.0 == "Yes"] <- 1 ## Record as having chronic pain if hip pain 3+ months
data$baseline_chronic_hip_pain[data$f.3414.0.0 == "No"] <- 0
data$baseline_chronic_hip_pain[data$f.3414.0.0 == "Do not know" | data$f.3414.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_knee_pain[data$f.3773.0.0 == "Yes"] <- 1 ## Record as having chronic pain if knee pain 3+ months
data$baseline_chronic_knee_pain[data$f.3773.0.0 == "No"] <- 0
data$baseline_chronic_knee_pain[data$f.3773.0.0 == "Do not know" | data$f.3773.0.0 == "Prefer not to answer"] <- NA
data$baseline_chronic_widespread_pain[data$f.2956.0.0 == "Yes"] <- 1 ## Record as having chronic pain if widespread pain 3+ months
data$baseline_chronic_widespread_pain[data$f.2956.0.0 == "No"] <- 0
data$baseline_chronic_widespread_pain[data$f.2956.0.0 == "Do not know" | data$f.2956.0.0 == "Prefer not to answer"] <- NA

## Sum chronic pain sites
data$baseline_chronic_pain <- data$baseline_chronic_headache + data$baseline_chronic_facial_pain + data$baseline_chronic_neck_shoulder_pain +
  data$baseline_chronic_back_pain + data$baseline_chronic_stomach_abdominal_pain + data$baseline_chronic_hip_pain + 
  data$baseline_chronic_knee_pain 

## If widespread chronic pain recorded, assign 8 chronic pain sites
data$baseline_chronic_pain[data$baseline_chronic_widespread_pain == 1] <- 8

### Baseline depression ----
## PHQ-2 score
data$baseline_depressed_mood <- recode(data$f.2050.0.0,
                                       'Not at all' = 0,
                                       'Several days' = 1,
                                       'More than half the days' = 2,
                                       'Nearly every day' = 3,
                                       'Do not know' = NaN,
                                       'Prefer not to answer' = NaN)

data$baseline_anhedonia <- recode(data$f.2060.0.0,
                                       'Not at all' = 0,
                                       'Several days' = 1,
                                       'More than half the days' = 2,
                                       'Nearly every day' = 3,
                                       'Do not know' = NaN,
                                       'Prefer not to answer' = NaN)

data$baseline_depression <- data$baseline_depressed_mood + data$baseline_anhedonia

## Keep list of White and Irish British ----
british_irish_keep <- na.omit(data$f.eid[data$f.21000.0.0 == "British" | data$f.21000.0.0 == "Irish"])

## Reduce dataframe ----

data <- data %>%
  rename(EOP_date = `f.120128.0.0`,
         touchscreen_date = `f.21822.0.0`) %>%
  select(f.eid, 
         followup_chronic_pain, followup_depression, comorbid_CPDep,
         PA_low, bad_sleep, too_much_sleep, too_little_sleep, lonely, smoking, high_alcohol_consumption, obese, unhealthy_diet, 
         age, sex, education, employment, deprivation, general_health,living_with_partner, baseline_chronic_pain, baseline_depression,
         EOP_date, touchscreen_date)
dim(data)

## Format dates prior to saving file
data$touchscreen_date <- as.Date(data$touchscreen_date)
data$EOP_date <- as.Date(data$EOP_date)

## Recode NaN as NA ----
data <- data %>% mutate_all(~ifelse(is.nan(.), NA, .))

## Save file ---- 
write.csv(data, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/data/UKB.csv", row.names = F)
write.csv(EOP_keep, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/EOP_keep.csv", row.names = F)
write.csv(alcohol_exclude, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/alcohol_exclude.csv", row.names = F)
write.csv(british_irish_keep, "/Volumes/GenScotDepression/users/hcasey/UKB_CP_MDD_lifestyle_CA/resources/british_irish_keep.csv", row.names = F)



