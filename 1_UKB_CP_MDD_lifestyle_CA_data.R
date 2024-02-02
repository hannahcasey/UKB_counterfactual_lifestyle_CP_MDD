library(dplyr)
library(lubridate)


## Get data ----

touchscreen <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/Touchscreen.rds")
## Qualification (6138)
## Employment status (6142)
## General health (2178)
## Loneliness (2020)
## Alcohol consumption (4407, 4418, 4429, 4440, 4451, 4462, 1568, 1578, 1588, 1598, 1608, 5364)
## Smoking (1239)
## Sleep (1160)
## Diet (1309, 1289, 1299, 1329, 1339, 1349, 1369, 1379)
## Baseline chronic pain sites
  ## Widespread (2956)
  ## Head (3799)
  ## Face (4067)
  ## Neck/shoulder (3404)
  ## Back (3571)
  ## Stomach/Abdominal (3741)
  ## Hip (3414)
  ## Knee (3773)
## Pysical Activity (to calculate MET score)
  ## Duration of walks (874)
  ## Number of days/week of moderate physical activity 10+ minutes (864)
  ## Duration of moderate activity (894)
  ## Number of days/week of moderate physical activity 10+ minutes (884)
  ## Duration of vigorous activity (914)
  ## Number of days/week of vigorous physical activity 10+ minutes (904)
## Date touchscreen completed (21822)


touchscreen_sm <- touchscreen[ ,grepl("f.eid", names(touchscreen)) |
                                 grepl("f.6138.", names(touchscreen))|
                                 grepl("f.6142.", names(touchscreen))|
                                 grepl("f.2178.", names(touchscreen))|
                                 grepl("f.2020.", names(touchscreen))|
                                 grepl("f.4407.", names(touchscreen))|
                                 grepl("f.4418.", names(touchscreen))|
                                 grepl("f.4429.", names(touchscreen))|
                                 grepl("f.4440.", names(touchscreen))|
                                 grepl("f.4451.", names(touchscreen))|
                                 grepl("f.4462.", names(touchscreen))|
                                 grepl("f.1568.", names(touchscreen))|
                                 grepl("f.1578.", names(touchscreen))|
                                 grepl("f.1588.", names(touchscreen))|
                                 grepl("f.1598.", names(touchscreen))|
                                 grepl("f.1608.", names(touchscreen))|
                                 grepl("f.5364.", names(touchscreen))|
                                 grepl("f.1239.", names(touchscreen))|
                                 grepl("f.1160.", names(touchscreen))|
                                 grepl("f.1309.", names(touchscreen))|
                                 grepl("f.1289.", names(touchscreen))|
                                 grepl("f.1299.", names(touchscreen))|
                                 grepl("f.1329.", names(touchscreen))|
                                 grepl("f.1339.", names(touchscreen))|
                                 grepl("f.1349.", names(touchscreen))|
                                 grepl("f.1369.", names(touchscreen))|
                                 grepl("f.1379.", names(touchscreen))|
                                 grepl("f.2956.", names(touchscreen))|
                                 grepl("f.3799.", names(touchscreen))|
                                 grepl("f.4067.", names(touchscreen))|
                                 grepl("f.3404.", names(touchscreen))|
                                 grepl("f.3571.", names(touchscreen))|
                                 grepl("f.3741.", names(touchscreen))|
                                 grepl("f.3414.", names(touchscreen))|
                                 grepl("f.3773.", names(touchscreen))|
                                 grepl("f.874.", names(touchscreen))|
                                 grepl("f.864.", names(touchscreen))|
                                 grepl("f.894.", names(touchscreen))|
                                 grepl("f.884.", names(touchscreen))|
                                 grepl("f.914.", names(touchscreen))|
                                 grepl("f.904.", names(touchscreen))]


procedural_metrics <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2020-09-phenotypes-ukb43743/ProceduralMetrics.rds")

procedural_metrics_sm <- procedural_metrics[ ,grepl("f.eid", names(procedural_metrics)) |
                                                           grepl("f.21822.", names(procedural_metrics))]

baseline_characteristics <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/BaselineCharacteristics.rds")
## Sex (31)
## Deprivation (189)

baseline_characteristics_sm <- baseline_characteristics[ ,grepl("f.eid", names(baseline_characteristics)) |
                                                           grepl("f.31.", names(baseline_characteristics)) |
                                                           grepl("f.189.", names(baseline_characteristics))]
  
recruitment <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/Recruitment.rds")
## Age (21003)

recruitment_sm <- recruitment[ ,grepl("f.eid", names(recruitment)) |
                                 grepl("f.21003.", names(recruitment))]

physical_measures <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/PhysicalMeasures.rds")
## BMI (21001)

physical_measures_sm <- physical_measures[ ,grepl("f.eid", names(physical_measures)) |
                                             grepl("f.21001.", names(physical_measures))]

MHQ <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-phenotypes-ukb44797/MentalHealth.rds")
## Speech (20518)
## Depression (20510)
## Inadequacy (20507)
## Tired (20519)
## Anhedonia (20514)
## Appetite (20511)
## Suicide (20513)
## Concentration (20508)
## Sleep (20517)
## Date MHQ completed (20400)

MHQ_sm <- MHQ[ , grepl("f.eid", names(MHQ)) |
                 grepl("f.20518.", names(MHQ)) |
                 grepl("f.20510.", names(MHQ)) |
                 grepl("f.20507.", names(MHQ)) |
                 grepl("f.20519.", names(MHQ)) |
                 grepl("f.20514.", names(MHQ)) |
                 grepl("f.20511.", names(MHQ)) |
                 grepl("f.20508.", names(MHQ)) |
                 grepl("f.20518.", names(MHQ)) |
                 grepl("f.20517.", names(MHQ)) |
                 grepl("f.20400.", names(MHQ))]


EOP <- readRDS("/Volumes/GenScotDepression/data/ukb/phenotypes/fields/2021-04-chronicpain-ukb45596/ExperienceOfPain.rds")
## PHQ-9 (120104-120112)
## Chronic pain sites
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
                 grepl("f.120106.", names(EOP)) |
                 grepl("f.120107.", names(EOP)) |
                 grepl("f.120108.", names(EOP)) |
                 grepl("f.120109.", names(EOP)) |
                 grepl("f.120110.", names(EOP)) |
                 grepl("f.120111.", names(EOP)) |
                 grepl("f.120112.", names(EOP)) |
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
  full_join(MHQ_sm, by='f.eid') %>%
  full_join(EOP_sm, by='f.eid')


## Determine which instance to use for touchscreen questionnaire (must be within 30 days of MHQ) ----

## Calculate number of days between MHQ and touchscreen questionnaire
data$MHQ_timing_difference_0 <- as.Date(data$f.120128.0.0) - as.Date(data$f.21822.0.0)
data$MHQ_timing_difference_1 <- as.Date(data$f.120128.0.0) - as.Date(data$f.21822.1.0)
data$MHQ_timing_difference_2 <- as.Date(data$f.120128.0.0) - as.Date(data$f.21822.2.0)

## Overlap only present 
hist(data$MHQ_timing_difference_2[abs(data$MHQ_timing_difference_2) < 32])

sum((abs(data$MHQ_timing_difference_2) < 32), na.rm = T)


## Calculate MET ---- 
## International Physical Activity Questionnaire short form:
  ## Physical activity guidelines of 150 minutes of walking or moderate activity per week or 75 minutes of vigorous activity, or combination

## Where no days spent doing activity, indicate time spent as 0
touchscreen_sm$f.874.0.0[touchscreen_sm$f.864.0.0 == 0] <- 0
touchscreen_sm$f.894.0.0[touchscreen_sm$f.884.0.0 == 0] <- 0
touchscreen_sm$f.914.0.0[touchscreen_sm$f.904.0.0 == 0] <- 0
touchscreen_sm$f.874.1.0[touchscreen_sm$f.864.1.0 == 0] <- 0
touchscreen_sm$f.894.1.0[touchscreen_sm$f.884.1.0 == 0] <- 0
touchscreen_sm$f.914.1.0[touchscreen_sm$f.904.1.0 == 0] <- 0
touchscreen_sm$f.874.2.0[touchscreen_sm$f.864.2.0 == 0] <- 0
touchscreen_sm$f.894.2.0[touchscreen_sm$f.884.2.0 == 0] <- 0
touchscreen_sm$f.914.2.0[touchscreen_sm$f.904.2.0 == 0] <- 0

## Replace -1 and -3 with NA
touchscreen_sm$f.874.0.0[touchscreen_sm$f.874.0.0 == -1 | touchscreen_sm$f.874.0.0 == -3] <- NA
touchscreen_sm$f.894.0.0[touchscreen_sm$f.894.0.0 == -1 | touchscreen_sm$f.894.0.0 == -3] <- NA
touchscreen_sm$f.914.0.0[touchscreen_sm$f.914.0.0 == -1 | touchscreen_sm$f.914.0.0 == -3] <- NA
touchscreen_sm$f.874.1.0[touchscreen_sm$f.874.1.0 == -1 | touchscreen_sm$f.874.1.0 == -3] <- NA
touchscreen_sm$f.894.1.0[touchscreen_sm$f.894.1.0 == -1 | touchscreen_sm$f.894.1.0 == -3] <- NA
touchscreen_sm$f.914.1.0[touchscreen_sm$f.914.1.0 == -1 | touchscreen_sm$f.914.1.0 == -3] <- NA
touchscreen_sm$f.874.2.0[touchscreen_sm$f.874.2.0 == -1 | touchscreen_sm$f.874.2.0 == -3] <- NA
touchscreen_sm$f.894.2.0[touchscreen_sm$f.894.2.0 == -1 | touchscreen_sm$f.894.2.0 == -3] <- NA
touchscreen_sm$f.914.2.0[touchscreen_sm$f.914.2.0 == -1 | touchscreen_sm$f.914.2.0 == -3] <- NA

## Indicate if participants get recommended PA from moderate and vigorous exercise
touchscreen_sm$PA_low_0.0 <- NA
touchscreen_sm$PA_low_0.0[(touchscreen_sm$f.894.0.0 + (touchscreen_sm$f.914.0.0 * 2) * 7) > 150] <- 0
touchscreen_sm$PA_low_0.0[(touchscreen_sm$f.894.0.0 + (touchscreen_sm$f.914.0.0 * 2) * 7) < 150] <- 1
touchscreen_sm$PA_low_1.0 <- NA
touchscreen_sm$PA_low_1.0[(touchscreen_sm$f.894.1.0 + (touchscreen_sm$f.914.1.0 * 2) * 7) > 150] <- 0
touchscreen_sm$PA_low_1.0[(touchscreen_sm$f.894.1.0 + (touchscreen_sm$f.914.1.0 * 2) * 7) < 150] <- 1
touchscreen_sm$PA_low_2.0 <- NA
touchscreen_sm$PA_low_2.0[(touchscreen_sm$f.894.2.0 + (touchscreen_sm$f.914.2.0 * 2) * 7) > 150] <- 0
touchscreen_sm$PA_low_2.0[(touchscreen_sm$f.894.2.0 + (touchscreen_sm$f.914.2.0 * 2) * 7) < 150] <- 1



