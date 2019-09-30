# cccm Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V2
# 25/10/2019

rm(list=ls())
today <- Sys.Date()

## Download necessary packages
# devtools::install_github("mabafaba/xlsformfill", force = T)
# devtools::install_github("mabafaba/cleaninginspectoR", force = T)
# devtools::install_github("agualtieri/dataqualitycontol", force = T)

## Load libraries
library(tidyverse)
library(xlsformfill)
library(dataqualitycontrol)
library(cleaninginspectoR)
library(koboloadeR)

## Download data from kobo server
# datasets <- kobo_datasets("reach_yemen:KOBOyemREACH2017", "kobohr")
# kobo_data_downloader("412367", "reach_yemen:KOBOyemREACH2017", "kobohr")

## Fake data just for testing
choices <- read.csv("./data/choices.csv", stringsAsFactors = F, check.names = F)
questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
questions$name <- tolower(questions$name)

response <- xlsform_fill(questions,
                         choices,
                         500)

## Anonymize dataset
response <- anonymise_dataset(response, c("start", "end", "deviceid", "imei", "q0_1_enumerator_name", "q0_2_gender", "q0_3_organization", "q0_3_organization_other", "q1_1_key_informant_name",
                                          "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "q5_2_gps_latitude", "a5_1_gps_longitude", "a5_2_gps_latitude", "__version__"))

## Check 1: run cleaninginspector
response_issue <- inspect_all(response, "uuid")
write.csv(response_issue, paste0("./output/dataset_issues_",today,".csv"))

## Check 3: run extra cleaning analysis
### Check that the site name is in the correct location - TBD


### Check that a service defined as a priority need is not classified as adequate
check_adequacy <- select(response, "uuid", c("rrm_distributions":"waste_disposal_services"), "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")

check_adequacy <- check_adequacy %>% mutate(food_check = ifelse((food_distribution == "adequate" & (i1_top_priority_need == "food" | i2_second_priority_need == "food" | i3_third_priority_need == "food")), 1,0))
check_adequacy <- check_adequacy %>% mutate(water_check = ifelse((wash_services == "adequate" & (i1_top_priority_need == "water" | i2_second_priority_need == "water" | i3_third_priority_need == "water")), 1,0))
check_adequacy <- check_adequacy %>% mutate(shelter_check = ifelse((shelter_maintenance_services == "adequate" & (i1_top_priority_need == "shelter_maintenance_assistance" | i2_second_priority_need == "shelter_maintenance_assistance" | i3_third_priority_need == "shelter_maintenance_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nfi_check = ifelse((nfi_distribution == "adequate" & (i1_top_priority_need == "non_food_items" | i2_second_priority_need == "non_food_items" | i3_third_priority_need == "non_food_items")), 1,0))
check_adequacy <- check_adequacy %>% mutate(sanitation_check = ifelse((waste_disposal_services == "adequate" & (i1_top_priority_need == "sanitation_services" | i2_second_priority_need == "sanitation_services" | i3_third_priority_need == "sanitation_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nutrition_check = ifelse((nutrition_services == "adequate" & (i1_top_priority_need == "nutrition_services" | i2_second_priority_need == "nutrition_services" | i3_third_priority_need == "nutrition_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(health_check = ifelse((health_services == "adequate" & (i1_top_priority_need == "medical_assistance" | i2_second_priority_need == "medical_assistance" | i3_third_priority_need == "medical_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(livelihoods_check = ifelse((livelihood_services == "adequate" & (i1_top_priority_need == "livelihood_assistance" | i2_second_priority_need == "livelihood_assistance" | i3_third_priority_need == "livelihood_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(cash_check = ifelse((cash_distributions == "adequate" & (i1_top_priority_need == "cash_assistance" | i2_second_priority_need == "cash_assistance" | i3_third_priority_need == "cash_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(legal_check = ifelse((legal_services == "adequate" & (i1_top_priority_need == "legal_services" | i2_second_priority_need == "legal_services" | i3_third_priority_need == "legal_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(protection_check = ifelse((protection_services == "adequate" & (i1_top_priority_need == "protection_services" | i2_second_priority_need == "protection_services" | i3_third_priority_need == "protection_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(education_check = ifelse((education_services == "adequate" & (i1_top_priority_need == "education" | i2_second_priority_need == "education" | i3_third_priority_need == "education")), 1,0))

write.csv(check_adequacy, paste0("./output/CCCM_SiteID_adequacy_issues_",today,".csv"), row.names = F)
browseURL(paste0("./output/CCCM_SiteID_adequacy_issues_",today,".csv"))
