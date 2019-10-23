# CCCM Site Monitoring Tool - Data Cleaning script
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V3
# 22/10/2019

rm(list=ls())
today <- Sys.Date()

## Download necessary packages
# devtools::install_github("mabafaba/xlsformfill", force = T)
# devtools::install_github("mabafaba/cleaninginspectoR", force = T)


## Install packages
install.packages("tidyverse")
install.packages("data.table")
install.packages("openxlsx")

## Load libraries
require(tidyverse)
require(dataqualitycontrol)
require(cleaninginspectoR)
require(data.table)
require(openxlsx)
#require(xlsformfill)


## Source
source("./R/cleanHead.R")
source("./R/time_check.R")


## Download data from kobo server
# datasets <- kobo_datasets("reach_yemen:KOBOyemREACH2017", "kobohr")
# kobo_data_downloader("412367", "reach_yemen:KOBOyemREACH2017", "kobohr")

## Fake data just for testing
#choices <- read.csv("./data/choices.csv", stringsAsFactors = F, check.names = F)
#questions <- read.csv("./data/questions.csv", stringsAsFactors = F, check.names = F)
#questions$name <- tolower(questions$name)

# response <- xlsform_fill(questions, choices,500)
 
## Upload data to be cleaned
response <- read.csv("./data/CCCM_Site_Id_cluster_V8_07102019_2019_10_21_05_41_40_PILOT HIGHLIGHTED.csv", stringsAsFactors = F)
names(response)[names(response) == "X_index"] <- "index"
names(response)[names(response) == "X_uuid"] <- "uuid"

## Remove group name and reduce to all lowercase
names(response) <- tolower(names(response))
response <- cleanHead(response)
response <- cleanHead(response)


## Anonymize dataset
# response <- anonymise_dataset(response, c("start", "end", "deviceid", "imei", "q0_1_enumerator_name", "q0_2_gender", "q0_3_organization", "q0_3_organization_other", "q1_1_key_informant_name",
                                         # "q1_2_key_informat_gender", "q1_3_key_informat_mobile_number", "q5_2_gps_latitude", "a5_1_gps_longitude", "a5_2_gps_latitude", "__version__"))

## Check 0: check that there are no site surveyed twice
n_occur <- data.frame(table(response$a4_site_name))
n_occur[n_occur$Freq > 1,]

duplicate_sites <- response[response$a4_site_name %in% n_occur$Var1[n_occur$Freq > 1],]

#write.csv(duplicate_sites, paste0("./output/duplicated_sites_",today,".csv"), row.names = F)
#browseURL(paste0("./output/duplicated_sites_",today,".csv"))

## Check 1: run cleaninginspector
response_issue <- inspect_all(response, "uuid")

#write.csv(response_issue, paste0("./output/dataset_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/dataset_issues_",today,".csv"))

## Check 2: run extra cleaning analysis
### Check that the site name is in the correct location - TBD

## Check 3: Check adequacy
### Check that a service defined as a priority need is not classified as adequate
check_adequacy <- select(response, "uuid", "a4_site_name", c("rrm_distributions":"waste_disposal_services"), "i1_top_priority_need", "i2_second_priority_need", "i3_third_priority_need")

check_adequacy <- check_adequacy %>% mutate(food_check = ifelse((food_distribution == "Adequate" & (i1_top_priority_need == "Food" | i2_second_priority_need == "Food" | i3_third_priority_need == "Food")), 1,0))
check_adequacy <- check_adequacy %>% mutate(water_check = ifelse((wash_services == "Adequate" & (i1_top_priority_need == "Water" | i2_second_priority_need == "Water" | i3_third_priority_need == "Water")), 1,0))
check_adequacy <- check_adequacy %>% mutate(shelter_check = ifelse((shelter_maintenance_services == "Adequate" & (i1_top_priority_need == "Shelter_maintenance_assistance" | i2_second_priority_need == "Shelter_maintenance_assistance" | i3_third_priority_need == "Shelter_maintenance_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nfi_check = ifelse((nfi_distribution == "Adequate" & (i1_top_priority_need == "Non_food_items" | i2_second_priority_need == "Non_food_items" | i3_third_priority_need == "Non_food_items")), 1,0))
check_adequacy <- check_adequacy %>% mutate(sanitation_check = ifelse((waste_disposal_services == "Adequate" & (i1_top_priority_need == "Sanitation_services" | i2_second_priority_need == "Sanitation_services" | i3_third_priority_need == "Sanitation_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(nutrition_check = ifelse((nutrition_services == "Adequate" & (i1_top_priority_need == "Nutrition_services" | i2_second_priority_need == "Nutrition_services" | i3_third_priority_need == "Nutrition_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(health_check = ifelse((health_services == "Adequate" & (i1_top_priority_need == "Medical_assistance" | i2_second_priority_need == "Medical_assistance" | i3_third_priority_need == "Medical_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(livelihoods_check = ifelse((livelihood_services == "Adequate" & (i1_top_priority_need == "Livelihood_assistance" | i2_second_priority_need == "Livelihood_assistance" | i3_third_priority_need == "Livelihood_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(cash_check = ifelse((cash_distributions == "Adequate" & (i1_top_priority_need == "Cash_assistance" | i2_second_priority_need == "Cash_assistance" | i3_third_priority_need == "Cash_assistance")), 1,0))
check_adequacy <- check_adequacy %>% mutate(legal_check = ifelse((legal_services == "Adequate" & (i1_top_priority_need == "Legal_services" | i2_second_priority_need == "Legal_services" | i3_third_priority_need == "Legal_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(protection_check = ifelse((protection_services == "Adequate" & (i1_top_priority_need == "Protection_services" | i2_second_priority_need == "Protection_services" | i3_third_priority_need == "Protection_services")), 1,0))
check_adequacy <- check_adequacy %>% mutate(education_check = ifelse((education_services == "Adequate" & (i1_top_priority_need == "Education" | i2_second_priority_need == "Education" | i3_third_priority_need == "Education")), 1,0))
check_adequacy <- check_adequacy %>% mutate(wash_sanitation_check = ifelse((wash_services == "Adequate" & (i1_top_priority_need == "Sanitation_services" | i2_second_priority_need == "Sanitation_services" | i3_third_priority_need == "Sanitation_services")), 1,0))


#write.csv(check_adequacy, paste0("./output/adequacy_issues_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adequacy_issues_",today,".csv"))

### Check longitude and latitude
wrong_lat_long <- select(response, "uuid", "a4_site_name", "a5_1_gps_longitude", "a5_2_gps_latitude")
wrong_lat_long <- wrong_lat_long %>% mutate(wrong_longitude = ifelse((a5_1_gps_longitude > 54.73893746 | a5_1_gps_longitude < 41.60824994), 1,0))
wrong_lat_long <- wrong_lat_long %>% mutate(wrong_latitude = ifelse((a5_2_gps_latitude > 18.99999992 | a5_2_gps_latitude < 11.90848018), 1,0))

#write.csv(wrong_lat_long, paste("./output/wrong_lat_long_",today,".csv"), row.names = F)
#browseURL(paste("./output/wrong_lat_long_",today,".csv"))

### Check phone number - still needs to be fixed
phonenumber <- select(response, "uuid","a4_site_name", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")

phonenumber1 <- phonenumber %>% mutate(ki_wrong_number = ifelse(grep("^[70|71|73|77|79]", q1_3_key_informat_mobile_number), 0, 1))
phonenumber1[, c("a4_site_name", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")] <- NULL
                            
            
phonenumber2 <- phonenumber %>% filter(!is.na(b3_exu_fp_mobile_number)) %>% 
                                         mutate(exu_fb_wrong_number = ifelse(grep("^[70|71|73|77|79]", b3_exu_fp_mobile_number), 0, 1))

phonenumber2[, c("a4_site_name", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")] <- NULL

phonenumber3 <- phonenumber %>% filter(!is.na(b6_smc_agency_fp_mobile_number)) %>% 
                                         mutate(smc_agency_wrong_number = ifelse(grep("^[70|71|73|77|79]", b6_smc_agency_fp_mobile_number), 0, 1))

phonenumber3[, c("a4_site_name", "q1_3_key_informat_mobile_number", "b3_exu_fp_mobile_number", "b6_smc_agency_fp_mobile_number")] <- NULL


phonenumber_df <- plyr::join_all(list(phonenumber, phonenumber1, phonenumber2, phonenumber3), by = "x_uuid", type = "left")

### Check that formal sites have more than 20 HH
formal <- select(response, "uuid", "a4_site_name", "a7_site_population_hh", "a10_formal_informal")
formal <- formal %>% mutate(less_than_20 = ifelse((a10_formal_informal == "Formal" & a7_site_population_hh < 20), 1,0))

#write.csv(formal, paste0("./output/formal_site_population_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/formal_site_population_issue_",today,".csv"))

### Check that collective centre is not a makeshift or emergency or transitional or open-air type of shelter
collective <- select(response, "uuid", "a4_site_name", "c1_type_of_site", "c9_primary_shelter_type")
collective <- collective %>% mutate(collective_issue = ifelse((c1_type_of_site == "Collective_centre" & (c9_primary_shelter_type == "Emergency_shelter" |
                                                                                              c9_primary_shelter_type == "Makeshift_shelter" |
                                                                                              c9_primary_shelter_type == "Transitional_shelter" |
                                                                                              c9_primary_shelter_type == "Open_air_no_shelter")), 1, 0))

#write.csv(collective, paste0("./output/collective_centre_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/collective_centre_issue_",today,".csv"))

### Check that waste disposal cannot be "non-existant" if they selected flush latrines
waste_disposal <- select(response, "uuid", "a4_site_name", "c10_primary_latrine_type", "waste_disposal_services")
waste_disposal <- waste_disposal %>% mutate(waste_disposal_issue = ifelse((waste_disposal_services == "Non_existant" & c10_primary_latrine_type == "Flush_latrine_to_tank_sewage_system_pit"), 1, 0))

#write.csv(waste_disposal, paste0("./output/waste_disposal_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/waste_disposal_issue_",today,".csv"))

### Check that adequate was services do not include: flush latrine to the open / open defecation / pit uncovered / illegal water connection / unprotected well / surface water
adequate_wash <- select(response, "uuid", "a4_site_name", "c10_primary_latrine_type", "c8_primary_water_source", "wash_services")
adequate_wash <- adequate_wash %>% mutate(adequate_wash_issue = ifelse((wash_services == "Adequate" & (c10_primary_latrine_type == "Flush_latrine_to_the_open" |
                                                                                        c10_primary_latrine_type == "Open_air" |
                                                                                        c10_primary_latrine_type == "Pit_latrine_open" |
                                                                                        c8_primary_water_source == "Illegal_connection_to_piped_network" |
                                                                                        c8_primary_water_source == "Unprotected_well" |
                                                                                        c8_primary_water_source == "Surface_water")), 1, 0))

#write.csv(adequate_wash, paste0("./output/adeqate_facility_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/adeqate_facility_issue_",today,".csv"))

### Check that eviction risk does not come with a tennacy agreement
eviction <- select(response, "uuid", "a4_site_name", "c3_tenancy_agreement_for_at_least_6_months", "f1_threats_to_the_site.eviction")
eviction <- eviction %>% mutate(eviction_issue = ifelse((c3_tenancy_agreement_for_at_least_6_months == "Yes" & f1_threats_to_the_site.eviction == TRUE), 1, 0))

#write.csv(eviction, paste0("./output/eviction_issue_",today,".csv"), row.names = F)
#browseURL(paste0("./output/eviction_issue_",today,".csv"))

### Check lenght of the survey, 10 = minimum, 40 = maximum (can be changed)
time_stamp <- select(response, "uuid", "start", "end")
check_time <- time_check(time_stamp, 10, 40)


### Save everything in one file
data_cleaning <- list("duplicated sites" = duplicate_sites,
                      "Time stamp check" = check_time,
                      "Wrong phone numbers" = phonenumber_df,
                      "Outliers and others" = response_issue,
                      "Service adequacy vs needs" = check_adequacy,
                      "Longitude and latitude" = wrong_lat_long,
                      "Number of HH in formal site" = formal,
                      "Shelter type in collective shelter" = collective,
                      "Waste disposal" = waste_disposal,
                      "Adequate WASH facilities" = adequate_wash,
                      "Tennecy agreement and eviction" = eviction)

write.xlsx(data_cleaning, paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"), colNames = TRUE)
browseURL(paste0("./output/CCCM_SiteID_data cleaning checks_",today,".xlsx"))
