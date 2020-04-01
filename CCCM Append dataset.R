### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V1
# 08/01/2020

## Useful resource
# https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in

rm(list=ls())
today <- Sys.Date()

require(tidyverse)
require(openxlsx)

### Load kobo choices file to vlook up old site codes
choices <- read.csv("./data/kobo/choices.csv", check.names = F)
external_choices <- read.csv("./data/kobo/external_choices.csv", check.names = F)

external_choices <- filter(external_choices, external_choices$list_name == "sitename")
names(external_choices)[names(external_choices) == "name"] <- "a4_site_code"
names(external_choices)[names(external_choices) == "label::english"] <- "a4_site_name"

### Produce Internal Updated dataset ###
#### Internal Master file
master_all_int <- read.xlsx("./output/internal/CCCM_SiteReporting_All Internal_2020-01-30_old.xlsx")

### Load Last Cleaned files ###
## Internal
last_internal_v1 <- read.xlsx("./output/internal/CCCM_SiteReporting_V1 Internal_2020-01-30.xlsx")
last_internal_v2 <- read.xlsx("./output/internal/CCCM_SiteReporting_V2 Internal_2020-02-06.xlsx")

## If only TRUE it means that no new values have been cleaned
unique(last_internal_v1$uuid %in% master_all_int$uuid)
unique(last_internal_v2$uuid %in% master_all_int$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_internal_v1, master_all_int, "uuid")
new_v2 <- anti_join(last_internal_v2, master_all_int, "uuid")

new_int <- plyr::rbind.fill(new_v1, new_v2)

## Append the unique new entries to the final Master ALL Internal
new_master_all_int <- plyr::rbind.fill(master_all_int, new_int)
write.xlsx(new_master_all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal_",today,".xlsx"))

### Internal Site ID code
all_int$a4_site_name <- str_trim(all_int$a4_site_name, "both")
all_int$a4_site_code <- external_choices$a4_site_code[match(all_int$a4_site_name, external_choices$a4_site_name)]
all_int <- all_int[order(all_int$a4_site_code, all_int$a4_site_name, na.last = FALSE),]

all_int <- all_int %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185, 5000)), paste0(a4_site_code)))
all_int$a4_site_code

write.xlsx(all_int, paste0("./output/internal/CCCM_SiteReporting_All Internal (WithID)_",today,".xlsx"))

########################################################

### Produce External Updated dataset ###
master_all_ext <- read.xlsx("./output/external/CCCM_SiteReporting_All External_2020-02-18.xlsx")

### Load Last Cleaned files ###
## External
last_external_v1 <- read.xlsx("./output/external/CCCM_SiteReporting_V1 External_2020-01-30.xlsx")
last_external_v2 <- read.xlsx("./output/external/CCCM_SiteReporting_V2 External_2020-02-06.xlsx")

## Create V1 and V2 variable
#last_external_v1$kobo_version <- "V1"
#last_external_v2$kobo_version <- "V2"

unique(last_external_v1$uuid %in% master_all_ext$uuid)
unique(last_external_v2$uuid %in% master_all_ext$uuid)

## Take latest dataset and remove the duplicated entries using the master
new_v1 <- anti_join(last_external_v1, master_all_ext, "uuid")
new_v2 <- anti_join(last_external_v2, master_all_ext, "uuid")

new_ext <- plyr::rbind.fill(new_v1, new_v2)

## Append the unique new entries to the final Master ALL Internal
new_master_all_ext <- plyr::rbind.fill(master_all_ext, new_ext)
write.xlsx(new_master_all_ext, paste0("./output/external/CCCM_SiteReporting_All External_",today,".xlsx"))

### Load the newly created ALL internal and external and crate new site id codes
all_int <- read.xlsx("./output/internal/CCCM_SiteReporting_All Internal_2020-02-18.xlsx")
all_ext <- read.xlsx("./output/external/CCCM_SiteReporting_All External_2020-02-19.xlsx")

### External Site ID code
all_ext$a4_site_name <- str_trim(all_ext$a4_site_name, "both")
all_ext$a4_site_code <- external_choices$a4_site_code[match(all_ext$a4_site_name, external_choices$a4_site_name)]
all_ext <- all_ext[order(all_ext$a4_site_code, all_ext$a4_site_name, na.last = FALSE),]

all_ext <- all_ext %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
all_ext$a4_site_code

write.xlsx(all_ext, paste0("./output/external/CCCM_SiteReporting_All External (WithID)_",today,".xlsx"))

########################################################

### Produce Dashboard Updated dataset ###
master_db <- read.xlsx("./output/dashboard/CCCM_SiteReporting_All DB_2020-02-20.xlsx")

### Load Last Cleaned files ###
## Dashboard
last_db_V1 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V1_2020-01-30.xlsx")
last_db_V2 <- read.xlsx("./output/dashboard/CCCM_Site Reporting_V2_2020-02-06.xlsx")

## Rename columns so that they match and delete extra ones
names(last_db_V1)[names(last_db_V1) == "b4_site_smc_agency_name"] <- "b2_site_smc_agency_name"
names(last_db_V1)[names(last_db_V1) == "b5_smc_agency_fp_name"] <- "b3_smc_agency_fp_name"
names(last_db_V1)[names(last_db_V1) == "b6_smc_agency_fp_mobile_number"] <- "b4_smc_agency_fp_mobile_number"
names(last_db_V1)[names(last_db_V1) == "b7_community_committee_in_place"] <- "b5_community_committee_in_place"
names(last_db_V1)[names(last_db_V1) == "b8_community_committee_fp_name"] <- "b6_community_committee_fp_name"
names(last_db_V1)[names(last_db_V1) == "b9_community_committee_fp_cell"] <- "b7_community_committee_fp_cell"

drops <- c("b4_site_smc_agency_name", "b5_smc_agency_fp_name", "b6_smc_agency_fp_mobile_number", "b7_community_committee_in_place", 
           "b8_community_committee_fp_name", "b9_community_committee_fp_cell")

last_db_V2 <- last_db_V2[ , !(names(last_db_V2) %in% drops)]
new_db <- rbind(last_db_V1, last_db_V2)

## Delete unnecessary column and add new one
new_db$X__version__ <- NULL
new_db$X_validation_status <- NULL
new_db$comments <- ""

write.xlsx(new_db, paste0("./output/dashboard/CCCM_SiteReporting_All DB_",today,".xlsx"))

## Create the DB site code
all_db <- read.xlsx("./output/dashboard/CCCM_SiteReporting_All DB_2020-02-20.xlsx")

all_db$a4_site_name <- str_trim(all_db$a4_site_name, "both")
all_db$a4_site_code <- external_choices$a4_site_code[match(all_db$a4_site_name, external_choices$a4_site_name)]
all_db <- all_db[order(all_db$a4_site_code, all_db$a4_site_name, na.last = FALSE),]

all_db <- all_db %>% mutate(a4_site_code = ifelse(is.na(a4_site_code) == TRUE, paste0(a2_district_code,"_",seq(1185,5000)), paste0(a4_site_code)))
all_db$a4_site_code <- str_replace(all_db$a4_site_code, "_", "-")

all_db$a4_site_code

write.xlsx(all_db, paste0("./output/dashboard/CCCM_SiteReporting_All DB (WithID)_",today,".xlsx"))
