### Script that takes V1 and V2 files and appends them to Masters
# REACH Yemen - alberto.gualtieri@reach-initiative.org
# V1
# 08/01/2020

rm(list=ls())
today <- Sys.Date()

require(tidyverse)
require(openxlsx)

### Load Masters

#### Dashboard
master_db <- read.xlsx("./output/dashboard/CCCM_Site Reporting_December ALL_2019-12-19.xlsx")

#### Internal
masterV1_int <- read.xlsx("./output/internal/CCCM_SiteReporting_December V1 Internal_2019-12-19.xlsx")
masterv2_int <- read.xlsx("./output/internal/CCCM_SiteReporting_December V2 Internal_2019-12-19.xlsx")

#### External
masterV1_ext <- read.xlsx("./output/external/CCCM_SiteReporting_December V1 External_2019-12-19.xlsx")
masterV2_ext <- read.xlsx("./output/external/CCCM_SiteReporting_December V2 External_2019-12-19.xlsx")

## Load Last Cleaned

#### Dashboard

#### Internal

#### External


