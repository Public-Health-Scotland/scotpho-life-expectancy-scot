###############################################.
## ScotPHO - Life expectancy - Scotland ----
###############################################.

# Data queried directly from statistics.gov
# install the opendata scotland r package which communicates with the statistics.gov wesbite api
# install.packages("devtools")
# devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov
library(readr)        # to write csv
library(dplyr)        # to get %>% operator
library(tidyr)      # pivot wider
library(readxl)     # to open ONS data
# datasets <- ods_all_datasets() # to see available datasets on statistics.gov.scot

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

# HLE data saved in life expectancy network folder.
source_network <- "/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/HLE data with CI/"

# If you aren't analyst named in file path then consider UPDATING the filepath
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Vicky/scotpho-life-expectancy-scot_vicky/shiny_app/data/"

# UPDATE data file location each year if you want a record of what data was published historically
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Life expectancy/202507_update/"


# parameters used to filter the opendata
simd <- c("all")
urban_rural <- c("all")
age_select <- "0-years"


###############################################.
# Life expectancy data
###############################################.

ods_structure("Life-Expectancy") # see structure and variables of this dataset

# date range for LE
date_range_le <- c("2001-2003", "2002-2004", "2003-2005", "2004-2006", "2005-2007",
                   "2006-2008", "2007-2009", "2008-2010", "2009-2011", "2010-2012",
                   "2011-2013", "2012-2014", "2013-2015", "2014-2016", "2015-2017", 
                   "2016-2018", "2017-2019", "2018-2020", "2019-2021", "2020-2022", "2021-2023", "2022-2024") # add most recent year

# extract data
le = ods_dataset("Life-Expectancy", refPeriod = date_range_le, geography = "sc",
                 urbanRuralClassification = urban_rural,
                 simdQuintiles = simd) %>%
  setNames(tolower(names(.))) %>%
  rename("year" = refperiod) %>% 
  filter(age == age_select) %>% 
  mutate(measure = "Life expectancy",
         sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female")) |>
  select(c("year", "measure", "sex", "measuretype", "value"))|>
  pivot_wider(names_from="measuretype" ,values_from="value") |>
  rename(value = count,
         lci = "95-lower-confidence-limit",
         uci = "95-upper-confidence-limit") |>
  arrange(sex, year)

#stats.gov open data platform now updated but leaving the chunk below commented out
#in case future updates are delayed again and we need to source some years data from flat files.

# 2020-2022 data released as provisional figures not available within stats.gov.scot
# sourced provisional figures from NRS website and manually formatted to allow December 2023 scotpho website update
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/life-expectancy-in-scotland/life-expectancy-in-scotland-2020-2022
# excel data from fig 5 and fig 6 saved to PHS network folder

# library(openxlsx)
# # open le data 
# le_2020to2022_scot <- read.xlsx("/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/2020 to 2022 provisional life expectancy from NRS website.xlsx", sheet = 1) %>%
#   filter(areaname=="Scotland") %>%
#   select(year,measure,sex,le) %>%
#   rename(value=le)
# 
# # combine stats.gov data with t
# le <- rbind(le, le_2020to2022_scot) %>% arrange(measure, sex, year)

###############################################.
# Healthy life expectancy data
###############################################.

# Open data downloaded from NRS website latest HLE publication pages
# https://www.nrscotland.gov.uk/publications/healthy-life-expectancy-2021-2023/
# (data read in  comes from ONS pivot table manipulated to include time series hle at birth data for all geographies and both sexes) 

ons_data <- read_excel((paste0(source_network,"ons-data-tables (from NRS HLE Publication July 2025).xlsx")), sheet = "pivot_extract") %>%
  setNames(tolower(names(.))) |>
  rename(value=hle,
         areacode = 'area code',
         areaname = 'area name') |>
  mutate(year =paste0(substr(period,1,4),"-",substr(period,9,12)),
         measure = "Healthy life expectancy")|>
  select(areacode, areaname, year, measure, sex, value, uci,lci )


# save scotland level data 
scot_hle <-ons_data |>
  filter(areacode=="S92000003") |>
  select(-areaname, -areacode)

# save NHS board data for board shiny app
# (see repo https://github.com/Public-Health-Scotland/scotpho-life-expectancy-hb)
hb_hle <-ons_data |>
  filter(substr(areacode,1,3)=="S08")
write_csv(hb_hle, paste0(data_folder, "/hle_nhsboard.csv"))

# save NHS board data for council shiny app
#(see repo https://github.com/Public-Health-Scotland/scotpho-life-expectancy-ca )
ca_hle <-ons_data |>
  filter(substr(areacode,1,3)=="S12")
write_csv(ca_hle, paste0(data_folder, "/hle_ca.csv"))



# PRE-July 2025 calculation methodology change HLE data could be sourced from statistics.gov
# Leaving the syntax for data extraction but commented out in case the new data is made available in opendata tool in future
# or in case there is a need to source historic data.

# ods_structure("healthy-life-expectancy") # see structure and variables of this dataset
# 
# # date range for HLE
# #date_range_hle <- c("2014-2016", "2015-2017", "2016-2018", "2017-2019", "2018-2020","2019-2021") # add most recent year
# date_range_hle <- c("2015-2017", "2016-2018", "2017-2019", "2018-2020","2019-2021") # add most recent year
# 
# # extract data
# hle = ods_dataset("healthy-life-expectancy", refPeriod = date_range_hle, geography = "sc",
#                   urbanRuralClassification = urban_rural,
#                   simdQuintiles = simd) %>%
#   setNames(tolower(names(.))) %>%
#   rename("year" = refperiod) %>% 
#   filter(age == age_select) %>% 
#   mutate(measure = "Healthy life expectancy",
#          sex = case_when(sex == "male" ~ "Male",
#                          sex == "female" ~ "Female")) %>% 
#   select(c("year", "measure", "measuretype", "sex", "value" )) |>
#   pivot_wider(names_from="measuretype" ,values_from="value") |>
#   rename(value = count,
#          lci = "95-lower-confidence-limit",
#          uci = "95-upper-confidence-limit") |>
#   arrange(sex, year)


###############################################.
# Combined LE and HLE data
###############################################.


# combine datasets
le_hle <- rbind(le, scot_hle) %>% arrange(measure, sex, year) %>%
  mutate(value=round(value,2))

# save as csv
write_csv(le_hle, paste0(data_folder, "le_hle_scot.csv"))

# Save data to shiny_app folder
saveRDS(le_hle, file = paste0(shiny_folder,"le_hle_scot.rds"))

# END
