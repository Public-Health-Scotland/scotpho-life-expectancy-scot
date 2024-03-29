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
# datasets <- ods_all_datasets() # to see available datasets on statistics.gov.scot

# Setting file permissions to anyone to allow writing/overwriting of project files
Sys.umask("006")

# UPDATE the analyst's folder - where data should be saved for shiny app to run
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Vicky/scotpho-life-expectancy-scot/shiny_app/data/"

# UPDATE data file location
data_folder <- "/PHI_conf/ScotPHO/Website/Topics/Life expectancy/202303_update/"


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
                   "2016-2018", "2017-2019", "2018-2020", "2019-2021") # add most recent year

# extract data
le = ods_dataset("Life-Expectancy", refPeriod = date_range_le, geography = "sc",
                 urbanRuralClassification = urban_rural,
                 simdQuintiles = simd, measureType = "count") %>%
  setNames(tolower(names(.))) %>%
  rename("year" = refperiod) %>% 
  filter(age == age_select) %>% 
  mutate(measure = "Life expectancy",
         sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female")) %>% 
  select(c("year", "measure", "sex", "value")) %>% 
  arrange(sex, year)

# 2020-2022 data released as provisional figures not available within stats.gov.scot
# sourced provisional figures from NRS website and manually formatted to allow December 2023 scotpho website update
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/life-expectancy/life-expectancy-in-scotland/life-expectancy-in-scotland-2020-2022
# excel data from fig 5 and fig 6 saved to PHS network folder

library(openxlsx)
# open le data 
le_2020to2022_scot <- read.xlsx("/PHI_conf/ScotPHO/Life Expectancy/Data/Source Data/NRS data/2020 to 2022 provisional life expectancy from NRS website.xlsx", sheet = 1) %>%
  filter(areaname=="Scotland") %>%
  select(year,measure,sex,le) %>%
  rename(value=le)

# combine stats.gov data with t
le <- rbind(le, le_2020to2022_scot) %>% arrange(measure, sex, year)

###############################################.
# Healthy life expectancy data
###############################################.

ods_structure("healthy-life-expectancy") # see structure and variables of this dataset

# date range for HLE
#date_range_hle <- c("2014-2016", "2015-2017", "2016-2018", "2017-2019", "2018-2020","2019-2021") # add most recent year
date_range_hle <- c("2015-2017", "2016-2018", "2017-2019", "2018-2020","2019-2021") # add most recent year

# extract data
hle = ods_dataset("healthy-life-expectancy", refPeriod = date_range_hle, geography = "sc",
                  urbanRuralClassification = urban_rural,
                  simdQuintiles = simd, measureType = "count") %>%
  setNames(tolower(names(.))) %>%
  rename("year" = refperiod) %>% 
  filter(age == age_select) %>% 
  mutate(measure = "Healthy life expectancy",
         sex = case_when(sex == "male" ~ "Male",
                         sex == "female" ~ "Female")) %>% 
  select(c("year", "measure", "sex", "value" )) %>% 
  arrange(year, sex)


###############################################.
# Combined LE and HLE data
###############################################.

# combine datasets
le_hle <- rbind(le, hle) %>% arrange(measure, sex, year) %>%
  mutate(value=round(value,2))

# save as csv
write_csv(le_hle, paste0(data_folder, "le_hle_scot.csv"))

# Save data to shiny_app folder
saveRDS(le_hle, file = paste0(shiny_folder,"le_hle_scot.rds"))

# temp remove annual change stats until we can convert to weeks and get agreement with NRS figures
# # create the annual change measure
# change <- le_hle %>% 
#   group_by(measure, sex) %>% 
#   mutate(diff = (value - lag(value))*52.14, #should be in weeks not years
#          measure = case_when(measure == "Life expectancy" ~ 
#                                "Annual change in life expectancy",
#                              measure == "Healthy life expectancy" ~ 
#                                "Annual change in healthy life expectancy")) %>% 
#   ungroup %>% 
#   select(-value) %>% 
#   rename("value" = diff) %>% 
#   filter(!is.na(value)) # remove NA from the annual change calculation
# 
# # join datasets together
# final <- rbind(le_hle, change)
# 
# # round measure to 1 decimal place
# final <- final %>%
#   mutate(value=round(value,2))
# 
# # save as csv
# write_csv(final, paste0(data_folder, "le_hle_scot.csv"))
# 
# # Save data to shiny_app folder
# saveRDS(final, file = paste0(shiny_folder,"le_hle_scot.rds"))

# END
