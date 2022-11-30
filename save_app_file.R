###############################################.
## ScotPHO - Life expectancy - Scotland ----
###############################################.

# Firstly, update the data in the 'HLE ScotPHO Web Section' folder (see below).
# The details of where the data is published can be found in the README file in the GitHub repo
# Then run this code to save the updated data file into analyst's folder for running the shiny app

library(readr)

# UPDATE the analyst's folder - where data should be saved for shiny app to run
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Catherine/scotpho-life-expectancy-scot/shiny_app/data/"

# updated data file location
data_folder <- "/PHI_conf/ScotPHO/Life Expectancy/HLE ScotPHO Web Section/Scotland/"

# read updated data file
le_hle <- read_csv(paste0(data_folder, "le_hle_scotland.csv"))

# Save data to shiny_app folder
saveRDS(le_hle, file = paste0(shiny_folder,"le_hle_scotland.rds"))

#END

###############################################.
## ScotPHO - Life expectancy - Scotland ----
###############################################.

# Firstly, update the data in the 'HLE ScotPHO Web Section' folder (see below) - the
# data is extracted from the SG opendata platform.
# Then run this code to save the updated data file into analyst's folder for running the shiny app

# data queried directly from statistics.gov
# install the opendata scotland r package which communicates with the statistics.gov wesbite api
# install.packages("devtools")

devtools::install_github("datasciencescotland/opendatascot")

library(opendatascot) # to extract from statistics.gov
library(readr)        # to write csv
library(dplyr)        # to get %>% operator
# datasets <- ods_all_datasets() # to see available datasets on statistic.gov.scot

# UPDATE the analyst's folder - where data should be saved for shiny app to run
shiny_folder <- "/PHI_conf/ScotPHO/1.Analysts_space/Catherine/scotpho-life-expectancy-scot/shiny_app/data/"

# updated data file location
data_folder <- "/PHI_conf/ScotPHO/Life Expectancy/HLE ScotPHO Web Section/Scotland/"



# parameters used to filter the opendata
simd <- c("all")
urban_rural <- c("all")
age_select <- "0-years"


###############################################.
# Healthy life expectancy data
###############################################.

ods_structure("healthy-life-expectancy") # see structure and variables of this dataset

# date range for HLE
date_range_hle <- c("2014-2016", "2015-2017", "2016-2018", "2017-2019", "2018-2020") # add most recent year

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



# combine datasets
le_hle <- rbind(le, hle) %>% arrange(measure, sex, year)

# create the annual change measure
change <- le_hle %>% 
  group_by(measure, sex) %>% 
  mutate(diff = value - lag(value),
         measure = case_when(measure == "Life expectancy" ~ 
                               "Annual change in life expectancy",
                             measure == "Healthy life expectancy" ~ 
                               "Annual change in healhy life expectancy")) %>% 
  ungroup %>% 
  select(-value) %>% 
  rename("value" = diff)

# join datasets together
final <- rbind(le_hle, change)


# save as csv
write_csv(final, paste0(data_folder, "le_hle_scot_new.csv"))

# Save data to shiny_app folder
saveRDS(final, file = paste0(shiny_folder,"le_hle_scot.rds"))

# END
