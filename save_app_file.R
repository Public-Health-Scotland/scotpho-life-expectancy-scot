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