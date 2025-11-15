library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)

###############################
# DATA IMPORT AND PREPARATION #
###############################

# Importing data from CSV file (source: Global Cancer Observatory, 2022)

path <- here::here("public-health", "data", "dataset-inc-males-in-2022-prostate.csv")
prostate_incidence_2022 <- read_csv(path, show_col_types = FALSE)

# Preview the data
head(prostate_incidence_2022)
print(paste0("Columns: ", paste(colnames(prostate_incidence_2022), collapse = ", ")))


french_incidence <- prostate_incidence_2022 %>%
    # Isolate french incidence data
    filter(str_detect(Label, regex("(france|french)", ignore_case = TRUE))) %>%
    # Remove unnecessary columns
    select(-`Alpha-3 code`, -`Cancer code`, -`Population code (ISO/UN)`, -Country, -Sex) %>%
    # Remove duplicate column & rename leftover column for clarity
    rename(cases = Number...7) %>%
    select(-`Number...10`)


# Preview the filtered data
glimpse(french_incidence)

# Save the filtered data for further analysis
saveRDS(french_incidence, here::here("public-health", "data", "french_prostate_incidence_2022.rds"))
write_csv(french_incidence, here::here("public-health", "data", "french_prostate_incidence_2022.csv"))


############################################################
# PLOTTING CONTINENTAL AND OVERSEAS FRENCH INCIDENCE RATES #
############################################################
