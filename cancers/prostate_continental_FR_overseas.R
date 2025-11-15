library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)

# Importing data from CSV file (source: Global Cancer Observatory, 2022)

path <- here::here("public-health", "data", "dataset-inc-males-in-2022-prostate.csv")
prostate_incidence_2022 <- read_csv(path, show_col_types = FALSE)

# Preview the data
head(prostate_incidence_2022)
# The dataset contains columns such as:
print(paste0("Columns: ", paste(colnames(prostate_incidence_2022), collapse = ", ")))

# Selecting all french regions including overseas territories
french_incidence <- prostate_incidence_2022 %>%
    filter(str_detect(Label, regex("(france|french)", ignore_case = TRUE)))

# Preview the filtered data
glimpse(french_incidence)

# Save the filtered data for further analysis
saveRDS(french_incidence, here::here("public-health", "data", "french_prostate_incidence_2022.rds"))
write_csv(french_incidence, here::here("public-health", "data", "french_prostate_incidence_2022.csv"))
