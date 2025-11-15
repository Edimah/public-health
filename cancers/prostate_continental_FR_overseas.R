library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)

# Importing data from CSV file (source: Global Cancer Observatory, 2022)

path <- here::here("public-health", "data", "dataset-inc-males-in-2022-prostate.csv")

prostate_data <- read_csv(path, show_col_types = FALSE)
