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

comparison <- french_incidence %>%
    mutate(region_type = if_else(Label == "France (metropolitan)", "Metropolitan France", "Overseas territories")) %>%
    mutate(Label = forcats::fct_reorder(Label, `ASR (World)`)) %>%
    mutate(label_en = str_remove(Label, "^France,\\s*")) # drop leading “France, ”

# English version
p_asr_en <- ggplot(
    comparison,
    aes(
        x = fct_reorder(label_en, `ASR (World)`), y = `ASR (World)`,
        fill = region_type
    )
) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c(
        "Metropolitan France" = "grey60",
        "Overseas territories" = "#2c7fb8"
    )) +
    labs(
        x = NULL, y = "ASR (World) per 100k",
        title = "Age-standardized rate (ASR) for prostate cancer incidence in continental and overseas France",
        caption = "Source: IARC Globocan 2022"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    )

# SAVING THE PLOTS #

# English version
output_dir <- here::here("public-health", "exports")

ggsave(
    filename = file.path(output_dir, "prostate_incidence_france_overseas_en.png"),
    plot = p_asr_en,
    width = 8, height = 5, dpi = 320, bg = "white"
)
# French version
comparison_fr <- comparison %>%
    mutate(
        label_fr = case_when(
            Label == "France (metropolitan)" ~ "France métropolitaine",
            Label == "French Guyana" ~ "Guyane française",
            Label == "France, La Réunion" ~ "La Réunion",
            Label == "France, Guadeloupe" ~ "Guadeloupe",
            Label == "France, Martinique" ~ "Martinique",
            Label == "France, Mayotte" ~ "Mayotte",
            Label == "France, New Caledonia" ~ "Nouvelle-Calédonie",
            Label == "French Polynesia" ~ "Polynésie française",
            TRUE ~ Label
        )
    )

p_asr_fr <- ggplot(
    comparison_fr,
    aes(
        x = fct_reorder(label_fr, `ASR (World)`), y = `ASR (World)`,
        fill = region_type
    )
) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = c(
        "Metropolitan France" = "grey60",
        "Overseas territories" = "#2c7fb8"
    )) +
    labs(
        x = NULL, y = "Taux standardisé mondial (pour 100 000 habitants)",
        title = "Incidence du cancer de la prostate en France métropolitaine et d'Outre-mer",
        caption = "Source: IARC Globocan 2022"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    )

ggsave(
    filename = file.path(output_dir, "prostate_incidence_france_overseas_fr.png"),
    plot = p_asr_fr,
    width = 8, height = 5, dpi = 320, bg = "white"
)
