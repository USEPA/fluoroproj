source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

################################################################################
# Data prep
################################################################################

# Read in data
handheld_data <- read_csv(here("data/raw/cyanofluor_fluoroquik_data.csv"))
phycoprobe_data <- merge_phycoprobe()
extracted_chla_data <- merge_extracted_chla()
extracted_phyco_data <- merge_extracted_phyco()
invivo_data <- merge_invivo()
field_data <- read_csv(here("data/raw/field data.csv"))

# Clean up data
handheld_data <- clean_handheld(handheld_data)
phycoprobe_data <- clean_phycoprobe(phycoprobe_data)
extracted_data <- bind_rows(extracted_chla_data, extracted_phyco_data)
extracted_data <- clean_extracted(extracted_data)
invivo_data <- clean_invivo(invivo_data)
field_data <- clean_field(field_data)


fluoroproj_data <- bind_rows(handheld_data, phycoprobe_data, extracted_data,
                             invivo_data, field_data)
fluoroproj_data <- mutate(fluoroproj_data, units = case_when(units == "µg/l" ~
                                              "µg/L",
                                            TRUE ~ units),
                          variable = case_when(variable == "bluegreen" ~
                                                 "phyco",
                                               variable == "pc" ~
                                                 "phyco",
                                               TRUE ~ variable),
                          method = case_when(method == "frozen1" ~
                                               "frozen",
                                             TRUE ~ method))

fluoroproj_data_dups <- fluoroproj_data %>%
  group_by(date, waterbody, instrument, method, variable, units, dups) %>%
  summarize(avg_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>%
  ungroup()

write_csv(fluoroproj_data, here("data/cleaned_fluoroproj_data.csv"))
write_csv(fluoroproj_data_dups, here("data/cleaned_fluoroproj_data_dups.csv"))
