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

# Clean up data
handheld_data <- clean_handheld(handheld_data)
phycoprobe_data <- clean_phycoprobe(phycoprobe_data)
extracted_data <- bind_rows(extracted_chla_data, extracted_phyco_data)
extracted_data <- clean_extracted(extracted_data)
invivo_data <- clean_invivo(invivo_data)


fluoroproj_data <- bind_rows(handheld_data, phycoprobe_data, extracted_data,
                             invivo_data)
fluoroproj_data <- mutate(fluoroproj_data, units = case_when(units == "µg/l" ~
                                              "µg/L",
                                            TRUE ~ units),
                          variable = case_when(variable == "bluegreen" ~
                                                 "phyco",
                                               variable == "pc" ~
                                                 "phyco",
                                               TRUE ~ variable))

x <- fluoroproj_data %>% 
  group_by(date, waterbody, instrument, variable, units) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  #group_by(waterbody,instrument, units) %>%
  #summarize(chl = mean(chl)) %>%
  #ungroup() %>%
  filter(units == "rfu") #%>%
  #select(-units) %>%
  #tidyr::pivot_wider(id_cols = c("date", "waterbody", "instrument"), names_from = "variable", 
  #                   values_from = "value")
  #select(waterbody,trilogy, everything()) %>%
  #arrange(trilogy)

x <- pivot_wider(fluoroproj_data, date:reps, names_from = instrument:units, values_from = value)
x <- filter(x, !is.na(`cyanofluor_fresh_pc:chl_ratio`))
trilogy <- select(x, date:reps, contains("trilogy"))
