source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

################################################################################
# Data prep
################################################################################

# Read in data
handheld_data <- read_csv(here("data/raw/cyanofluor_fluoroquik_data.csv"))
phycoprobe_data <- merge_phycoprobe()
extracted_chla_data <- merge_extracted_chla()
#extracted_phyco_data <- merge_extracted_phyco()
invivo_data <- merge_invivo()

# Clean up data
handheld_data <- clean_handheld(handheld_data)
phycoprobe_data <- clean_phycoprobe(phycoprobe_data)
extracted_chla_data <- clean_extracted(extracted_chla_data)#, extracted_phyco_data)


fluoroproj_data <- bind_rows(handheld_data, phycoprobe_data, extracted_chla_data)

x <- fluoroproj_data %>% 
  filter(variable == "chl") %>%
  group_by(date, waterbody, instrument, variable, units) %>%
  summarize(chl = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(waterbody,instrument, units) %>%
  summarize(chl = mean(chl)) %>%
  ungroup() %>%
  filter(units == "µg/L") %>%
  tidyr::pivot_wider(id_cols = "waterbody", names_from = "instrument", 
                     values_from = "chl") %>%
  filter(complete.cases(.))

  
