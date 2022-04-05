source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

fp_data <- read_csv(here("data/cleaned_fluoroproj_data_dups.csv")) %>%
  filter(instrument != "uri fluoroquik" & !is.na(method)) %>%
  mutate(instrument = case_when(instrument == "duluth fluoroquik" ~
                                  "fluoroquik",
                                TRUE ~ instrument)) %>%
  filter(field_dups < 4) %>%
  group_by(date, waterbody,instrument,method,variable,units) %>%
  summarize(avg_value = mean(avg_value)) %>%
  ungroup()
  

# Plot #1 - Extracted Chl vs All others Chl scatterplot matrix
chla_compare_plot <- ext_vs_all_plot(fp_data, "chl", c("fresh", "extracted"),
                                     c("algaetorch (µg/L)", "phycoprobe (µg/L)",
                                       "fluoroquik (µg/L)", "cyanofluor (rfu)",
                                       "trilogy in vivo (rfu)"))

# Plot #2 - Extracted Phyco vs All others Phyco scatterplot matrix
phyco_compare_plot <- ext_vs_all_plot(fp_data, "phyco", c("fresh", "extracted"),
                                      c("algaetorch (µg/L of chlorophyll)", 
                                        "phycoprobe (µg/L of chlorophyll)",
                                        "fluoroquik (µg/L)", 
                                        "fluorosense (µg/L)", 
                                        "cyanofluor (rfu)"))


# Plot #3 - chl, instrument, and fresh frozen avg bar chart
chla_swarm_ugl <- fp_data %>%
  #filter(instrument != "fluoroquik") %>%
  group_by(waterbody,instrument,method,variable,units) %>%
  summarize(avg_value = mean(avg_value)) %>%
  ungroup() %>%
  beeswarm_plot("chl", "µg/L", c("trilogy : extracted","phycoprobe : fresh", "phycoprobe : frozen", "fluoroquik : fresh", "fluoroquik : frozen"))

chla_swarm_rfu <- fp_data %>%
  filter(instrument != "fluoroquik") %>%
  group_by(waterbody,instrument,method,variable,units) %>%
  summarize(avg_value = mean(avg_value)) %>%
  ungroup() %>%
  beeswarm_plot("chl", "rfu", c("cyanofluor : fresh", "cyanofluor : frozen",
                                "trilogy in vivo : fresh", 
                                "trilogy in vivo : frozen")) 
# Plot #4 - phyco, instrument, and fresh frozen avg bar chart
phyco_swarm_ugl <- fp_data %>%
  group_by(waterbody,instrument,method,variable,units) %>%
  summarize(avg_value = mean(avg_value)) %>%
  ungroup() %>%
  beeswarm_plot("phyco", "µg/L", c("trilogy : extracted","phycoprobe : fresh", "phycoprobe : frozen", "fluoroquik : fresh","fluoroquik : frozen"))
phyco_swarm_rfu <- fp_data %>%
  group_by(waterbody,instrument,method,variable,units) %>%
  summarize(avg_value = mean(avg_value)) %>%
  ungroup() %>%
  beeswarm_plot("phyco", "rfu", c("cyanofluor : fresh", "cyanofluor : frozen"))

ggsave(here("figures/chla_instrument_compare.png"), chla_compare_plot, 
       width = 15, height = 8.5)  
ggsave(here("figures/phyco_instrument_compare.png"), phyco_compare_plot, 
       width = 15, height = 8.5)
ggsave(here("figures/chla_ugl_fresh_frozen.png"), chla_swarm_ugl, 
       width = 15, height = 8.5)
ggsave(here("figures/chla_rfu_fresh_frozen.png"), chla_swarm_rfu, 
       width = 15, height = 8.5)
ggsave(here("figures/phyco_ugl_fresh_frozen.png"), phyco_swarm_ugl, 
       width = 15, height = 8.5)
ggsave(here("figures/phyco_rfu_fresh_frozen.png"), phyco_swarm_rfu, 
       width = 15, height = 8.5)


# Fluoroquik only plots

fq_compare_fresh_chla_plot <- fp_data %>%
  fq_plot("chl", c("fresh", "frozen"), c("chl"))
fq_compare_fresh_chla_plot

fq_compare_frozen_chla_plot <- fp_data %>%
  fq_plot("chl", "frozen", c("ch1 hi", "ch1 lo", "chl", "ch2 hi", "ch2 lo"))
fq_compare_frozen_chla_plot

fq_compare_fresh_phyco_plot <- fp_data %>%
  fq_plot("phyco", "fresh", c("ch1 hi", "ch1 lo", "phyco", "ch2 hi", "ch2 lo"))
fq_compare_fresh_phyco_plot

fq_compare_frozen_phyco_plot <- fp_data %>%
  fq_plot("phyco", "frozen", c("ch1 hi", "ch1 lo", "phyco", "ch2 hi", "ch2 lo"))
fq_compare_frozen_phyco_plot

fq_compare_chl_fresh_frozen_plot <- fp_data %>%
  fq_fresh_frozen_plot("chl")
fq_compare_phyco_fresh_frozen_plot <- fp_data %>%
  fq_fresh_frozen_plot("phyco")

ggsave(here("figures/fluoroquik_chl_fresh_frozen.png"),
       fq_compare_chl_fresh_frozen_plot, 
       width = 15, height = 8.5)
ggsave(here("figures/fluoroquik_phyco_fresh_frozen.png"),
       fq_compare_phyco_fresh_frozen_plot, 
       width = 15, height = 8.5)
