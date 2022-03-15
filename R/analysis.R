source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

fp_data_wb <- read_csv(here("data/cleaned_fluoroproj_data_dups.csv"))

inst_avg <- fp_data_wb %>%
  filter(method %in% c("frozen","extracted"), units == "µg/L", 
         variable == "chl") %>%
  group_by(instrument, variable) %>%
  summarize(instrument_avg_conc = mean(avg_value)) %>%
  ungroup() 
  
tril_fq_data <- fp_data_wb %>%
  filter(method %in% c("frozen", "extracted"),
         instrument %in% c("trilogy", "duluth fluoroquik"),
         variable %in% c("chl", "ch1 hi", "ch1 lo", "ch2 hi", "ch2 lo")) %>%
  select(date, waterbody, dups, instrument, variable, units, avg_value) %>%
  filter(!(instrument == "trilogy" & units == "rfu")) %>%
  pivot_wider(date:dups, names_from = c("instrument", "variable", "units"), 
              values_from = avg_value)

trilogy_conc_vs_fluoroquick_conc <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_chl_µg/L`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch1 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch1 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3)  

trilogy_conc_vs_fluoroquick_ch1_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch1 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch1 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch2_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch2 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch2_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_chl_µg/L`, y = `duluth fluoroquik_ch2 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

combined_plot <- cowplot::plot_grid(trilogy_conc_vs_fluoroquick_conc,
                   trilogy_conc_vs_fluoroquick_ch2_lo,
                   trilogy_conc_vs_fluoroquick_ch2_hi,
                   trilogy_conc_vs_fluoroquick_conc,
                   trilogy_conc_vs_fluoroquick_ch1_lo,
                   trilogy_conc_vs_fluoroquick_ch1_hi,
                   nrow = 2, ncol = 3)

combined_plot


  
