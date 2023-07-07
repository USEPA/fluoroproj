#source(here::here("R/data_prep.R"))
source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

fp_data_wb <- read_csv(here("data/cleaned_fluoroproj_data_dups.csv")) %>%
  filter(!grepl("culture", waterbody),
         !grepl("standard", waterbody),
         waterbody != "windmist") %>%
  filter(!(waterbody %in% c("lower melville", "upper melville")))

phycotech_data <- read_csv(here("data/cleaned_phycotech_data.csv")) %>%
  filter(!(waterbody %in% c("Melville Pond"))) %>%
  mutate(barplot_groups = case_when(division == "Euglenophyta" ~
                                      "Other",
                                    division == "Haptophyta" ~
                                      "Other",
                                    division == "Pyrrhophyta" ~
                                      "Other", 
                                    division == "Cryptophyta" ~
                                      "Other",
                                    TRUE ~ division))

# Figures
# Have instruments in same spot on figure
# color-blind friendly palette
# Ratio Figure 
# Grouped bar chart with waterbody on x, and bars for each division, relative and total biovolume - read from pre-sums division tab
# Crazy idea, scatterplots from above but size of point from relative cyano biovolume
# Compare phyco and chl to cell counts - maybe can make argument that cell counts bad, phyco good.
chla_compare_plot <- ext_vs_all_plot(fp_data_wb, "chl", c("fresh", "extracted"),
                                     c("algaetorch (µg/L)", "phycoprobe (µg/L)",
                                       "cyanofluor (rfu)",
                                       "trilogy in vivo (rfu)"))
ggsave(here::here("figures/fig2_chla_scatter.jpg"), chla_compare_plot, 
       width = 7.5, height = 5.25, dpi = 300)

phyco_compare_plot <- ext_vs_all_plot(fp_data_wb, "phyco", c("fresh", "extracted"),
                                      c("algaetorch (µg/L of chlorophyll)", 
                                        "phycoprobe (µg/L of chlorophyll)",
                                        "cyanofluor (rfu)", 
                                        "fluorosense (µg/L)"))
ggsave(here::here("figures/fig3_phyco_scatter.jpg"), phyco_compare_plot, 
       width = 7.5, height = 5.25, dpi = 300)

ratio_compare_plot <- ext_vs_all_plot(fp_data_wb, "pc:chl", c("fresh", "extracted"))

division_bar_plot_relative <- grouped_bar_plot(phycotech_data, "relative_biovolume")
ggsave(here::here("figures/fig4_rel_bio_bar.jpg"), division_bar_plot_relative, 
       width = 7.5, height = 5.25, dpi = 300)

division_bar_plot_total <- grouped_bar_plot(phycotech_data, "biovolume_concentration")
ggsave(here::here("figures/fig5_total_bio_bar.jpg"), division_bar_plot_total, 
       width = 7.5, height = 5.25, dpi = 300)






# Beyond here be dragons
inst_avg <- fp_data_wb %>%
  filter(method %in% c("frozen","extracted"), units == "µg/L", 
         variable == "chl") %>%
  group_by(instrument, variable) %>%
  summarize(instrument_avg_conc = mean(avg_value)) %>%
  ungroup() 
inst_avg

tril_fq_data <- fp_data_wb %>%
  filter(method %in% c("frozen", "extracted"),
         instrument %in% c("trilogy", "duluth fluoroquik"),
         variable %in% c("phyco", "ch1 hi", "ch1 lo", "ch2 hi", "ch2 lo")) %>%
  select(date, waterbody, dups, instrument, variable, units, avg_value) %>%
  filter(!(instrument == "trilogy" & units == "rfu")) %>%
  pivot_wider(date:dups, names_from = c("instrument", "variable", "units"), 
              values_from = avg_value)

trilogy_conc_vs_fluoroquick_conc <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_phyco_µg/L`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch1 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch1 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3)  

trilogy_conc_vs_fluoroquick_ch1_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch1 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch1_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch1 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch2_hi <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch2 hi_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

trilogy_conc_vs_fluoroquick_ch2_lo <- tril_fq_data %>%
  ggplot(aes(x = `trilogy_phyco_µg/L`, y = `duluth fluoroquik_ch2 lo_rfu`, 
             color = waterbody)) +
  geom_point(size = 3) 

combined_plot_phyco <- cowplot::plot_grid(trilogy_conc_vs_fluoroquick_conc,
                   trilogy_conc_vs_fluoroquick_ch2_lo,
                   trilogy_conc_vs_fluoroquick_ch2_hi,
                   trilogy_conc_vs_fluoroquick_conc,
                   trilogy_conc_vs_fluoroquick_ch1_lo,
                   trilogy_conc_vs_fluoroquick_ch1_hi,
                   nrow = 2, ncol = 3)

combined_plot_phyco

tril_all_data <- fp_data_wb %>%
  filter(method %in% c("fresh", "extracted"),
         !instrument %in% c("uri fluoroquik", "sonde"),
         variable %in% c("phyco")) %>%
  select(date, waterbody, dups, instrument, variable, units, avg_value) %>%
  filter(!(instrument == "trilogy" & units == "rfu")) %>%
  pivot_wider(date:dups, names_from = c("instrument", "variable", "units"), 
              values_from = avg_value)

plot(tril_all_data[,4:9])
  
