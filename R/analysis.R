#source(here::here("R/data_prep.R"))
source(here::here("R/packages.R"))
source(here::here("R/functions.R"))

fp_data_wb <- read_csv(here("data/cleaned_fluoroproj_data_dups.csv")) %>%
  filter(!grepl("culture", waterbody),
         !grepl("standard", waterbody),
         waterbody != "windmist") %>%
  filter(!(waterbody %in% c("lower melville", "upper melville")))

phycotech_data <- read_csv(here("data/cleaned_phycotech_data.csv")) %>%
  filter(!(waterbody %in% c("Melville Pond", "Georgiaville Pond", "Slack Reservoir", "Stafford Pond"))) %>%
  mutate(barplot_groups = case_when(division == "Euglenophyta" ~
                                      "Other",
                                    division == "Haptophyta" ~
                                      "Other",
                                    division == "Pyrrhophyta" ~
                                      "Other", 
                                    division == "Cryptophyta" ~
                                      "Other",
                                    TRUE ~ division)) %>%
  mutate(waterbody = case_when(waterbody == "Barber Pond" ~ "barber",
                               waterbody == "Indian Lake" ~ "indian",
                               waterbody == "Yawagoo Pond" ~ "yawgoo",
                               waterbody == "JL Curran Reservoir" ~ "curran",
                               waterbody == "Mashapaug Pond" ~ "mashapaug",
                               waterbody == "Warwick Pond" ~ "warwick",
                               TRUE ~ waterbody))


  
# Figures
# Have instruments in same spot on figure
# color-blind friendly palette
# Ratio Figure 
# Grouped bar chart with waterbody on x, and bars for each division, relative and total biovolume - read from pre-sums division tab
# Crazy idea, scatterplots from above but size of point from relative cyano biovolume
# Compare phyco and chl to cell counts - maybe can make argument that cell counts bad, phyco good.
chla_compare_plot <- ext_vs_all_plot(fp_data_wb, "chl", c("fresh", "extracted"),
                                     c("algaetorch chlorophyll\n(µg/L)", 
                                       "phycoprobe chlorophyll\n(µg/L)",
                                       "cyanofluor chlorophyll\n(rfu)",
                                       "trilogy in vivo chlorophyll\n(rfu)"))
ggsave(here::here("manuscript/images/fig2_chla_scatter.jpg"), chla_compare_plot, 
       width = 8.5, height = 5.25, dpi = 300)

phyco_compare_plot <- ext_vs_all_plot(fp_data_wb, "phyco", c("fresh", "extracted"),
                                      c("algaetorch phycocyanin\n(µg/L of chlorophyll)", 
                                        "phycoprobe phycocyanin\n(µg/L of chlorophyll)",
                                        "cyanofluor phycocyanin\n(rfu)", 
                                        "fluorosense phycocyanin\n(µg/L)"))
ggsave(here::here("manuscript/images/fig3_phyco_scatter.jpg"), phyco_compare_plot, 
       width = 8.5, height = 5.25, dpi = 300)

#ratio_compare_plot <- ext_vs_all_plot(fp_data_wb, "pc:chl", c("fresh", "extracted"))

division_bar_plot_cellsml <- grouped_bar_plot(phycotech_data, "concentration")
ggsave(here::here("manuscript/images/fig4_total_cellsml_bar.jpg"), division_bar_plot_cellsml, 
       width = 8.5, height = 5.25, dpi = 300)

# As of 10/19/2023 still need to clean these up a bit.
# consistency on y axis for each unit, x axis labels are scrunched
fluoro_chl_count_scatter <- flouro_vs_count_plot(fp_data_wb, phycotech_data, "chlorophyll")
ggsave(here::here("manuscript/images/fig5_chl_vs_cells.jpg"), fluoro_chl_count_scatter, 
       width = 8.5, height = 5.25, dpi = 300)

fluoro_phyco_count_scatter <- flouro_vs_count_plot(fp_data_wb, phycotech_data, "phycocyanin")
ggsave(here::here("manuscript/images/fig6_phyco_vs_cells.jpg"), fluoro_phyco_count_scatter, 
       width = 8.5, height = 5.25, dpi = 300)

# Field Site Map
field_sites <- map_field_sites() 
ggsave(here::here("manuscript/images/fig1_map.jpg"), field_sites, 
       width = 6, height = 8, dpi = 300)

# Fluoro summary
fluoro_summary <- summary_table(fp_data_wb)
