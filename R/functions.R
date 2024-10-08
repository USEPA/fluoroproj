#' Merge/convert trilogy extracted chla data
#' 
merge_extracted_chla <- function(){
  files <- c(list.files(here("data/raw/extracted chl/"), ".csv", 
                        full.names = TRUE)) 
  files <- files[!grepl("prelimanary", files)]
  extracted_data <- purrr::map_df(files, 
                                  function(x) {
                                    xdf <- read_csv(x, na = c("", "NA", "na"))
                                    xdf <- mutate(xdf, site = as.character(site),
                                                  depth = as.character(depth),
                                                  dup = as.numeric(dup),
                                                  reps = as.numeric(reps))
                                    ce_convert_rfus(xdf, "ext_chla", "2022", 
                                                    "g04", std_check = TRUE)})
  extracted_data
}

#' Merge/convert trilogy extracted chla data
#' 
merge_extracted_phyco <- function(){
  files <- c(list.files(here("data/raw/extracted phyco/"), ".csv", 
                        full.names = TRUE))
  extracted_data <- purrr::map_df(files, 
                                  function(x) {
                                    xdf <- read_csv(x, na = c("", "NA", "na"))
                                    ce_convert_rfus(xdf, "phyco", "2021", 
                                                    "g04", std_check = TRUE)})
  extracted_data
}

#' Merge phycoprobe data
#' 
merge_phycoprobe <- function(){
  files <- list.files(here("data/raw/phycoprobe/"), ".csv", full.names = TRUE)
  phycoprobe_data <- purrr::map_df(files,  
                                   function(x) 
                                     read_csv(x, na = c("", "NA", "na", "--"))[-1,])
  
  phycoprobe_data
}   

#' Merge phycoprobe data
#'  
merge_invivo <- function(){
  
  files_fresh <- list.files(here("data/raw/trilogy in vivo/"), "fresh.csv", full.names = TRUE)
  files_frozen <- list.files(here("data/raw/trilogy in vivo/"), "frozen1.csv", full.names = TRUE)
  
  # Need to add in fresh/frozen method here.
  invivo_data_fresh <- purrr::map_df(files_fresh,  
                               function(x) {
                                 xdf <- read_csv(x, na = c("", "NA", "na"))
                                 ce_convert_rfus(xdf, "invivo_chla", "2021", 
                                                 "g04", std_check = FALSE)})
  invivo_data_frozen <- purrr::map_df(files_frozen,  
                                     function(x) {
                                       xdf <- read_csv(x, na = c("", "NA", "na")) 
                                       ce_convert_rfus(xdf, "invivo_chla", "2021", 
                                                       "g04", std_check = FALSE)})
  invivo_data_fresh <- mutate(invivo_data_fresh, method = "fresh")
  invivo_data_frozen <- mutate(invivo_data_frozen, method = "frozen")
  invivo_data <- bind_rows(invivo_data_fresh, invivo_data_frozen)
  invivo_data <- filter(invivo_data, units == "rfu")
  invivo_data <- unique(invivo_data)
  invivo_data
}   

#' Clean cyanoflour/fluoroquick data
#' 
#' @param df cyanoflour/fluoroquick data frame
clean_handheld <- function(df){
  
  handheld_data <- mutate(df, date = ymd(paste(year, month, day)),
                          variable = tolower(parameter))
  handheld_data <- select(handheld_data, date, waterbody, field_dups = dup, 
                          lab_reps = reps, instrument, 
                          method = `fresh/frozen1/frozen3`, variable, units, value)
  handheld_data <- filter(handheld_data, !variable %in% c("chl blk", "pc blk"))
  handheld_data <- mutate(handheld_data, units = case_when(units == "ug/l" ~
                                                             "µg/L",
                                                           TRUE ~ units))
  handheld_data <- mutate(handheld_data, field_dups = as.character(field_dups),
                          lab_reps = as.character(lab_reps))
  # Assumes hi is chl and lo is phyco
  # What are ch1/ch2 hi/lo???
  
  #borrowed from https://stackoverflow.com/questions/54221280/how-to-declare-encoding-for-all-character-columns-in-a-data-frame
  handheld_data <- dplyr::mutate_if(handheld_data, is.character, .funs = 
                                        function(x){return(`Encoding<-`(x, "UTF-8"))})
  handheld_data
}

#' Clean phycoprobe data
#' 
#' @param df phycoprobe data frame
clean_phycoprobe <- function(df){  
  
  phycoprobe_data <- rename_all(df, tolower)
  phycoprobe_data <- mutate(phycoprobe_data, date = ymd(paste(year, month, day)),
                            instrument = "phycoprobe", units = "µg/L")
  #Assuming this data is what we want from phycoprobe
  phycoprobe_data <- select(phycoprobe_data, date, waterbody, field_dups = dup, 
                            lab_reps = reps, instrument, 
                            method = `fresh/frozen1`, units, 
                            chl = `total conc.`, bluegreen = `bluegreen...8`, unbound_phyco = )
  phycoprobe_data <- pivot_longer(phycoprobe_data, cols = chl:bluegreen, 
                                  names_to = "variable", values_to = "value")
  phycoprobe_data <- mutate(phycoprobe_data, value = as.numeric(value),
                            units = case_when(variable == "bluegreen" ~
                                                "µg/L of chlorophyll",
                                              TRUE ~ units))
  phycoprobe_data <- group_by(phycoprobe_data, date, waterbody, field_dups, 
                              lab_reps, instrument, method, units, variable)
  phycoprobe_data <- summarize(phycoprobe_data, value = mean(value, na.rm = TRUE))
  phycoprobe_data <- ungroup(phycoprobe_data)
  phycoprobe_data <- select(phycoprobe_data, date:method, variable, units, 
                            value)
  phycoprobe_data <- mutate(phycoprobe_data, field_dups = as.character(field_dups),
                          lab_reps = as.character(lab_reps))
  
  #borrowed from https://stackoverflow.com/questions/54221280/how-to-declare-encoding-for-all-character-columns-in-a-data-frame
  phycoprobe_data <- dplyr::mutate_if(phycoprobe_data, is.character, .funs = 
                                  function(x){return(`Encoding<-`(x, "UTF-8"))})
  phycoprobe_data
}

#' Clean extracted data
#' 
#' @param df merged extracted data
clean_extracted <- function(df){
  
  extracted_data <- mutate(df, method = "extracted", instrument = "trilogy")
  extracted_data <- select(extracted_data, date, waterbody, field_dups, lab_reps, 
                           instrument, method, variable, units, value)
  extracted_data <- mutate(extracted_data, 
                           variable = case_when(variable == "ext_chla" ~
                                                  "chl",
                                                TRUE ~ variable),
                           waterbody = case_when(waterbody == "windmist farm" ~
                                                   "windmist", 
                                                 TRUE ~ waterbody),
                           units = tolower(units))

  extracted_ratio <- pivot_wider(extracted_data,
                                 id_cols = date:method,
                                 names_from = c("variable", "units"), 
                                 values_from = value)
 
  extracted_ratio <- mutate(extracted_ratio, 
                            `pc:chl_ratio-rfu` = phyco_rfu/chl_rfu,
                            `pc:chl_ratio-conc` = `phyco_µg/l`/`chl_µg/l`)
  extracted_ratio <- select(extracted_ratio, date:method, `pc:chl_ratio-rfu`, `pc:chl_ratio-conc`)
  extracted_ratio <- pivot_longer(extracted_ratio, c(`pc:chl_ratio-rfu`, `pc:chl_ratio-conc`), 
                                  names_to = c("variable", "units"), 
                                  names_sep = "_", values_to = "value")
  extracted_data <- bind_rows(extracted_data, extracted_ratio)
  extracted_data
}

#' Clean invivo data
#' 
#' @param df merged extracted data
clean_invivo <- function(df){
  invivo_data <- mutate(df, instrument = "trilogy in vivo")
  invivo_data <- select(invivo_data, date, waterbody, field_dups, lab_reps, 
                           instrument, method, variable, units, value)
  invivo_data <- mutate(invivo_data, 
                           variable = case_when(variable == "invivo_chla" ~
                                                  "chl",
                                                TRUE ~ variable),
                           waterbody = case_when(waterbody == "windmist farm" ~
                                                   "windmist", 
                                                 TRUE ~ waterbody))
  invivo_data
}

#' Clean field data
#' 
#' @param df merged extracted data
clean_field <- function(df){
  
  field_data <- mutate(df, date = ymd(paste(year, month, day)),
                       variable = case_when(variable == "PC" ~
                                              "pc",
                                            variable == "cyano" ~
                                              "pc",
                                            variable == "total" ~
                                              "chl",
                                            TRUE ~ variable),
                       units = case_when(variable == "pc" & instrument == "fluorosense" ~
                                           "µg/L",
                                         variable == "pc" & instrument == "algaetorch" ~
                                           "µg/L of chlorophyll",
                                         variable == "chl" ~
                                           "µg/L",
                                         variable == "turb" ~
                                           "FTU",
                                         variable == "%do" ~
                                           "%",
                                         variable == "do" ~
                                           "mg/L",
                                         variable == "spc" ~
                                           "µS/cm",
                                         variable == "salinity" ~
                                           "ppt",
                                         variable == "temp" ~
                                           "°C",
                                         variable == "ph" ~
                                           "ph",
                                         TRUE ~ NA_character_
                                         ),
                       field_dups = reps, method = case_when(notes == "frozen1" ~
                                                               "frozen",
                                                             TRUE ~ "fresh"),
                       lab_reps = case_when(grepl("culture", waterbody) ~
                                              1,
                                            TRUE ~ reps))
  field_data <- select(field_data, date, waterbody, field_dups, 
                       lab_reps, instrument, method, 
                       variable, units, value)
  field_data <- mutate(field_data, field_dups = as.character(field_dups),
                            lab_reps = as.character(lab_reps))
  field_data <- dplyr::mutate_if(field_data, is.character, .funs = 
                                        function(x){return(`Encoding<-`(x, "UTF-8"))})
  field_data
  
}

ext_vs_all_plot <- function(fpdata, var, meth, x_order = NULL){
  

  if(var == "chl"){
    varname <- "chlorophyll"
    xvar <- sprintf("extracted chlorophyll (\u03BCg/L)")
    my_breaks <- c(0, 15, 30)
    extracted_data <- fpdata %>%
      filter(method == "extracted", instrument == "trilogy",
             units != "rfu" , variable == var) %>%
      select(date, waterbody, field_dups, extracted_value = avg_value)
  } else if (var == "phyco"){
    varname <- "phycocyanin"
    xvar <- sprintf("extracted phycocyanin (\u03BCg/L)")
    my_breaks <- c(0, 5, 10)
    extracted_data <- fpdata %>%
      filter(method == "extracted", instrument == "trilogy",
             units != "rfu" , variable == var) %>%
      select(date, waterbody, field_dups, extracted_value = avg_value)
  } else if (var == "pc:chl"){
    varname <- var
    xvar <- "extracted phycocyanin rfu : extracted chlorophyll rfu"
    my_breaks <- c(0, 2, 4)
    extracted_data <- fpdata %>%
      filter(method == "extracted", instrument == "trilogy",
             units == "ratio-rfu" , variable == var) %>%
      select(date, waterbody, field_dups, extracted_value = avg_value)
  }
  
  
  
  plot_data <- fpdata %>%
    filter(method %in% meth, instrument != "trilogy",
           variable == var) %>%
    left_join(extracted_data) %>%
    mutate(instrument_unit = paste0(instrument, " ", varname, "\n(", units, ")")) 
  if(is.null(x_order)){
    x_order <- unique(plot_data$instrument_unit)
  }
  if(var == "chl"){
    plot_data <- plot_data %>%
      mutate(ylim = case_when(units == "µg/L" ~ 
                                50,
                              units == "rfu" ~
                                5000,
                              TRUE ~ avg_value))
  } else if(var == "phyco"){
    plot_data <- plot_data %>%
      mutate(ylim = case_when(units == "µg/L" ~ 
                                40,
                              units == "µg/L of chlorophyll" ~ 
                                40,
                              units == "rfu" ~
                                3000,
                              TRUE ~ avg_value))
  }
  
  plot_data <- plot_data %>%
    filter(instrument_unit %in% x_order) %>%
    mutate(instrument_unit = factor(instrument_unit, levels = x_order)) %>%
    group_by(instrument_unit) %>%
    mutate(r_square = summary(lm(avg_value ~ extracted_value))$r.squared,
           r_square = round(r_square, 2),
           r_square_x = case_when(variable == "chl" ~ 10,
                                  variable == "phyco" ~ 3,
                                  TRUE ~ 10),
           r_square_y = ylim * 0.9) %>%
    ungroup() 
  
  blank_data <- plot_data %>%
    select(extracted_value, avg_value = ylim, waterbody, instrument_unit)
  
  myplot <- plot_data %>%
    ggplot(aes(x = extracted_value, y = avg_value)) +
    geom_point(size = 4, aes(color = waterbody)) +
    geom_smooth(data = plot_data, method = "lm", se = FALSE) +
    geom_blank(data = blank_data) +
    facet_wrap(instrument_unit ~ ., scales = "free", strip.position = "left") +
    theme_ipsum_rc() +
    scale_color_viridis_d(option = "plasma") +
    theme(axis.title.y = element_blank(), 
          strip.text.y.left = element_text(angle=90, hjust = 1, size = 11), 
          strip.placement = "outside",
          axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 11, hjust = 0.5, vjust = -1),
          legend.text=element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_text(size = 11)) +
    labs(x = xvar) + 
    geom_text(aes(x = r_square_x, y = r_square_y,
                  label = paste0("R² = ", r_square)))
  myplot
}

beeswarm_plot <- function(fpdata, var, units, x_order = NULL){
  
  if(var == "chl" & units != "rfu"){
    yvar <- sprintf("chlorophyll (\u03BCg/L)")
  } else if (var == "phyco" & units != "rfu"){
    yvar <- sprintf("phycocyanin (\u03BCg/L)")
  } else if (var == "chl" & units == "rfu"){
    yvar <- sprintf("chlorophyll (rfu)")
  } else if (var == "phyco" & units == "rfu"){
    yvar <- sprintf("phycocyanin (rfu)")
  }
  
  if(units != "rfu"){
    fp_swarm_data <- fpdata %>% 
      filter(variable == var, units != "rfu") %>%
      mutate(instrument_method = paste0(instrument, " : ", method))
  } else {
    fp_swarm_data <- fpdata %>% 
      filter(variable == var, units == "rfu") %>%
      mutate(instrument_method = paste0(instrument, " : ", method))
  }
  if(is.null(x_order)){
    x_order <- unique(fp_swarm_data$instrument_method)
  }
  
  fp_swarm_data <- fp_swarm_data %>%
    filter(instrument_method %in% x_order) %>%
    mutate(instrument_method = factor(instrument_method, levels = x_order))
  
  swarm  <- fp_swarm_data  %>%
    ggplot(aes(y = avg_value, x = instrument_method, color = waterbody)) +
    geom_beeswarm(cex = 1.5, size = 4) +
    theme_ipsum_rc() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    labs(x = "", y = yvar) +
    theme(strip.text = element_text(size = 11), 
          axis.title.y = element_text(size = 11, vjust = 3),
          axis.title.x = element_text(size = 11),
          axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11))
  swarm
}


fq_plot <- function(fpdata, var, meth, x_order = NULL){
  
  if(var == "chl"){
    xvar <- sprintf("extracted chlorophyll (\u03BCg/L)")
  } else if (var == "phyco"){
    xvar <- sprintf("extracted phycocyanin (\u03BCg/L)")
  }
  
  extracted_data <- fpdata %>%
    filter(method == "extracted", instrument == "trilogy",
           units != "rfu" , variable == var) %>%
    select(date, waterbody, extracted_value = avg_value)
  
  plot_data <- fpdata %>%
    filter(method %in% meth, instrument == "fluoroquik") %>%
    left_join(extracted_data, by = c("date", "waterbody"))  
  if(is.null(x_order)){
    x_order <- unique(plot_data$variable)
  }
  plot_data <- plot_data %>%
    filter(variable %in% x_order) %>%
    mutate(variable = factor(variable, levels = x_order))
  
  
  myplot <- plot_data %>%
    ggplot(aes(x = extracted_value, y = avg_value, color = waterbody)) +
    geom_point(size = 3) +
    facet_wrap(variable ~ ., scales = "free", strip.position = "left") +
    theme_ipsum_rc() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme(axis.title.y = element_blank(), 
          strip.text.y.left = element_text(angle=90, hjust = 1, size = 11), 
          strip.placement = "outside",
          axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 11, hjust = 0.5, vjust = -1),
          legend.text=element_text(size = 11)) +
    labs(x = xvar)
  myplot
}

fq_fresh_frozen_plot <- function(fpdata, var, x_order = NULL){
  
  
  if(var == "chl"){
    xvar <- sprintf("extracted chlorophyll (\u03BCg/L)")
    yvar <- sprintf("chlorophyll (\u03BCg/L)")
    my_breaks <- c(0, 15, 30)
    my_y_breaks <- c(0, 1, 2)
    
  } else if (var == "phyco"){
    xvar <- sprintf("extracted phycocyanin (\u03BCg/L)")
    yvar <- sprintf("phycocyanin (\u03BCg/L)")
    my_breaks <- c(0, 5, 10)
    my_y_breaks <- c(0, 50, 100)
  }
  
  extracted_data <- fpdata %>%
    filter(method == "extracted", instrument == "trilogy",
           units != "rfu" , variable == var) %>%
    select(date, waterbody, extracted_value = avg_value)
  
  plot_data <- fpdata %>%
    filter(method %in% c("fresh", "frozen"), instrument == "fluoroquik", 
           variable == var) %>%
    left_join(extracted_data, by = c("date", "waterbody"))  
  if(is.null(x_order)){
    x_order <- unique(plot_data$variable)
  }
  plot_data <- plot_data %>%
    filter(variable %in% x_order) %>%
    mutate(variable = factor(variable, levels = x_order))
  
  
  myplot <- plot_data %>%
    ggplot(aes(x = extracted_value, y = avg_value, color = waterbody)) +
    geom_point(size = 3) +
    facet_wrap(. ~ method, strip.position = "top") +
    theme_ipsum_rc() +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_x_continuous(breaks = my_breaks) +
    scale_y_continuous(breaks = my_y_breaks, labels = my_y_breaks, 
                       limits = range(my_y_breaks)*1.1) +
    theme(strip.text = element_text(size = 11), 
          axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 11, hjust = 0.5, vjust = -1),
          axis.title.y = element_text(size = 11, hjust = 0.5, vjust = 2),
          legend.text=element_text(size = 11)) +
    labs(x = xvar, y = yvar)
  myplot
}

#' Clean Phycotech data
clean_phycotech <- function(phycotech_df){
  phycotech_df_clean <- phycotech_df %>%
    select(waterbody = system_name, date = sample_date, division, 
           concentration = concentration_natural_units_per_ml_,
           relative_concentration, 
           biovolume_concentration = total_biovolume_cubic_um_per_ml_,
           relative_biovolume = relative_total_biovolume) %>%
    # TODO: Standardize names with our data
    mutate(date = ymd(date),
           waterbody = case_when(waterbody == "Yawagoo Pond" ~
                                   "Yawgoo",
                                 TRUE ~ waterbody)) %>%
    filter(date > "2021-09-01")
  phycotech_df_clean
}

#' Clean Phycotech data
clean_phycotech_cyano <- function(phycotech_df){
  
  phycotech_df_clean <- phycotech_df %>%
    select(waterbody = system_name, date = sample_date, division, family, genus, 
           biovolume_concentration = total_biovolume_cubic_um_per_ml_,
           relative_biovolume = relative_total_biovolume,
           concentration = concentration_natural_units_per_ml_,
           relative_concentration) %>%
    # TODO: Standardize names with our data
    mutate(date = ymd(date),
           waterbody = case_when(waterbody == "Yawagoo Pond" ~
                                   "Yawgoo",
                                 TRUE ~ waterbody)) %>%
    filter(date > "2021-09-01") %>%
    filter(division == "Cyanophyta") %>%
    group_by(waterbody,family, genus) %>%
    summarize(biovolume = sum(biovolume_concentration, na.rm = TRUE),
              relative_biovolume = sum(relative_biovolume, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(waterbody, desc(relative_biovolume)) %>%
    mutate(genus = case_when(is.na(genus) ~ 
                               "Unknwon Chroococcaceae",
                             TRUE ~ genus))
  phycotech_df_clean
}

#' Make grouped bar plot
grouped_bar_plot <- function(phycotech_df, yvar){
 
  yvar <- rlang::sym(yvar)
  bar_plot <- phycotech_df %>%
    ggplot(aes(x = waterbody, y = !!yvar, fill = barplot_groups)) +
    geom_bar(position = "dodge", stat = "identity") +
    scale_fill_manual(values = c("brown","springgreen", "goldenrod", "cyan","grey50")) +
    theme_ipsum_rc()
    
  bar_plot
}

# Make scatterplot of fluoro concentrations vs counts
flouro_vs_count_plot <- function(fluoro_df, phycotech_df, xvar = c("chlorophyll", "phycocyanin")){
  
  xvar <- match.arg(xvar)
  plot_data <- fluoro_df %>%
    mutate(instrument_unit = paste0(instrument, " ", xvar, "\n(", units, ")")) 
  #if(is.null(x_order)){
  #  x_order <- unique(plot_data$instrument_unit)
  #}
  if(xvar == "chlorophyll"){
    plot_data <- plot_data %>%
      mutate(ylim = case_when(units == "µg/L" ~ 
                                50,
                              units == "rfu" ~
                                5000,
                              TRUE ~ avg_value))
  } else if(xvar == "phycocyanin"){
    plot_data <- plot_data %>%
      mutate(ylim = case_when(units == "µg/L" ~ 
                                40,
                              units == "µg/L of chlorophyll" ~ 
                                40,
                              units == "rfu" ~
                                3000,
                              TRUE ~ avg_value))
  }
  
  fluoro_wb <- plot_data %>%
    filter(variable %in% c("chl", "phyco"),
           !(instrument == "trilogy" & units == "rfu")) %>%
    select( -date, -sd_value) %>%
    pivot_wider(names_from = c(variable), values_from = avg_value) %>%
    group_by(waterbody, instrument_unit) %>%
    summarize(chl = mean(chl,na.rm = TRUE), phyco = mean(phyco, na.rm = TRUE)) %>%
    ungroup()
  phyco_wb <- phycotech_df %>%
    filter(division == "Cyanophyta") %>%
    group_by(waterbody) %>%
    summarize(concentration  = sum(concentration, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(waterbody = stringr::str_replace(waterbody, " Pond", "")) %>%
    mutate(waterbody = stringr::str_replace(waterbody, " Lake", "")) %>%
    mutate(waterbody = stringr::str_replace(waterbody, " Reservoir", "")) %>%
    mutate(waterbody = case_when(waterbody == "Yawagoo" ~
                                   "yawgoo",
                                 waterbody == "JL Curran" ~
                                   "curran",
                                 TRUE ~ waterbody)) %>%
    mutate(waterbody = tolower(waterbody))
  
  if(xvar == "chlorophyll"){
    plot_df <- left_join(fluoro_wb, phyco_wb) %>%
      select(waterbody, instrument_unit, yvar = chl, concentration) %>%
      filter(!is.na(yvar))
  } else if(xvar == "phycocyanin"){
    plot_df <- left_join(fluoro_wb, phyco_wb) %>%
      select(waterbody, instrument_unit, yvar = phyco, concentration) %>%
      filter(!is.na(yvar))
  }
  
  plot_df <- plot_df %>%
    mutate(instrument = factor(instrument_unit)) %>%
    group_by(instrument) %>%
    mutate(r_square = summary(lm(yvar ~ concentration))$r.squared,
           r_square = round(r_square, 2),
           r_square_x = 40000,
           r_square_y = max(yvar)*1.275) %>%
    ungroup()
  
  myplot <- plot_df %>%
    ggplot(aes(x = concentration, y = yvar)) +
    geom_point(size = 4, aes(color = waterbody)) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(instrument ~ ., scales = "free", strip.position = "left") +
    theme_ipsum_rc() +
    scale_color_viridis_d(option = "plasma") +
    theme(axis.title.y = element_blank(), 
          strip.text.y.left = element_text(angle=90, hjust = 1, size = 11), 
          strip.placement = "outside",
          axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 11, hjust = 0.5, vjust = -1),
          legend.text=element_text(size = 11),
          axis.text.y = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.position = c(0.9, 0.17)) +
    labs(x = "cyanobacterial cells/ml") + 
    geom_text(aes(x = r_square_x, y = r_square_y,
                  label = paste0("R² = ", r_square)), color="black")
  myplot
}

map_field_sites <- function(){
  
  ri <- st_read(here::here("data/ri.shp")) |>
    st_transform(4326)
  ri_wb <- st_read(here::here("data/ri_lakes.shp")) |>
    st_transform(4326)
  sites <- ri_wb |>
    filter(GNIS_NAME %in% c("Barber Pond", 
                            "John L Curran Lower Reservoir", "Indian Lake",
                            "Mashapaug Pond", "Warwick Pond", "Yawgoo Pond"))
    
  sites_gg <- ggplot(ri) +
    geom_sf(size = 0.75, fill = "grey95", show.legend = FALSE) +
    geom_sf(data = ri_wb, fill = "darkblue", color = NA, alpha = 0.5) +
    geom_sf(data= st_centroid(sites), aes(color = GNIS_NAME), size = 2.5) +
    scale_color_viridis_d(option = "plasma") +
    scale_shape_manual(values = c(16,17)) +
    scale_x_continuous(breaks = seq(-72.0, -71.0, by  = 0.2)) +
    scale_y_continuous(breaks = seq(41.0, 42.0, by = 0.2)) +
    theme_ipsum() +
    theme(legend.position = "bottom", legend.title = element_blank())
    #theme(legend.title = element_blank()) #+
    #guides(color = guide_legend(nrow = 3))
  sites_gg
  
}

summary_table <- function(fluoro_df){
  #idx <- !(fluoro_df$instrument == "trilogy" & fluoro_df$units == "rfu")
  summ_df <- fluoro_df |>
    filter(method == "extracted" & units == "µg/L") |>
    filter(variable %in% c("chl", "phyco")) |>
    group_by(waterbody, instrument, variable, units) |>
    summarise(mean = mean(avg_value, na.rm = TRUE), sd = sd(avg_value, na.rm = TRUE)) |>
    ungroup()
  summ_df <- filter(summ_df, instrument == "trilogy") |>
    mutate(mean = round(mean, 1), sd = round(sd, 1)) |>
    select(!instrument) |>
    as.data.frame()
  list(chl_summary = summ_df[summ_df$variable == "chl",], 
       phyco_summary = summ_df[summ_df$variable == "phyco",])
}
