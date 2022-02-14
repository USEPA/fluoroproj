#' Merge/convert trilogy extracted chla data
#' 
merge_extracted_chla <- function(){
  files <- c(list.files(here("data/raw/extracted chl/"), ".csv", 
                        full.names = TRUE))
  extracted_data <- purrr::map_df(files, 
                                  function(x) 
                                    ce_convert_rfus(x, "ext_chla", "2021", 
                                                    "ours", std_check = FALSE))
  extracted_data
}

#' Merge/convert trilogy extracted chla data
#' 
merge_extracted_phyco <- function(){
  files <- c(list.files(here("data/raw/extracted phyco/"), ".csv", 
                        full.names = TRUE))
  browser()
  extracted_data <- purrr::map_df(files, 
                                  function(x) 
                                    read_csv(x, na = c("", "NA", "na")))
  extracted_data
}

#' Merge phycoprobe data
#' 
merge_phycoprobe <- function(){
  files <- list.files(here("data/raw/phycoprobe/"), ".csv", full.names = TRUE)
  #phycoprobe_header <- read_csv(files[1], n_max = 1)
  phycoprobe_data <- purrr::map_df(files,  
                                   function(x) 
                                     read_csv(x, na = c("", "NA", "na", "--"))[-1,])
  #phycoprobe_data <- bind_rows(phycoprobe_header, phycoprobe_data)
  phycoprobe_data
}   

#' Merge phycoprobe data
#'  
merge_invivo <- function(){
  files <- list.files(here("data/raw/trilogy in vivo/"), ".csv", full.names = TRUE)
  invivo_data <- purrr::map_df(files,  
                               function(x) 
                                 read_csv(x, na = c("", "NA", "na")))
  invivo_data
}   

#' Clean cyanoflour/fluoroquick data
#' 
#' @param df cyanoflour/fluoroquick data frame
clean_handheld <- function(df){
  handheld_data <- mutate(df, date = ymd(paste0(year, month, day)),
                          variable = tolower(parameter))
  handheld_data <- select(handheld_data, date, waterbody, dups = dup, reps, 
                          instrument, method = `fresh/frozen1/frozen3`, 
                          variable, units, value)
  handheld_data <- filter(handheld_data, variable %in% c("chl", "chl blk", 
                                                          "pc", "pc blk"))
  handheld_data <- mutate(handheld_data, units = case_when(units == "ug/l" ~
                                                             "µg/L",
                                                           TRUE ~ units))
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
  phycoprobe_data <- mutate(phycoprobe_data, date = ymd(paste0(year, month, day)),
                            instrument = "phycoprobe", units = "µg/L")
  #Assuming this data is what we want from phycoprobe
  phycoprobe_data <- select(phycoprobe_data, date, waterbody, dups = dup, reps, 
                            instrument, method = `fresh/frozen1`, units,
                            chl = `total conc.`, bluegreen = `bluegreen...8`)
  phycoprobe_data <- pivot_longer(phycoprobe_data, cols = chl:bluegreen, 
                                  names_to = "variable", values_to = "value")
  phycoprobe_data <- select(phycoprobe_data, date:method, variable, units, 
                            value)
  phycoprobe_data <- mutate(phycoprobe_data, value = as.numeric(value))
  #borrowed from https://stackoverflow.com/questions/54221280/how-to-declare-encoding-for-all-character-columns-in-a-data-frame
  phycoprobe_data <- dplyr::mutate_if(phycoprobe_data, is.character, .funs = 
                                  function(x){return(`Encoding<-`(x, "UTF-8"))})
  phycoprobe_data
}

#' Clean extracted data
#' 
#' @param df merged extracted data
clean_extracted <- function(df){
  #browser()
  extracted_data <- mutate(df, method = "extracted", instrument = "trilogy")
  extracted_data <- select(extracted_data, date, waterbody, dups, reps, 
                           instrument, method, variable, units, value)
  extracted_data <- mutate(extracted_data, 
                           variable = case_when(variable == "ext_chla" ~
                                                  "chl",
                                                TRUE ~ variable),
                           waterbody = case_when(waterbody == "windmist farm" ~
                                                   "windmist", 
                                                 TRUE ~ waterbody))
  extracted_data
}