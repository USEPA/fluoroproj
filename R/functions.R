#' Function to open and merge data
#' 
merge_data <- function(){
  browser()
  cf_fq_data <- read_csv(here("data/raw/cyanofluor_fluoroquik_data.csv"))
  
  extract_chla_data <- read_csv(here("data/raw/extracted chl/chl_2021_10_6_&_21_&_11_9.csv"))
  extract_phyco_data1 <- read_csv(here("data/raw/extracted phyco/phyco_2021_10_21_&_11_9.csv"))
  extract_phyco_data2 <- read_csv(here("data/raw/extracted phyco/phyco_2021_10_6_&_21.csv"))
  extract_data <- bind_rows(extract_chla_data, extract_phyco_data1, 
                            extract_phyco_data2)
  
  phycoprobe_fresh_data1 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_10_21_fresh.csv"))
  phycoprobe_fresh_data2 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_10_6_fresh.csv"))
  phycoprobe_fresh_data3 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_11_9_fresh.csv"))
  phycoprobe_fresh <- bind_rows(phycoprobe_fresh_data1, phycoprobe_fresh_data2, 
                                phycoprobe_fresh_data3)
  
  phycoprobe_frozen_data1 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_10_21_frozen1.csv"))
  phycoprobe_frozen_data2 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_10_6_frozen1.csv"))
  phycoprobe_frozen_data3 <- read_csv(here("data/raw/phycoprobe/phycoprobe_2021_11_9_frozen1.csv"))
  phycoprobe_frozen <- bind_rows(phycoprobe_frozen_data1, 
                                 phycoprobe_frozen_data2, 
                                 phycoprobe_frozen_data3)
  
  phycoprobe_data <- bind_rows(phycoprobe_fresh, phycoprobe_frozen)
    
  invivo_fresh_data1 <- read_csv(here("data/raw/trilogy in vivo/trilogy in vivo 2021_10_21_fresh.csv"))
}