library(compeco)
library(readr)
library(here)

my_rfus <- read_csv(here("data/raw/extracted chl/chl_2021_10_6_&_21_&_11_9.csv"),
                    na = c("NA", "na", ""))

my_converted_rfus <- ce_convert_rfus(my_rfus, module = "ext_chla", year = "2022", 
                fluorometer = "g04")
