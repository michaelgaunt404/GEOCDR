#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Maps data locations, extracts links, writes out data for DYNA-MECH
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: sources map_utility.R
#-------- script maps locations
#-------- script extracts DTA links based off data locations
#-------- script writes out links and locations
#-------- manual QC process takes place to validate
#-------- script loads QC link/locations mappings and writes out for model use
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

secret_key = fread("./docs/tokens/token.csv", header = F)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script remotely knits RMarkdown files.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script needs to be colocated in same folder as data
#-------- Script writes to same location
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magrittr)
library(readxl)
library(tidyverse)
library(lubridate)
library(data.table)

# library(ggmap) #ggplot2 for spatial 
library(tmap) #vis for maps (ggmap alternative)
library(rgdal) #for inport/outport
library(rgeos) #for spatial analysis operations 


library(data.table)
library(tidyverse)
library(sf)
library(magrittr)
library(readxl)
library(data.table)
library(tidyverse)
library(hereR)
library(tictoc)
library(furrr)
future::plan(multiprocess)


suppressMessages({
  tmap_mode('view')
})

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
  setwd("~/")
  rstudioapi::getSourceEditorContext()$path %>%
    as.character() %>%
    gsub("R.*","\\1", .) %>%
    path.expand() %>%
    setwd()
}
getwd()
#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_folders = list.dirs("./data")

files_ca = "./data/openaddr-collected-us_west/us/ca"
files_all_states = "./data/openaddr-collected-us_west/us/"
list.files(paste0(getwd(), "/data/openaddr-collected-us_west/us/ca"))
list.dirs(files_all_states)[-1]
list.files("./data/openaddr-collected-us_west/us//wy")

data = list.dirs(files_all_states)[-1] %>%  
  data.table(folder_paths = .) %>%  
  mutate(state = str_trunc(folder_paths, 2, "left", ellipsis = ""), 
         folder_paths = str_replace(folder_paths, "//", "/")) %>%
  group_by(state) %>%
  nest() %>%
  mutate(file_paths = map(data, function(x) list.files(x)))
list.files(./data/openaddr-collected-us_west/us//wy)
  # # tidyr::separate(col = file_paths, sep = ",") %>%  
  # str_split_fixed(data$file_paths, pattern = ",", n = 45 ) %>%  
  #   remove_empty()
  #   data.table()



city_df = list.files(files_ca) %>%  
  data.table(files = .) %>% 
  .[!str_detect(files, ".vrt")] %>%  
  .[,`:=`(City = str_remove(files, ".csv") %>%  
            str_remove("city_of_") %>% 
            str_replace_all("_", " "))] %>%  
  group_by(City) %>%  
  nest()


future::plan(sequential)
tictoc::tic.clear()
tic()
extracted_city_df = city_df %>%  
  mutate(extracted_data = future_map2(data, City, file_extractr))  %>% 
  unnest(cols = "extracted_data") %>%  
  dplyr::select(extracted_data) %>%  
  data.table() %>% 
  .[,`:=`(extracted_data = extracted_data %>% 
            str_remove_all(", NA"))]
toc()

future::plan(multiprocess)
tictoc::tic.clear()
tic()
extracted_city_df = city_df %>%  
  mutate(extracted_data = future_map2(data, City, file_extractr))  %>% 
  unnest(cols = "extracted_data") %>%  
  dplyr::select(extracted_data) %>%  
  data.table() %>% 
  .[,`:=`(extracted_data = extracted_data %>% 
            str_remove_all(", NA"))]
toc()

extracted_city_df[,2] %>%  
  sample_n(15000) %>% 
  fwrite("./data/extracted_city_df.csv")

extracted_city_df[,2] %>%  
  # sample_n(15000) %>% 
  fwrite("./data/extracted_city_df_full_taste.csv")




file_extractr = function(file_path, location){
  file_path %>% 
    paste(files_ca, ., sep = "/") %>% 
    fread(stringsAsFactors = F) %>%
    mutate_at("NUMBER", as.integer) %>% 
    mutate_all(list(~na_if(.,""))) %>%
    data.table() %>%
    .[!is.na(NUMBER),] %>%
    .[,`:=`(full_address = paste(paste(NUMBER, STREET, sep = " "),
                                 location, "CA", POSTCODE, sep = ", "))] %>%  
    .[,full_address]
}

hey = file_extractr(city_df, "city_of_jackson")

city_df %>% 
  fwrite("./data/city_df")



#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

