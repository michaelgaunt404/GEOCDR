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

#package library~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
  setwd("~/")
  rstudioapi::getSourceEditorContext()$path %>%
    as.character() %>%
    gsub("R.*","\\1", .) %>%
    path.expand() %>%
    setwd()
}

getwd()
#sourcing script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (!exists("Kept_Extracted_Data")) {
#   print("F I L E    S O U R C E D")
  suppressMessages({
    suppressWarnings({
      source("./R/utility.R")
    })
  })
#   
# } else {
#   print("F I L E S    P R E A S E N T")
# }

#link extraction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~making SF objects
set_key(secret_key$V1[2])

set_verbose(TRUE)

addresses = read.csv("./data/addresses.csv", header = T, stringsAsFactors = F) %>% 
  janitor::remove_constant() %>%  
  mutate(Address = Address %>%  str_trim())

geocoded <- geocode(addresses$Address, autocomplete = FALSE) %>% 
  mutate(color = is.na(street))



library(gridExtra)
png(filename = "output.png", width=480,height=480,bg = "white")
grid.table(c_df)
dev.off()
output %>%  print('output.png')
tabyl(geocoded, color) %>% 
  adorn_pct_formatting(digits = 0, affix_sign = TRUE) %>%  
  grid.table()

output.png
message(geocoded)

tmp = c("UC Berkeley", "Albany Hill", "1032 Cornell, Albany, United States") %>%  
  data.table()

"S State St & W Madison St, Chicago, IL 60603, United States" %>% 
  geocode(autocomplete = FALSE)

evaluate::evaluate(geocoded)
geocoded %>%  
  tm_shape() + 
  tm_dots() 

png()


practice = poi %>%  
  data.table() %>%  
  mutate(geometry = geometry %>%  
           as.character() %>%  
           str_remove_all("[:alpha:]") %>% 
           str_remove_all("\\(") %>% 
           str_remove_all("\\)")) %>% 
  separate(geometry , sep = ",", into = c("Long", "Lat"))

herro = st_as_sf(practice, coords = c("Long", "Lat"), crs = st_crs(4326)) 

uhohhotdog1 = herro %>% 
  reverse_geocode(results = 1, landmarks = FALSE, url_only = FALSE) %>%
  mutate(description = str_glue("<strong>{label}</strong><br>
                                            Parent Point ID: {id}<br>
                                            Sub-Point ID: {rank}<br>
                                            Dist. to input coord.: {distance}<br>
                                            Geocode Level: {level}"))

leaflet() %>%  
  addTiles() %>%  
  addCircles(data = uhohhotdog1, layerId = "HHHH", color = "#CC7E85", popup = ~description) %>%  
  # addCircleMarkers(data = uhohhotdog1, layerId = "ssss") %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("HHHH", "ssss"),
    options = layersControlOptions(collapsed = FALSE)
  )
tmap_mode("view")
uhohhotdog1 %>%  
  tm_shape() + 
  tm_dots() %>% 
  tmap_leaflet()









herro %>%  
  tm_shape() +
  tm_dots()  
routeee = route(poi[1:2, ], poi[3:4, ], mode = "car") 

routeee %>% 
  tm_shape() +  
  tm_lines(col = "distance", 
           style = "fixed") +
  tm_shape(poi[1:4, ]) +  
  tm_dots()
rbind(c(2500, 21.66, "reg"),
      
c(12000, 128.91, "reg"),
c(75000, 887.67, "reg"),
c(75000, 708.03, "FURRR")) %>%  
  data.table() %>%  
  ggplot() +
  geom_point(aes(V2, V1, color = V3))

75000/887
887/60
75000/708
708/60



yolo = bind_rows(expand.grid(Lat = seq(47.624434, 47.605077, length.out = 50), 
                             Long = seq(-122.337948, -122.284596, length.out = 50)),
                 expand.grid(Lat = seq(34.084487, 34.026098, length.out = 100), 
                             Long = seq(-118.404456, -118.263198, length.out = 100))) %>% 
  data.table(oKJNher = c("njmankjew", "skjn")) %>% fwrite("./data/coord.csv")
  rename_all(tolower) %>% 
  st_as_sf(., coords = c("Long", "Lat"), crs = st_crs(4326))

head(yolo) %>%
  st_as_sf(., coords = c("long", "lat"), crs = st_crs(4326)) 

library(tictoc)
tic.clear()
tic()
bigboi = yolo  %>% 
  reverse_geocode(., results = 1, landmarks = FALSE, url_only = FALSE)
toc()  
  

bigboi %>%  
  sample_n(10) %>%  tm_shape() + tm_dots(col = "blue",
                                         alpha = .7, size = 1, scale = .5, shape = 21, 
                                         border.lwd = 1, border.col = "black", border.alpha = 1)




data = read.csv('./data/extracted_city_df_full_taste.csv', stringsAsFactors = F) %>%  
  data.table() 

  
data_500_test = sample_n(data, 500) %>%  
  mutate(group = ((as.numeric(rownames(.)))%/%25) %>%  
  as.factor()) %>%  
  group_by(group) %>%  
  nest()  

geocode(head(data_500_test_NO)[,1] %>%  as.character())

hey = data_500_test %>%  
    mutate(geocoded = map(data, function(x) x$extracted_data %>%  geocode(autocomplete = F)))


final = hey %>%  unnest(cols = c(data, geocoded)) %>%  
  st_as_sf() 

data$extracted_data %>%  str_remove("^.,")

#speed test ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_75k_nest = sample_n(data, 75000) %>%  
  mutate(group = ((as.numeric(rownames(.)))%/%5000) %>%  
           as.factor()) %>%  
  group_by(group) %>%  
  nest()  
set_key("BLhCb55CN6JC61TBVrVbcXqEG7YK5dMJDSj475c2jNo")
data_75k = sample_n(data, 75000)
data_75k %>%  str()

plan(sequential)
tic.clearlog()
tic()
speed_test_75k = geocode(data_75k[,1], autocomplete = F)
toc()

plan(multiprocess)
tic.clearlog()
tic()
speed_test_75k_nest = data_75k_nest %>%  
  mutate(geocoded = future_map(data, function(x) x$extracted_data %>%  
                          geocode(autocomplete = F), .progress = T))

speed_test_75k_nest_final = speed_test_75k_nest %>%  unnest(cols = c(data, geocoded)) %>%  
  st_as_sf()
toc()







install.packages("pacman")
install.packages("pacman")
install.packages("pacman")

pacman::p_load(httr, tidyverse, jsonlite, data.table, magrittr)



https://batch.geocoder.ls.hereapi.com/6.2/jobs?gen=8&action=run&mailto=%3Cmy_email%3E&header=true&indelim=%7C&outdelim=%7C&outcols=displayLatitude%2CdisplayLongitude%2ClocationLabel%2ChouseNumber%2Cstreet%2Cdistrict%2Ccity
%2CpostalCode%2Ccounty%2Cstate%2Ccountry&outputCombined=false&apiKey=H6XyiCT0w1t9GgTjqhRXxDMrVj9h78ya3NuxlwM7XUs

https://geocoder.ls.hereapi.com
/6.2/
  
  
url = "https://geocoder.ls.hereapi.com/6.2/geocode.json?apiKey=zzdfbQNBJyDwFxbu_Q2yeNhK8nlOpcJv0iBsCSqhn4I&searchtext=425+W+Randolph+Chicago"
url = "https://geocoder.ls.hereapi.com/6.2/geocode.json?apiKey=zzdfbQNBJyDwFxbu_Q2yeNhK8nlOpcJv0iBsCSqhn4I&searchtext=425+W+Randolph+Chicago"

url = "https://geocoder.ls.hereapi.com/6.2/geocode.json?apiKey=zzdfbQNBJyDwFxbu_Q2yeNhK8nlOpcJv0iBsCSqhn4I&searchtext=425+W+Randolph+Chicago&searchtext=1032+Cornell+Albany"


raw_result = httr::GET(url)

hello = httr::content(raw_result) %>% fromJSON()

httr::content(raw_result, as = "text") %>% fromJSON() %>% glimpse()
tmp = httr::content(raw_result, as = "text") %>% fromJSON()
tmpdf = tmp$Response$View$Result %>%  data.table()
tmpdf$Location
tmpdf %>%  data.frame()


tmp = httr::content(raw_result, as = "text") %>%
  jsonlite::fromJSON() 

tmp$Response$View$Result

tmp$Response$View$Result[[1]]$Location$DisplayPosition %>%  glimpse()

tmp$Response$View$Result[[1]]$Location$NavigationPosition %>%  glimpse()

tmp$Response$View[[1]]$Result[[1]]$Location$DisplayPosition$Latitude


https://batch.geocoder.ls.hereapi.com/6.2/jobs?gen=8&action=run&mailto=%3Cmy_email%3E&header=true&indelim=%7C&outdelim=%7C&
  outcols=displayLatitude%2CdisplayLongitude%2ClocationLabel%2ChouseNumber%2Cstreet%2Cdistrict%2Ccity%2CpostalCode%2Ccounty%2Cstate%2Ccountry&outputCombined=false&apiKey=H6XyiCT0w1t9GgTjqhRXxDMrVj9h78ya3NuxlwM7XUs




hello = httr::GET(url_batch)
hello$content

httr::POST()

b2 <- "http://httpbin.org/post"

getwd()
GET()
POST(url, body = list(x = upload_file("mypath.txt")))



uploadfiled = httr::upload_file(path = "zz_pipe.txt")


# POST(url = 
#        body = list(y = uploadfiled)
#      
#      system()

firstpost = POST(url= "https://batch.geocoder.ls.hereapi.com/6.2/jobs",
                 config = list(action='run',
                              header='true',
                              inDelim='\t',
                              outDelim=',',
                              outCols='recId,latitude,longitude,locationLabel',
                              outputcombined='true',
                              mailTo='mike.gaunt.404@gmail.com'),
                 body = list(x = upload_file("./data/12k_address_short.txt")))
?&apiKey=BLhCb55CN6JC61TBVrVbcXqEG7YK5dMJDSj475c2jNo&action=run&header=true&inDelim=\t&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=mike.gaunt.404@gmail.com 

query = list(action='run',
             header='true',
             inDelim='\t',
             outDelim=',',
             outCols='recId,latitude,longitude,locationLabel',
             outputcombined='true',
             mailTo='mike.gaunt.404@gmail.com')


system("")
    
raw_result = GET(url = "https://batch.geocoder.ls.hereapi.com/6.2/jobs/6ZAOD4a0bveCg3kpQ4pGxEZo1Z03N192/result?apiKey=BLhCb55CN6JC61TBVrVbcXqEG7YK5dMJDSj475c2jNo")
tmp = httr::content(raw_result, as = "text") %>%
  jsonlite::fromJSON() 
raw_result %>% str()
tmp$Response$View$Result

tmp$Response$View$Result[[1]]$Location$DisplayPosition %>%  glimpse()

tmp$Response$View$Result[[1]]$Location$NavigationPosition %>%  glimpse()

tmp$Response$View[[1]]$Result[[1]]$Location$DisplayPosition$Latitude
unzip(raw_result$content)

temp <- tempfile()
download.file(url, temp)
carsData <- read.table(unz(temp, "a1.dat"))

unzip(temp)
temp
httr::POST()

getwd()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data = "./data/12k_address_short.txt"
data = "./data/12k_address_fulladdresses_short.txt"
data = "./data/overwrite_testtt.txt"
data = "./data/12k_address_short_commadd.csv"
key = 'BLhCb55CN6JC61TBVrVbcXqEG7YK5dMJDSj475c2jNo'
email = "mike.gaunt.404@gmail.com"

#write request cURL that will be sent out via system OS
#ISSUE MAC USERS MAY MESS THIS UP 
request_curl = paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @', 
                   data, 
                   ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=', 
                   key, 
                   '&action=run&header=true&inDelim=|&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=', 
                   email)


'Content-Type: text/csv'

request_curl_csv = paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @', 
                      data, 
                      ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=', 
                      key, 
                      '&action=run&header=true&inDelim=,&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=', 
                      email)

paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @', 
       data, 
       ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=', 
       key, 
       '&action=run&header=true&inDelim=|&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=', 
       email) %>%  
  system(intern = T)

batchout = system(request_curl, intern = T)

#want to detect if an error has occured and tell user what it is
#would like this to trigger a pop up if possible
if (str_detect(tail(batchout, 1), "ns2:Error")) {
  error_msg = tail(batchout, 1) %>%  
    gsub(".*<Details>", "\\1",.) %>%  
    gsub("</Details>.*", "\\1",.) %>%  
    str_trim()
  message(paste("E R R O R:: ", error_msg))
  
} else {
  batchout[4] %>%  
    gsub(".*<Status>", "\\1",.) %>%  
    gsub("</Status>.*", "\\1",.) %>%  
    paste0("Data pakcage ", .) %>% 
    message()
  
  job_code = tail(batchout, 1) %>%  
    gsub(".*<RequestId>", "\\1",.) %>%  
    gsub("</RequestId>.*", "\\1",.) %>%  
    str_trim()
  
  status_request_url = paste0("https://batch.geocoder.ls.hereapi.com/6.2/jobs/",
                              job_code,
                              "?=status&apiKey=", 
                              key)
  
  i = 0
  while(TRUE){
    print(i)
    if (i > 120){
      break 
    }
    
    status_info = GET(url = status_request_url) %>% 
      content(type = "text") %>%  
      jsonlite::fromJSON()
    
    print(status_info$Response$Status)
    
    if (status_info$Response$Status == "completed"){
      break 
    }

    Sys.sleep(time = 2) #Time in seconds
    i = i + 2
  }
}

#grabs job status and puts into kable table
# this should not be ran if the status code isnt completed




status_info = GET(url = status_request_url) %>% 
  content(type = "text") %>%  
  jsonlite::fromJSON()

data.table(`Job Details` = c("Status", "Duration", "No. Records Sent", "Success"), 
           Metrics = c(status_info$Response$Status, 
                       paste(ymd_hms(status_info$Response$JobFinished) - ymd_hms(status_info$Response$JobStarted), " seconds"), 
                       status_info$Response$TotalCount, 
                       paste0(100*round(status_info$Response$SuccessCount/status_info$Response$TotalCount,3), " %") )) %>%  
  kable(escape = F, caption = "405LB") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = F, font_size = 11) 

#once completed data is then pulled from the API 
#zip file is saved to relative local folder 
#unzipped and data mapped
data_request_curl = paste0("curl https://batch.geocoder.ls.hereapi.com/6.2/jobs/",
                          job_code,
                          "/result?apiKey=", 
                          key, 
                          " --output ./output/geocode.zip")
system(data_request_curl)
# download.file(data_request_url, "./output/geocoded_data.zip") 

unzipped = unzip("./output/geocode.zip", exdir = "./output", overwrite = TRUE) 
geocode_data = fread(unzipped)
geocode_data %>%  
  na.omit() %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = st_crs(4326)) %>% 
  mapview()

key = "hh"
key = 'BLhCb55CN6JC61TBVrVbcXqEG7YK5dMJDSj475c2jNo'
batchout = paste0(cURL = 'curl -X POST -H "Content-Type: text/plain" --data-binary @',
                  "./data/overwrite_testtt.txt",
                  ' "https://batch.geocoder.ls.hereapi.com/6.2/jobs?&apiKey=',
                  key,
                  '&action=run&header=true&inDelim=|&outDelim=,&outCols=recId,latitude,longitude,locationLabel&outputcombined=true&mailTo=',
                  email) %>%
  system(intern = T)


batchoutNokey = batchout 
batchoutnogooddata = batchout

batchoutgood = batchout

batchoutNokey %>%  tail(1) %>%  str_detect("Request")
batchoutnogooddata %>%  tail(1)  %>%  str_detect("Request")
!str_detect(tail(batchoutgood, 1), "Request")

addresses %>%
  set_colnames("searchText") %>%
  mutate(recId = rownames(.) %>%
           as.numeric()) %>%
  write.table("./data/overwrite_testtt.txt", 
              row.names = F, 
              quote = F, 
              sep = "|")


















