library(tidyverse)
library(rvest)

#Get file list from the NOAA site
url <- "https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# Read the HTML page
webpage <- read_html(url)

# Extract all the links from the page
link_ls <- data.frame(link=webpage %>%
  html_nodes("a") %>%
  html_attr("href")) %>%
  filter(str_detect(link,"StormEvents_details-ftp")) 

#file_sel<-link_ls$link[1]

#Download all the files
file_download<-function(file_sel){
  
  links_url=paste0("https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/",
                   file_sel)
  
  destfile <- gsub(".gz","",file_sel)
  
  download.file(links_url, destfile, mode = "wb")
  
  data <- read_csv(gzfile(destfile)) %>%
    mutate(TOR_OTHER_CZ_FIPS=as.character(TOR_OTHER_CZ_FIPS),
           DAMAGE_CROPS=as.character(DAMAGE_CROPS),
           cty_fips=paste0(str_pad(STATEFIPS,width=2,pad="0",
                                   str_pad(CZ_FIPS,width=3,pad="0"))) %>%
    select(BEGIN_YEARMONTH:CZ_NAME,cty_fips,everything()))
  
  data 
}

data<-map_df(link_ls$link,file_download)

#It's a 1.5 GB file, so break it up by type.
storm_types<-unique(data$EVENT_TYPE)


#Write the storm csvs
write_storm<-function(type_sel){
  stormtype<-tolower(type_sel) |>
    gsub(" ", "_", x = _) |>               # Replace spaces with underscores
    gsub("[^A-Za-z0-9_]", "", x = _)       # Remove all non-alphanumeric and non-underscore characters
  
  file_name<-paste0("data/stormevents/noaa_stormevent_",stormtype,"_2025_05.csv")
  write_csv(data %>% filter(EVENT_TYPE==type_sel),file_name)
}

map(storm_types,write_storm)
