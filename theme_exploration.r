############ Data wrangling ############



### Libraries ###### 
library(EIAdata)
library(fpp3)
library(tidyverse)
library(magrittr)
library(lubridate)


#setwd("G:/Dokumenter/Google drive folder/NHH/Master/ENE434/Term assignment/repo")



key <- "81a7388709d31bb149eb1cc9c7eba736"





## All in local time

texas_nuclear_generation <- getEIA(ID = "EBA.TEX-ALL.NG.NUC.HL", key = key) %>% as.data.frame()

texas_gas_generation <- getEIA(ID ="EBA.TEX-ALL.NG.NG.HL" , key = key) %>% as.data.frame()

texas_generation <- getEIA(ID = "EBA.TEX-ALL.NG.HL", key = key) %>% as.data.frame()

texas_demand <- getEIA(ID = "EBA.TEX-ALL.D.HL", key = key) %>% as.data.frame()



retrieveHour <- function(x) {
  #'
  #'Function that extracts hour  from date 
  hour_vec <- x[13:14]
  return (paste(hour_vec[1], hour_vec[2], ":00", sep = ""))
}


retrieveDate <- function(x) {
  #'
  #'Function that extracts hour  from date 

  date_vec <- (strsplit(x, split = ""))[[1]][2:11]
  string <- ""
  for (symbol in date_vec) {
    if (symbol == ".") next
    string <- paste(string, symbol, sep = "")
  }
  return (string)
}

processFrameGeneration <- function(df, type) {
  #' Readies a generation data frame
  #' @df : dataframe, input generation data frame
  #' @type: string, type of power generation (e.g natural gas or nuclear)
  #' @return: returns a processed data frame
  
  oldName <- colnames(df)[1]
  df %<>% 
    mutate(date_raw = rownames(df),
           hour = retrieveHour(date_raw),
           date = retrieveDate(date_raw),
           type = type,
           date = lubridate::ymd(date)) %>% 
    rename("mWh" = oldName)
  rownames(df) <- seq(1, nrow(df))
  return (df)
}

total_data <- texas_nuclear_generation %>% mutate(date_raw = rownames(texas_nuclear_generation), type = "nuclear") %>% as_tibble() %>% bind_rows(
  texas_gas_generation %>% mutate(date_raw = rownames(texas_nuclear_generation), type = "gas"
))


total_data %<>%  mutate(date_raw = rownames(total_data))


test <- texas_nuclear_generation %>%  
  as.data.frame() 


test %<>% 
  mutate(date_raw = rownames(test),
         hour = sapply(strsplit(date_raw, split = ""),  function(x) retrieveHour(x)[[1]], simplify=FALSE),
         date = mapply(retrieveDate, date_raw),
         date = lubridate::ymd(date))




test["date"] <- rownames(texas_nuclear_generation)

length <- length(strsplit(test$date[1], split = "", fixed = FALSE)[[1]])
for (i in 2:nrow(test)) {
  length_temp = length(strsplit(test$date[1], split = "", fixed = FALSE)[[1]])
  if (length == length_temp) print("TRUE")
}




dates <- rownames(texas_demand)

texas_demand["date"] <- rownames(lubridate::texas_demand)
rownames(texas_demand) <- seq(nrow(texas_demand))
nuclear["date"] <- rownames(nuclear)

nuclear %>% as_tibble()


## Save power data

save( file = "Data/power_data.Rdata")




### Weather data:
# Three stations as of now: Southern rough, Houston, LBJ road

texas_weather <- read.csv("Data/texas_weather.csv") %>%
  dplyr::select(NAME:TMIN) %>% 
  rename(station_name = NAME,
         date = DATE,
         temp_avg = TAVG,
         temp_min = TMIN,
         temp_max = TMAX) %>% 
  mutate(date = lubridate::ymd(date))
