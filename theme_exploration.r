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


## Generation data

texas_nuclear_generation <- getEIA(ID = "EBA.TEX-ALL.NG.NUC.HL", key = key) %>% as.data.frame()

texas_gas_generation <- getEIA(ID ="EBA.TEX-ALL.NG.NG.HL" , key = key) %>% as.data.frame()

texas_wind_generation <- getEIA(ID ="EBA.TEX-ALL.NG.WND.HL" , key = key) %>% as.data.frame()

texas_solar_generation <- getEIA(ID ="EBA.TEX-ALL.NG.SUN.HL" , key = key) %>% as.data.frame()

texas_coal_generation <- getEIA(ID ="EBA.TEX-ALL.NG.COL.HL" , key = key) %>% as.data.frame()

texas_hydro_generation <- getEIA(ID ="EBA.TEX-ALL.NG.WAT.HL" , key = key) %>% as.data.frame()

texas_other_generation <- getEIA(ID ="EBA.TEX-ALL.NG.OTH.HL" , key = key) %>% as.data.frame()

texas_generation <- getEIA(ID = "EBA.TEX-ALL.NG.HL", key = key) %>% as.data.frame()


## Demand data
texas_demand <- getEIA(ID = "EBA.TEX-ALL.D.HL", key = key) %>% as.data.frame()


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



retrieveHour <- function(x) {
  #'
  #'Function that extracts hour  from date 
  hourVec <- (strsplit(x, split = ""))[[1]][13:14]
  return (paste(hourVec[1], hourVec[2], ":00", sep = ""))
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
    mutate(dateRaw = rownames(df),
           hour = retrieveHour(dateRaw),
           date = mapply(retrieveDate, dateRaw),
           type = type,
           date = lubridate::ymd(date)) %>% 
    rename("mWh_generated" = oldName) %>% 
    dplyr::select(-dateRaw)
  rownames(df) <- seq(1, nrow(df))
  return (df)
}

generation_data <- processFrameGeneration(texas_nuclear_generation, "nuclear") %>%
  bind_rows(processFrameGeneration(texas_gas_generation, "gas"),
            processFrameGeneration(texas_coal_generation, "coal"),
            processFrameGeneration(texas_solar_generation, "solar"),
            processFrameGeneration(texas_wind_generation, "wind"),
            processFrameGeneration(texas_hydro_generation, "hydro"),
            processFrameGeneration(texas_other_generation, "other")) 


total_generation_data <- 



texas_demand  %<>%
  mutate(dateRaw = rownames(texas_demand),
         hour = retrieveHour(dateRaw),
         date = mapply(retrieveDate, dateRaw),
         date = lubridate::ymd(date)) %>% 
  rename("mWh_demand" = colnames(texas_demand)[1]) %>% 
  dplyr::select(-dateRaw)

rownames(texas_demand) <- seq(1, nrow(texas_demand)) 


demand_hour <- texas_demand %>% 
  group_by(date) %>% 
  summarise(mWh_demand_per_day = sum(mWh_demand))



generation_data_hour


#save(texas_demand, file = "Data/texas_demand.Rdata)
#save(total_generation_data, file = "Data/total_generation_data.Rdata")
load(file = "Data/total_generation_data.Rdata")
load(file = "Data/texas_demand.Rdata")

total_data <- texas_nuclear_generation %>% mutate(dateRaw = rownames(texas_nuclear_generation), type = "nuclear") %>% as_tibble() %>% bind_rows(
  texas_gas_generation %>% mutate(dateRaw = rownames(texas_nuclear_generation), type = "gas"
))


total_data %<>%  mutate(dateRaw = rownames(total_data))


test <- texas_nuclear_generation %>%  
  as.data.frame() 


test %<>% 
  mutate(dateRaw = rownames(test),
         hour = mapply(retrieveHour, dateRaw),
         date = mapply(retrieveDate, dateRaw),
         date = lubridate::ymd(date))






