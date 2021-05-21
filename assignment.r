################################################################################
############################ Data wrangling######################################
################################################################################

## Install x13 binary files if necessary
#install.packages("seasonal", type = "source") 



### Libraries ###### 
library(EIAdata)
library(fpp3)
library(tidyverse)
library(magrittr)
library(lubridate)
library(x13binary)
library(seasonal)

#setwd("G:/Dokumenter/Google drive folder/NHH/Master/ENE434/Term assignment/repo")

## Load preassembled Rdata files

load(file = "Data/demand_data.Rdata")
load(file = "Data/generation_data.Rdata")




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
           hour = mapply(retrieveHour,dateRaw),
           date = mapply(retrieveDate, dateRaw),
           type = type,
           date = lubridate::ymd(date)) %>% 
    rename("mWh_generated" = oldName) %>% 
    dplyr::select(-dateRaw)
  rownames(df) <- seq(1, nrow(df))
  return (df)
}




## Generation data for all sources

generation_data <- processFrameGeneration(texas_nuclear_generation, "nuclear") %>%
  bind_rows(processFrameGeneration(texas_gas_generation, "gas"),
            processFrameGeneration(texas_coal_generation, "coal"),
            processFrameGeneration(texas_solar_generation, "solar"),
            processFrameGeneration(texas_wind_generation, "wind"),
            processFrameGeneration(texas_hydro_generation, "hydro"),
            processFrameGeneration(texas_other_generation, "other")) 

total_generation_data <- generation_data %>% 
  group_by(hour, date) %>% 
  summarise(type = "total",
            date = date,
            hour =  hour,
            mWh_generated = sum(mWh_generated)) %>% 
  unique()

## Bind rows into generation data 

generation_data %<>% bind_rows(total_generation_data)



## Calculate

demand_data <- texas_demand  %>%
  mutate(dateRaw = rownames(texas_demand),
         hour = mapply(retrieveHour,dateRaw),
         date = mapply(retrieveDate, dateRaw),
         date = lubridate::ymd(date)) %>% 
  rename("mWh_demand" = colnames(texas_demand)[1]) %>% 
  dplyr::select(-dateRaw)

rownames(demand_data) <- seq(1, nrow(demand_data)) 


demand_data_daily <- demand_data %>% 
  group_by(date) %>% 
  summarise(mWh_demand_daily = sum(mWh_demand))


generation_daily <- generation_data %>% 
  group_by(date, type) %>% 
  summarise(mWh_generated = sum(mWh_generated),
            type = type) %>% 
  unique()


save(demand_data, demand_data_daily,  file = "Data/demand_data.Rdata")
save(generation_data, generation_daily, file = "Data/generation_data.Rdata")


### Descriptive statistics ####



#### Descriptive plots  #### 


generation_daily %>%
  filter(type == "total") %>% 
  ggplot(aes(x = date, y = mWh_generated)) +
  geom_point()
  
  



generation_monthly <- generation_daily %>% 
  mutate(month = lubridate::month(date),
         year  = lubridate::year(date)) %>% 
  group_by(month, year, type) %>% 
  summarise(mWh_generated = sum(mWh_generated)) %>% 
  unique() %>% 
  mutate(date = lubridate::ymd(paste(year, month, "-01"))) %>% 
  dplyr::select(-year,-month)
  

demand_data_monthly <- demand_data_daily %>% 
  mutate(month = lubridate::month(date),
         year  = lubridate::year(date)) %>% 
  group_by(month, year) %>% 
  summarise(mWh_demand_monthly = sum(mWh_demand_daily)) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(date = lubridate::ymd(paste(year, month, "-01"))) %>% 
  dplyr::select(-year,-month)


## Training data (Monthly)
generation_total_train <- generation_monthly %>% filter(year(date) < 2019)


# Seasonal decomposition 


x13_decomp <- seas(ts(demand_data_monthly %>%  filter(year(date) > 2015) %>%   dplyr::select(mWh_demand_monthly), 
                    start = c("2016"), 
                    frequency = 12)) ## Assuming one cold weather season

x13_decomp <- data.frame(x13_decomp) %>% 
  left_join(demand_data_monthly, by = "date")

x13_decomp %>% ggplot(aes(x = date, y = seasonal)) + geom_line()





### 




################################################################################
############################ Forecasts testing #################################
################################################################################

## Univariate models






