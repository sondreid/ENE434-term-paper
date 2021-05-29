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
#setwd("C:/Users/sondr/OneDrive/Desktop/ENE434 repo")

## Load preassembled Rdata files

load(file = "Data/texas_data.Rdata")


color_scheme <- c("black", "#EDA63A", "#5093F8") # General color scheme for plots 


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


texas_temperature <- read.csv("Data/texas_temperature.csv") %>%
  dplyr::select(NAME:TMIN) %>% 
  rename(station = NAME,
         date = DATE,
         temp_avg = TAVG,
         temp_min = TMIN,
         temp_max = TMAX) %>% 
  mutate(date = lubridate::ymd(date)) %>% 
  dplyr::select(date, station, temp_avg, temp_min, temp_max)

# Compose new dataframe where three station data is averaged
texas_temperature_avg <- texas_temperature %>% group_by(date) %>% 
  filter(!is.na(temp_avg)) %>% 
  summarise(temp_avg = mean(temp_avg),
            temp_min = mean(temp_min), 
            temp_max = mean(temp_max)) 

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

generation_data <- 
            processFrameGeneration(texas_nuclear_generation, "nuclear") %>%
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



## Create new data frame for demand 

demand_data <- texas_demand  %>%
  mutate(dateRaw = rownames(texas_demand),
         hour = mapply(retrieveHour,dateRaw),
         date = mapply(retrieveDate, dateRaw),
         date = lubridate::ymd(date)) %>% 
  rename("mWh_demand" = colnames(texas_demand)[1]) %>% 
  dplyr::select(-dateRaw)

rownames(demand_data) <- seq(1, nrow(demand_data)) 


############################### DAILY ######################
###########################################################


# Compute daily demand
demand_data_daily <- demand_data %>% 
  group_by(date) %>% 
  summarise(mWh_demand_daily = sum(mWh_demand))


generation_daily <- generation_data %>% 
  group_by(date, type) %>% 
  summarise(mWh_generated = sum(mWh_generated),
            type = type) %>% 
  unique()






### Descriptive statistics ####



#### Descriptive plots  #### 


generation_daily %>%
  filter(type == "total") %>% 
  ggplot(aes(x = date, y = mWh_generated)) +
  geom_point()
  
  




# Monthly generation and demand frames

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



save(demand_data, demand_data_daily, 
     demand_data_monthly, 
     generation_data, 
     generation_daily, 
     generation_monthly,
     texas_temperature,  file = "Data/texas_data.Rdata")

######### Seasonal decomposition of demand ########################################


x13_decomp <- seas(ts(demand_data_monthly %>%  filter(year(date) > 2015) %>%  dplyr::select(mWh_demand_monthly), 
                    start = c("2016"), 
                    frequency = 12)) ## Assuming one cold weather season



x13_decomp <- data.frame(x13_decomp) %>% 
  left_join(demand_data_monthly, by = "date")




facet_cols <- c("seasonal", "seasonaladj", "mWh_demand_monthly")
x13_decomp_pivot <- pivot_longer(x13_decomp, cols = all_of(facet_cols),  names_to = "components", values_to = "demand") %>% 
  dplyr::select(date, components, demand)

x13_decomp_pivot %>% 
  ggplot(aes(x = date, y = demand, col = components)) +
  geom_line() + 
  facet_grid(rows = vars(components),
             scales = "free_y")  +
  scale_colour_manual(values = color_scheme) +
  labs(title =  "Decomposition plots using X13 SEATS method")


x13_decomp %>% ggplot(aes(x = date, y = seasonal)) + geom_line() + labs(x = "Date")


#################################################################
######################## Weather model fitting ##################
#################################################################

# Texas before crisis, e.g before 2021 data

texas_temperature_bf_crisis <- texas_temperature_avg %>% 
  filter(year(date) < 2021)
texas_temperature_crisis    <- texas_temperature_avg %>% 
  filter(year(date) == 2021)

texas_temperature_average_feb <- texas_temperature_avg %>% 
  filter(month(date) == 2) %>% 
  mutate(day = day(date)) %>% 
  group_by(day) %>% 
  summarise(temp_avg = mean(temp_avg),
            month = "02-") %>% 
  mutate(month_day = paste(month, day, sep = "")) %>% 
  select(-month)
    
##### Local smoothed regression useing loess sd 
loess_texas_temperature_bf_crisis <- texas_temperature_bf_crisis %>% 
  mutate(date = as.numeric(date)) %>%  
  msir::loess.sd(x = texas_temperature_bf_crisis$date, y = texas_temperature_bf_crisis$temp_avg, 
           span = 0.01, nsigma = 2.576)


loess_texas_temperature_bf_crisis <- msir::loess.sd(x = as.numeric(texas_temperature_bf_crisis$date), 
                                                   y = texas_temperature_bf_crisis$temp_avg,
                                                   span = 0.05,
                                                   nsigma = 2.576)

loess_texas_temperature_bf_crisis <- msir::loess.sd(x = as.numeric(texas_temperature_average_feb$day), 
                                                    y = texas_temperature_average_feb$temp_avg,
                                                    span = 0.1,
                                                    nsigma = 2.576)



loess_texas_temperature_crisis <- msir::loess.sd(x = as.numeric(texas_temperature_crisis$date), 
                                                 y = texas_temperature_crisis$temp_avg,
                                                 span = 0.1,
                                                 nsigma = 2.576)


## Store results from predicitons as well as confidence intervals
texas_temperature_crisis %<>% 
  mutate(pred_loess  = loess_texas_temperature_crisis$y,
         upper = loess_texas_temperature_crisis$upper,
         lower = loess_texas_temperature_crisis$lower)

texas_temperature_bf_crisis %<>% 
  mutate(pred_loess  = loess_texas_temperature_bf_crisis$y,
         upper = loess_texas_temperature_bf_crisis$upper,
         lower = loess_texas_temperature_bf_crisis$lower)


### Plot general difference


texas_temperature_avg %>% 
  ggplot() +
  geom_line(aes(x = date, y = temp_avg, col = "observed")) +
  geom_line(aes(x = date, y = pred_loess, col = "predicted bf crisis"), 
            data = texas_temperature_bf_crisis %>% filter(year(date) > 2006)) +
  geom_line(aes(x = date, y = pred_loess, col = "predicted crisis"), 
            data = texas_temperature_crisis) +
  scale_colour_manual(values = color_scheme) +
  labs(title = "Loess estimation of weather data")


# Before crisis

texas_temperature_bf_crisis %>% 
  ggplot() +
  geom_line(aes(x = date, y = temp_avg, col = "observed")) +
  geom_line(aes(x = date, y = pred_loess, col = "predicted")) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper), fill = "grey") +
  scale_colour_manual(values = color_scheme) +
  labs(title = "Loess estimation of weather data")


resid_vec <- (texas_temperature_bf_crisis$pred_loess- texas_temperature_bf_crisis$temp_avg)
training_rmse <- RMSE(.resid = resid_vec)




average_lower_temperatures_bf_crisis <- texas_temperature_bf_crisis %>% 
  filter(month(date) == 2) %>% 
  mutate(day = day(date)) %>% 
  group_by(day) %>% 
  summarise(lower_avg = mean(lower),
            temp_avg  = mean(temp_avg),
            upper_avg = mean(upper))



texas_temperature_avg_bf_crisis %>% 
  mutate(pred_loess_before = loess_texas_temperature_bf_crisis$y,
         upper_before      = loess_texas_temperature_bf_crisis$upper,
         lower_before      = loess_texas_temperature_bf_crisis$lower,
         pred_loess_crisis = loss_texas_temperature_cris$y,
         upper_crisis      = loss_texas_temperature_cris$upper,
         lower_crisis      = loss_texas_temperature_cris$lower)




average_lower_temperatures_bf_crisis %>% 
  ggplot() +
  geom_line(aes(x = day, y = temp_avg, col = "pre 2021")) +
  geom_line(aes(x = day, y = temp_avg, col = "2021"),
            data = texas_temperature_crisis %>% filter(month(date) == 2)  %>% mutate(day = day(date))) +
  geom_line(aes(x = day, y = temp_avg, col = "2011"),
            data = texas_temperature_bf_crisis %>% filter(month(date) == 2, year(date) == 2011) %>% mutate(day = day(date))) +
  scale_colour_manual(values = color_scheme) +
  labs(title = "Average weather temperature for february")






##### Evaluating models #######
 








################################################################################
############################ Forecasts testing #################################
################################################################################

## Univariate models



# Dynamic regression model


# 2011 temperatures




