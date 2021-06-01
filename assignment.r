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
library(stats)
library(seasonal)

#setwd("G:/Dokumenter/Google drive folder/NHH/Master/ENE434/Term assignment/repo")
#setwd("C:/Users/sondr/OneDrive/Desktop/ENE434 repo")

## Load preassembled Rdata files

load(file = "Data/texas_data.Rdata")


color_scheme <- c("black", "#EDA63A", "#5093F8",
                  "#34eb89", "#9d45f5",  "#f5455c") # General color scheme for plots 


key <- "81a7388709d31bb149eb1cc9c7eba736"


## All in local time


## Generation data

texas_power_prices      <- getEIA(ID = "ELEC.PRICE.TX-ALL.M", key = key) %>% as.data.frame()

texas_gas_prices        <- getEIA(ID = "NG.N3035TX3.M", key = key) %>%  as.data.frame()

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



# Power prices in texas
texas_power_prices %<>% 
  mutate(date = lubridate::ymd(rownames(texas_power_prices))) %>% 
  rename("cents_kWh" = colnames(texas_power_prices)[1]) %>% 
  dplyr::select(date, cents_kWh)
rownames(texas_power_prices) <- seq(1,nrow(texas_power_prices))
  
# Gas prices

texas_gas_prices %<>%  mutate(date = lubridate::ymd(rownames(texas_gas_prices))) %>% 
  rename("dollar_thousand_cubic" = colnames(texas_gas_prices)[1]) %>% 
  dplyr::select(date, dollar_thousand_cubic)
rownames(texas_gas_prices) <- seq(1,nrow(texas_gas_prices))


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
            temp_min = min(temp_min), 
            temp_max = max(temp_max)) 

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
  summarise(mWh_demand_daily = mean(mWh_demand))


generation_daily <- generation_data %>% 
  group_by(date, type) %>% 
  summarise(mWh_generated = mean(mWh_generated),
            type = type) %>% 
  unique()





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
                                                    span = 0.5,
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

# Split into training

# ARIMA optimal fit

# Lower temperatures


# 2011 minimum temperatures

# Divide into training set and test set using 

# finding whole weeks with 7 day seasonality
#seq(ymd("2019-11-4"), ymd("2020-01-31"), by = "days") %>% length()

# Last energy outtages

demand_data_daily %>% 
  filter(year(date)  == 2011) %>% 
  ggplot() +
  geom_line(aes(date, mWh_demand_daily)) +
  theme_bw()

# Training dataset 
train_demand_temp <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(date > "2020-01-01" & date < "2020-02-01")

test_demand_temp <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(year(date) == 2020 &
         month(date) == 2)

train_demand_temp_2021 <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(date >= "2021-01-01" & date < "2021-02-01")


test_demand_temp_2021 <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(year(date) == 2021 &
         month(date) == 2)


## Stationarity tests

unitroot_kpss(train_demand_temp$mWh_demand_daily) 
tseries::adf.test(train_demand_temp$mWh_demand_daily)

#train_demand_temp$mWh_demand_daily <- difference(train_demand_temp$mWh_demand_daily)
#train_demand_temp %<>% filter(!is.na(mWh_demand_daily))
# Reject stationarity - no need for differencing


# Optimal univariate model
fit_arima_uni <- train_demand_temp %>% 
  as_tsibble(index = date) %>% model(arima_fit = ARIMA(mWh_demand_daily~ 0+ pdq(1,1,1) +PDQ(3,0,2), 
                                                           stepwise = FALSE,
                                                           approximation = FALSE))

#ok model: 0+ pdq(1,1,1) +PDQ(3,0,2)7


# Generate forecasts
fc_arima_uni <- fit_arima_uni %>% forecast(h = 29 )


fc_arima_uni %>% accuracy(test_demand_temp %>% as_tsibble())





fc_arima_uni %>% 
  mutate(date = seq(ymd("2020-02-01"), ymd("2020-02-29"), by = "days")) %>% 
  ggplot() +
  geom_line(aes(x = date, y = .mean, col = "Arima uni")) + 
  geom_line(aes(x = date, y = mWh_demand_daily, col = "train"), data = train_demand_temp) +
  geom_line(aes(x = date, y = mWh_demand_daily, col = "test"), data = test_demand_temp) +
  scale_colour_manual(values = color_scheme) +
  labs(title= "Arima fc") +
  theme_bw()


# Dynamic regression model






fit_arima_temperature_demand <- train_demand_temp %>% 
  as_tsibble(index = date) %>% 
  model(arima_dynamic_temp = ARIMA(mWh_demand_daily ~ temp_avg,
                                   stepwise = FALSE,
                                   approximation = FALSE))



# Forecast

fc_arima_temperature_demand <- fit_arima_temperature_demand %>% 
  forecast(new_data = temperature_2011)


fc_arima_temperature_demand %>% 
  ggplot() +
  geom_line(aes(x = date, y = .mean, col = "Arima uni")) + 
  geom_line(aes(x = date, y = mWh_demand_daily, col = "Observed prior data"), data = train_demand_temp) +
  geom_line(aes(x = date, y = mWh_demand_daily, col = "Test set"), data = test_demand_temp) +
  scale_colour_manual(values = color_scheme) +
  labs(title= "Arima fc") +
  theme_bw()


# Accuracy

# MASE

MASE(.resid = test_demand_temp$mWh_demand_daily-fc_arima_temperature_demand$.mean, .train = train_demand_temp$mWh_demand_daily, .period = 7)



# 2021- can we predict the peak demand? 

# Temperature 2011

temperature_2011_2021 <- texas_temperature_avg %>% 
  filter(date > "2011-01-16" &
           date < "2011-02-14") %>% 
  mutate(date = seq(ymd("2021-02-01"), ymd("2021-02-28"), by = "days")) %>% 
  as_tsibble(index = date)


## Winter months of texas

texas_temperature_avg %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE),
         day = day(date)) %>% 
  group_by(month, year) %>% 
  summarise(avg_temp_month = mean(temp_avg)) %>% 
  ggplot() +
  geom_col(aes(x = year, y = avg_temp_month, col = year)) +
  scale_fill_hp_d(option = "DracoMalfoy") +
  facet_wrap(~month) +
  theme_bw() +
  labs(title = "Mean monthly temperature for the period 2000-2021",
       subtitle = "in degrees celsius",
       ylab = "Monthly average temperature in celsius",
       xlab = "Year")



###### Temperature arima sim ########################################
#################################################################
arima_temperature_2011 <- texas_temperature_avg %>% filter(year(date) == 2011 &
                                                                month(date) == 2) %>% 
  as_tsibble(index = date)

unitroot_kpss(temperature_2011_2021$temp_avg) 

ggtsdisplay(temperature_2011_2021$temp_avg, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Non-differenced temperature data from 2011-01-16:2011-02-14")

fit_arima_temperature <- temperature_2011_2021 %>% model(arima_temperature = ARIMA(temp_avg,
                                                                                    stepwise = FALSE,
                                                                                    approximation = FALSE))

Residuals <- augment(fit_arima_temperature)$.innov
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = paste("Residuals of ", 
                         fit_arima_temperature$arima_temperature, 
                         "model"))


simulate_temperature <- function(fit, days  = 30, startdate = "2020-02-01") {
  #'Generates an optimized arima fit of based on the input dataframe
  #'on 'variable'. Based on the coefficients, AR and SMA orders, return
  #'a simulated ARIMA series
  #'@df : input temperature dataframe
  #'@variable: variable of df which is to be fitted
  
  ar_terms = (fit_arima_temperature %>% 
                 coefficients %>% 
                 filter(str_detect(term,"ar")))$estimate %>% 
    c(.)
  
  ma_terms = (fit_arima_temperature %>% coefficients 
               %>% filter(str_detect(term, "ma")))$estimate %>% 
    c(.)
  arima_sim_model = list(order = fit_arima_temperature$arima_temperature[[1]]$fit$spec[1:3] %>% 
                            t() %>% c(.), 
                          ar = ar_terms, 
                          ma = ma_terms)
  
  sigma = sd(residuals(fit_arima_temperature)$.resid)
  
  sim_arima_temperature = arima.sim(model = arima_sim_model,
                                     n = days,
                                     sd = sigma)
  temperature_sim = data.frame(date = seq(from = ymd(startdate), length.out = days, by = "day"),
                                temp_avg = sim_arima_temperature) %>% 
                                as_tsibble(index = date)
  return(temperature_sim)
}


forecast_sim <- function(ts, fit, days = 30, startdate = "2020-02-01", n = 100) {
  #'
  #'@ts: timeseries or tsibble object of demand
  
  arima_sim_temperature = simulate_temperature(fit, days, startdate)
  
  fit_demand = ts  %>% 
    model(arima_dynamic_demand = ARIMA(mWh_demand_daily ~ temp_avg))
  fc_demand = fit_demand %>% forecast(new_data = arima_sim_temperature)
  return (fc_demand)
  
}

simulate_temperature(fit_arima_temperature) %>% autoplot()

test <- forecast_sim(train_demand_temp_2021,fit_arima_temperature )
test %>% autoplot()


## Peak generation

max_generation <- (generation_daily %>% filter(type == "total") %>% 
  arrange(desc(mWh_generated)))[1,]$mWh_generated

max_generation <- 56000

## Any forecasted daily demand over estimated max_generation 

demand_exceeded <- data.frame("run_nr" = seq(1, 1000), 
                              exceed = FALSE,
                              max_demand = 0)
for(i in 1:12) {
  fc_sim = forecast_sim(ts = train_demand_temp_2021, fit = fit_arima_temperature )
  
  max_demand = max(fc_sim$.mean)
  demand_exceeded[i, "max_demand"] <- max_demand
  if (max_demand > max_generation)  demand_exceeded[i, "exceed"] <- TRUE
}


demand_exceeded %>% 
  summarise(count = sum(exceed, na.rm = TRUE),
            mean  = mean(max_demand),
            max   = max(max_demand)) %>% 
  kbl(caption = "Results ") %>%
  kable_classic(full_width = F, html_font = "Times new roman")




#save(somewhat_extreme_sim, file = "Data/extreme_sim.Rdata")

##################### Using lower percentile temperatures ##########################

lower_percentile_temperature_2020 <- texas_temperature_bf_crisis %>% 
  filter(month(date) == 1 &
         year(date) == 2000) %>% 
  dplyr::select(date, lower) %>% 
  rename(temp_avg = "lower") %>% 
  mutate(date = seq(ymd("2021-01-01"), ymd("2021-01-31"), by = "days")) %>% 
  as_tsibble(index = date)
  



# Four days
temperature_2011_2021 <- texas_temperature_avg %>% 
  filter(date > "2011-01-31" &
           date < "2011-02-6") %>% 
  mutate(date = seq(ymd("2021-02-01"), ymd("2021-02-5"), by = "days")) %>% 
  as_tsibble(index = date)


train_demand_temp_2021 <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(date > "2020-12-15" & date < "2021-01-15") %>% 
  as_tsibble(index = date)
  


fit_2021_arima_temperature_demand <- train_demand_temp_2021   %>% 
  as_tsibble(index = date) %>% 
  model(arima_dynamic_temp_2021 = ARIMA(mWh_demand_daily ~ temp_avg + pdq(0,0,2) + PDQ(1,0,0)),
        arima_dynamic_2021_opt  = ARIMA(mWh_demand_daily ~ temp_avg) )


fc_arima_temperature_demand_2021 <- fit_2021_arima_temperature_demand %>% 
  forecast(new_data = lower_percentile_temperature_2020)








# Plot 2021 forecast



fc_arima_temperature_demand_2021 %>% 
  ggplot() +
  geom_line(aes(x = date, y = .mean, col = .model)) + 
  geom_line(aes(x = date, y = mWh_demand_daily, col = "Observed prior data"), data = train_demand_temp_2021) +
  geom_line(aes(x = date, y = mWh_demand_daily, col = "Test set"), data = test_demand_temp_2021) +
  scale_colour_manual(values = color_scheme) +
  labs(title= "Dynamic regression model February 2021",
       ylab = "Demand in mWh",
       xlab = "Date") +
  theme_bw()






### Evaluation 




