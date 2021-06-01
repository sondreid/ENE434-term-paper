####################### Simulation script ################################


# Join demand datasets and temperature dataset
demand_temp_2021 <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(date >= "2020-12-01" & date < "2021-02-01") %>% 
  as_tsibble(index = date)




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



## Any forecasted daily demand over estimated max_generation 

demand_exceeded <- data.frame("run_nr" = seq(1, 100), 
                              exceed = FALSE,
                              max_demand = 0)
for(i in 1:100) {
  fc_sim = forecast_sim(ts = train_demand_temp_2021, fit = fit_arima_temperature )
  
  max_demand = max(fc_sim$.mean)
  demand_exceeded[i, "max_demand"] <- max_demand
  if (max_demand > max_generation)  demand_exceeded[i, "exceed"] <- TRUE
}


demand_exceeded


demand_exceeded %>% 
  summarise(count = sum(exceed, na.rm = TRUE), 
            percentage = paste((count/n())*100, "%"),
            mean  = mean(max_demand),
            max   = max(max_demand)) %>% 
  kbl(caption = "Results ") %>%
  kable_classic(full_width = F, html_font = "Times new roman")




## Might be reasonable to assume that demand would have kept high levels

#### FIT GARCH simulation model

nuclear_data <- generation_daily %>% 
  filter(type == "nuclear" &
         date > "2020-12-01" & date < "2021-03-01")



fit_nuclear_arima <- nuclear_data %>% 
  as_tsibble(index = date) %>% 
  model(nuclear_arima = ARIMA(mWh_generated,
                              stepwise = FALSE,
                              approximation = FALSE))

ugarchspec(mean.model = (armaOrder = )
  variance.model = )



