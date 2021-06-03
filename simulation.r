####################### Simulation script ################################


# Join demand datasets and temperature dataset
demand_temp_2021 <- demand_data_daily %>% 
  left_join(texas_temperature_avg, by = "date") %>% 
  filter(date >= "2020-12-01" & date < "2021-02-01") %>% 
  as_tsibble(index = date)


arima_temperature_2011 <- texas_temperature_avg %>% 
  filter(date > "2011-01-16" &
           date < "2011-02-14") %>% 
  mutate(date = seq(ymd("2021-02-01"), ymd("2021-02-28"), by = "days")) %>% 
  as_tsibble(index = date)



unitroot_kpss(arima_temperature_2011$temp_avg) 

ggtsdisplay(arima_temperature_2011$temp_avg, 
            plot.type = "partial", 
            lag.max = 24, 
            theme = theme_bw(),
            main = "Non-differenced temperature data from 2011-01-16:2011-02-14")


############ Fit arima series on 2011 weather data#################
fit_arima_temperature <- arima_temperature_2011 %>% 
  model(arima_temperature = ARIMA(temp_avg,
                                  stepwise = FALSE,
                                  approximation = FALSE))


# Flytt til dynamic regression
Residuals <- augment(fit_arima_temperature)$.innov
ggtsdisplay(Residuals, 
            plot.type = "histogram", 
            lag.max = 24, 
            theme = theme_bw(),
            main = paste("Residuals of ", 
                         fit_arima_temperature$arima_temperature, 
                         "model"))


arima_simulation <- function(fit,
                             days  = 30, 
                             startdate = "2021-02-01") 
  {
  #'Generates an optimized arima fit of based on the input dataframe
  #'on 'variable'. Based on the coefficients, AR and SMA orders, return
  #'a simulated ARIMA series
  #'@df : input temperature dataframe
  
  ar_terms = (fit %>% 
                coefficients %>% 
                dplyr::filter(stringr::str_detect(term,"ar")))$estimate %>% 
    c(.)
  
  ma_terms = (fit %>% 
                coefficients  %>% 
                dplyr::filter(stringr::str_detect(term, "ma")))$estimate %>% 
    c(.)
  
  constant_term = (fit %>% coefficients %>% 
                     dplyr::filter(stringr::str_detect(term, "constant")))$estimate %>% 
    c(.)
  if (identical(constant_term, numeric(0))) constant_term <- 0
  if (identical(ma_terms, numeric(0))) {
    arima_sim_model = list(order = fit[[1]][[1]]$fit$spec[1:3] %>% 
                             t() %>% c(.), 
                           ar = ar_terms)
  }
  else {
    arima_sim_model = list(order = fit[[1]][[1]]$fit$spec[1:3] %>% 
                             t() %>% c(.), 
                           ar = ar_terms,
                           ma = ma_terms)
  }
  
  sigma = sd(residuals(fit)$.resid)
  
  sim_arima = arima.sim(model = arima_sim_model,
                                    n = days,
                                    n.start = 5,
                                    sd = sigma)
  
  return( 
    data.frame(date = seq(from = ymd(startdate), length.out = length(sim_arima), by = "day"),
              variable = sim_arima ) %>% 
      mutate(variable = variable + constant_term + rnorm(n = 1, mean = 0, sd(sim_arima)))  %>% 
      as_tsibble(index = date)) 
}




arima_simulation(fit_arima_temperature) %>% plot()

forecast_sim <- function(ts, 
                         fit, 
                         days = 30, 
                         startdate = "2020-02-01") 
  {
  #' Function that performs a dynamic arima forecast based on a fitted 
  #' arima series. 
  #'@ts: timeseries or tsibble object of demand
  
  arima_sim = arima_simulation(fit, days, startdate)
  fit_demand = ts  %>% 
    model(arima_dynamic_demand = ARIMA(mWh_demand_daily~variable,
                                       stepwise = TRUE,
                                       approximation = FALSE))
  fc_demand = fit_demand %>% forecast(new_data = arima_sim)
  return (fc_demand)
  
}





demand_temp_2021 %<>% 
  rename("variable" = temp_avg)


test <- forecast_sim(demand_temp_2021,fit_arima_temperature, days = 30, startdate = "2020-02-01")
test %>% autoplot()


## Peak generation

max_generation <- (generation_daily %>% dplyr::filter(type == "total") %>% 
                     arrange(desc(mWh_generated)))[1,]$mWh_generated



## Any forecasted daily demand over estimated max_generation 

demand_exceeded <- data.frame("run_nr" = seq(1, 100), 
                              exceed = FALSE,
                              max_demand = 0)
for(i in 1:100) {
  fc_sim = forecast_sim(ts = demand_temp_2021, fit = fit_arima_temperature )
  
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
  kbl(caption = "Forecasted simulated demand on 2011 temperatures ", align = "c") %>%
  kable_paper(full_width = F) %>%
  footnote(general  = "Based on 1000 simulations") %>% 
  kable_classic(full_width = F, html_font = "Times new roman")




## Might be reasonable to assume that demand would have kept high levels

#### FIT GARCH simulation model

nuclear_data <- generation_daily %>% 
  dplyr::filter(type == "nuclear" &
         (year(date) == 2021 || year(date ==2011)) & 
           month(date) == 2)


nuclear_data <- generation_daily %>% 
  filter(type == "nuclear" &
         date  >= "2021-01-15" &
         date  < "2021-03-01")


gas_data <- generation_daily %>% 
  filter(type == "gas" &
           date  >= "2021-01-15" &
           date  < "2021-03-01")



coal_data <- generation_daily %>% 
  filter(type == "coal" &
           date  >= "2021-01-15" &
           date  < "2021-03-01")


## ARIMA fits"" 

# Nuclear ###
fit_nuclear_arima <- nuclear_data %>% 
  as_tsibble(index = date) %>% 
  model(nuclear_arima = ARIMA(mWh_generated,
                              stepwise = FALSE,
                              approximation = FALSE))


# Gas ###
fit_gas_arima <- gas_data %>% 
  as_tsibble(index = date) %>% 
  model(gas_arima = ARIMA(mWh_generated,
                              stepwise = FALSE,
                              approximation = FALSE))



## Coal

# Gas ###
fit_coal_arima <- coal_data %>% 
  as_tsibble(index = date) %>% 
  model(coal_arima = ARIMA(mWh_generated,
                              stepwise = FALSE,
                              approximation = FALSE))




arma_order <- fit_nuclear_arima[[1]][[1]]$fit$spec[1:3] %>% t(.) %>% c(.)
nuclear_data %<>% ts()

fit_nuclear_garch = garchFit(~arma(0,3) + garch(1,1), data = nuclear_data$mWh_generated, trace=F)

garchFit(~arma(1,1) + garch(1,1), data = cons_ts["returns"], trace = F)

garchSpec <- garchSpec(model = list(mu = 5124.0109630 ,
                              ar = c()
                              ma = c(0.5934401 ,1,0.2694950 ),
                              omega = 4.4782909 ,
                              alpha = 1,
                              beta = 0.3129629 ))



nuclear_garch_spec <- garchSpec(model = list(mu = 5124.0109630,
                                             ma = c(0.5934401 ,1 ,0.2694950),
                                             beta = 0.3129629,
                                             omega = 4.4782909))

sim_nuclear_garch <- garchSim(nuclear_garch_spec@model, n = 30)
sim_nuclear_garch %>% plot()


spec = garchSpec(model = list(alpha = c(0.2, 0.4), beta = 0))
garchSim(spec, n = 10)




## ugarch sim

varModel <- list(model = "sGARCH", garchOrder = garchOrder)


spec_ugarch <-ugarchspec(
            mean.model <- list((armaOrder = arma_order), method = "CSS"),
            variance.model= list(garchOrder=c(1,1)))

fit_ugarch <- ugarchfit(data = nuclear_data$mWh_generated, spec = spec_ugarch, out.sample = 20)

sim_ugarch <- ugarchsim(fit_ugarch, n.sim = 28, n.start = 0, startMethod = "sample", m.sim = 1)





garch_sim <- function(fit, df, days = 20, startdate = "2021-02-01") {
  #'
  #'@fit: fitted arima model
  #'@days: 
  arma_order <- fit[[1]][[1]]$fit$spec[1:3] %>% dplyr::select(p,q) %>% 
    t(.) %>% c(.)
  spec_ugarch = ugarchspec(
    mean.model <- list(armaOrder = arma_order,  include.mean = TRUE),
    variance.model= list(model="sGARCH", garchOrder=c(1,1)))
  
  fit_ugarch = ugarchfit(data = df$mWh_generated, spec = spec_ugarch, out.sample = days)
  return(tsibble(mWh_generated = ugarchsim(fit_ugarch, 
                                          n.sim = days, 
                                          n.start = 0, 
                                          startMethod = "sample", 
                                          m.sim = 1)@simulation$seriesSim,
                date = seq(ymd(startdate), length.out = days, by = "day")))
}


## Nuclear sim
garch_sim(fit_nuclear_arima, nuclear_data) %>% 
  autoplot()


# Gas sim
garch_sim(fit_gas_arima, gas_data) %>% 
  autoplot()

# Coal sim
garch_sim(fit_coal_arima, coal_data) %>% 
  autoplot()


ugarchsim(fit_ugarch, n.sim = 28, n.start = 0, startMethod = "sample", m.sim = 1)@simulation$seriesSim %>% tibble() %>% 
  mutate(date = seq(ymd("2021-02-01"), length.out = 28, by = "day")) %>% 
  as_tsibble() %>% 
  autoplot()



## 



# Assume observed demand values
sim_all_sources <- function(subtract_from = "gas", replace = TRUE, sim_length = 45, fc_length = 25, startdate = "2021-01-01") {
  #' 
  generation_types <- generation_daily %>%  
    filter(type != "total") %>% 
    group_by(type) %>% 
    summarise(type = type) %>% unique() 
  
  generation_types <-  generation_types$type %>% c(.)
  
  simulation_period = seq(ymd(startdate), length.out = days, by = "day")
  
  simulation_output = tibble(date = seq(ymd(startdate), length.out = fc_length, by = "day"),
         mWh_generated  = 0 )
  
  for (generation_type in generation_types) {
    print(generation_type)
    type_df = generation_daily %>% 
      filter(type == !!generation_type &
             date %in% simulation_period) %>% 
      as_tsibble(index = date)
    
    arima_fit = type_df %>% model(arima_fit = ARIMA(mWh_generated,
                                                       stepwise = FALSE,
                                                       approximation = FALSE))
    print(arima_fit %>% coefficients())
    print(type_df)
    type_sim = garch_sim(fit = arima_fit, df = type_df, days = fc_length, startdate = startdate)
    simulation_output %<>% bind_rows(tibble(date = seq(ymd(startdate), length.out = fc_length, by = "day"),
                                            type = generation_type,
                                            mWh_generated = type_sim@simulation$seriesSim %>% tibble()))
    
    
  }
  return (simulation_output)
  
}

simulation_period <- seq(ymd("2021-01-15"), length.out = 45, by = "day")

sim_type <- function(generation_type, sim_period, fc_length = 15, startdate = "2021-02-01") {
  type_df = generation_daily %>% 
    filter(type == !!generation_type &
             date %in% simulation_period) %>% 
    as_tsibble(index = date)
  arima_fit = type_df %>% model(arima_fit = ARIMA(mWh_generated,
                                                  stepwise = FALSE,
                                                  approximation = FALSE))
  return(
    garch_sim(fit = arima_fit, df = type_df, days = fc_length, startdate = startdate)
  )
}
sim_type("gas", sim_period = simulation_period) %>% autoplot()


sim_all_sources(fc_length  = 10)



type_df = generation_daily[generation_daily$type == generation_type 
                           & generation_daily$date %in% simulation_period, ,] 





# Check demand surpass




## Try out arima simulation

arima_simulation(fit_nuclear_arima, days = 28, startdate = "2021-02-01")

arima_simulation(fit_nuclear_arima) %>% autoplot()




## Add component and subtract for natural gas


