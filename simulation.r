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
                             start_date = "2021-02-01") 
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
    data.frame(date = seq(from = ymd(start_date), length.out = length(sim_arima), by = "day"),
              variable = sim_arima ) %>% 
      mutate(variable = variable + constant_term + rnorm(n = 1, mean = 0, sd(sim_arima)))  %>% 
      as_tsibble(index = date)) 
}




arima_simulation(fit_arima_temperature) %>% plot()

forecast_sim <- function(ts, 
                         fit, 
                         days = 30, 
                         start_date = "2020-02-01") 
  {
  #' Function that performs a dynamic arima forecast based on a fitted 
  #' arima series. 
  #'@ts: timeseries or tsibble object of demand
  
  arima_sim = arima_simulation(fit, days, start_date)
  fit_demand = ts  %>% 
    model(arima_dynamic_demand = ARIMA(mWh_demand_daily~variable,
                                       stepwise = TRUE,
                                       approximation = FALSE))
  fc_demand = fit_demand %>% forecast(new_data = arima_sim)
  return (fc_demand)
  
}





demand_temp_2021 %<>% 
  rename("variable" = temp_avg)


test <- forecast_sim(demand_temp_2021,fit_arima_temperature, days = 30, start_date = "2020-02-01")
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




garch_sim <- function(fit, df, fc_length = 20, start_date = "2021-02-01") {
  #'
  #'@fit: fitted arima model
  #'@fc_length: 
  arma_order <- fit[[1]][[1]]$fit$spec[1:3] %>% dplyr::select(p,q) %>% 
    t(.) %>% c(.)
  spec_ugarch = ugarchspec(
    mean.model <- list(armaOrder = arma_order,  include.mean = TRUE),
    variance.model= list(model="sGARCH", garchOrder=c(1,1)))
  
  fit_ugarch = ugarchfit(data = df$mWh_generated, spec = spec_ugarch, out.sample = fc_length)
  return(tsibble(mWh_generated = ugarchsim(fit_ugarch, 
                                          n.sim = fc_length, 
                                          n.start = 0, 
                                          startMethod = "sample", 
                                          m.sim = 1)@simulation$seriesSim,
                date = seq(ymd(start_date), length.out = fc_length, by = "day")))
}


## Nuclear sim
garch_sim(fit_nuclear_arima, nuclear_data %>% mutate(mWh_generated = mWh_generated + 1300)) %>% 
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

simulation_period <- seq(ymd("2021-01-15"), length.out = 45, by = "day")

sim_type <- function(generation_type, sim_period = simulation_period,
                     fc_length = 15, start_date = "2021-02-01",
                     alteration_amount = 0) {
  #'
  #'@generation_type : string name of power generation type
  #'@sim_period: sequence of dates which denotes the period of simulation
  #'@fc_length:  forecasting period
  #'@start_date: start date of forcasting period
  type_df = generation_daily %>% 
    filter(type == !!generation_type &
             date %in% sim_period) %>% 
    as_tsibble(index = date)
  arima_fit = type_df %>% model(arima_fit = ARIMA(mWh_generated,
                                                  stepwise = TRUE,
                                                  approximation = TRUE))
  
  garch_simulation <- garch_sim(fit = arima_fit, 
                                df = type_df, 
                                fc_length = fc_length,
                                start_date = start_date)   %>% 
    mutate(mWh_generated = case_when((mWh_generated + alteration_amount) <= 0 ~ 0,
          (mWh_generated + alteration_amount) >= 0 ~ mWh_generated + alteration_amount)) 

  
  return(garch_simulation)
}




# Assume observed demand values
sim_all_sources <- function(sim_period = simulation_period,
                            fc_length = 15, start_date = "2021-02-01",
                            subtract_from = "gas", replace = FALSE,
                            alteration_amount = 5000,
                            added_nuclear = 0) {
  #' 
  generation_types <- generation_daily %>%  
    filter(type != "total") %>% 
    group_by(type) %>% 
    summarise(type = type) %>% unique() 
  generation_types <-  generation_types$type %>% c(.)
  
  simulation_output = NULL

  
  
  for (generation_type in generation_types) {
    if ((generation_type == subtract_from) && replace) {
      simulation_output %<>% bind_rows(simulation_output,
        tibble(date = seq(ymd(start_date), length.out = fc_length, by = "day"),
               mWh_generated  = sim_type(
                 generation_type = generation_type, 
                 sim_period = sim_period, fc_length = fc_length, 
                 start_date = start_date,
                 alteration_amount = -alteration_amount)$mWh_generated,
               type = generation_type)
      )
    }
    else if (generation_type == "nuclear" && replace) {
      simulation_output %<>% bind_rows(simulation_output,
                                       tibble(date = seq(ymd(start_date), length.out = fc_length, by = "day"),
                                              mWh_generated  = sim_type(
                                                generation_type = generation_type, 
                                                sim_period = sim_period,
                                                fc_length = fc_length, start_date = start_date,
                                                alteration_amount = alteration_amount)$mWh_generated + added_nuclear,
                                              type = generation_type)
      )
    }
    else {
      simulation_output %<>% bind_rows(simulation_output,
                                       tibble(date = seq(ymd(start_date), length.out = fc_length, by = "day"),
                                              mWh_generated  = sim_type(
                                                generation_type = generation_type, 
                                                sim_period = sim_period, 
                                                fc_length = fc_length, 
                                                start_date = start_date,
                                                alteration_amount = 0)$mWh_generated,
                                              type = generation_type)
      )
    }
    
  }
  return (simulation_output %>% unique())
  
}






sim_type(generation_type = "gas", alteration_amount = -500, fc_length = 19) %>% autoplot()


simulation_all_output <- sim_all_sources(fc_length = 19, replace = TRUE, alteration_amount = 10000, added_nuclear = 20000)

colnames(simulation_all_output) <- c("date", "mWh_generated", "type")

total_simulation_output <- simulation_all_output %>% 
  group_by(date) %>% 
  summarise(total = sum(mWh_generated))

simulation_all_output %>% 
  ggplot() +
  geom_line(aes(x = date, y = mWh_generated, col = type)) +
  geom_line(aes(x = date, y= total, col = "total"),linetype = "dashed", size = 1.5, data = total_simulation_output) +
  geom_line(aes(x = date, y = mWh_demand_daily, col = "demand"), data = 
              demand_data_daily  %>% 
              filter(date %in% simulation_all_output$date)) +
  labs(title = "Simulation") +
  scale_color_manual(values = color_scheme) +
  theme_bw()


## 100 simulation runs without 


max_deviation <- demand_data_daily %>% 
  left_join(generation_daily %>% filter(type == "total"), by = "date") %>% 
  filter(year(date) == 2021 & month(date) == 2) %>% 
  filter(!is.na(mWh_generated))

observed_maximum_deviation <- tibble(max = max(max_deviation$mWh_demand_daily - max_deviation$mWh_generated),
                                     max_date = max_deviation[which.max(max_deviation$mWh_demand_daily - max_deviation$mWh_generated), ])



power_simulation <- function(sim_nr, fc_length  = 19, replace = TRUE, 
                             subtract_from = "gas", alteration_amount = 5000, 
                             start_date = "2021-02-01") {
  power_exceeded <- data.frame("run_nr" = seq(1, sim_nr), 
                                exceed = FALSE,
                                max_deviation = 0)
  
  for(i in 1:sim_nr) {
    simulation_all_output = sim_all_sources(fc_length = 19) 
    colnames(simulation_all_output) <- c("date", "mWh_generated", "type")
    
    total_simulation_output = simulation_all_output %>% 
      group_by(date) %>% 
      summarise(total = sum(mWh_generated)) %>% 
      left_join(demand_data_daily, by = "date")
    
    
    max_dev <- max(total_simulation_output$mWh_demand_daily - 
                     total_simulation_output$total)  # Calculate deviation between demand and generation
    power_exceeded[i, "max_deviation"] <- max_dev
    
    
    if (max_dev >= observed_maximum_deviation$max)   power_exceeded[i, "exceed"] <- TRUE
 
  }
  return(power_exceeded)
}


power_gen_exceeds <- power_simulation(100)
power_gen_exceeds

# max deviation in 2021



