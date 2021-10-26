library(tidyverse)
library(lubridate)
library(modeltime)
library(timetk)
library(skimr)
library(h2o)
library(caTools)
library(highcharter)
library(tidymodels)

# ------------------------------------ Data preparation  ------------------------------------
raw<-read.csv('daily-minimum-temperatures-in-me (1).csv')
colnames(raw)<-c('Date', 'Temperature')

raw$Temperature<-gsub("\\?", "", raw$Temperature);
raw$Date<-as.Date(raw$Date, format="%m/%d/%Y");
raw$Temperature<-as.numeric(raw$Temperature)
                  
raw %>% plot_time_series(Date, Temperature);

raw_tk<-raw %>% tk_augment_timeseries_signature();
raw_tk %>% skim()
raw_tk %>% glimpse()

df_tk <- raw_tk %>%
  select(-contains("hour"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>%        #ordered type new columns will be converted to factor type 
  mutate_if(is.character,as_factor)

# ------------------------------------ H2O ------------------------------------
h2o.init()    
split<-sample.split(df_tk, SplitRatio=0.8);
train_h2o <- subset(df_tk, split==TRUE) %>% as.h2o()
test_h2o <- subset(df_tk, split==FALSE) %>% as.h2o()

y <- "Temperature" 
x <- df_tk %>% select(-Temperature) %>% names()

#1. Building h2o::automl().
model_h2o <- h2o.automl(
  x = x, y = y, 
  training_frame = train_h2o, 
  validation_frame = test_h2o,
  leaderboard_frame = test_h2o,
  stopping_metric = "RMSE",
  seed = 123, nfolds = 10,
  exclude_algos = c("DRF", "GLM","GBM", "XGBoost"),
  max_runtime_secs = 600) 

model_h2o@leaderboard %>% as.data.frame() 
h2o_leader <- model_h2o@leader

pred_h2o <- h2o_leader %>% h2o.predict(test_h2o) 

h2o_leader %>% 
  h2o.rmse(train = T,
           valid = T,
           xval = T)

error_tbl <- subset(df_tk, split == FALSE) %>% 
  add_column(pred = pred_h2o %>% as_tibble() %>% pull(predict)) %>%
  rename(actual = Temperature) %>% 
  select(Date,actual,pred)

highchart() %>% 
  hc_xAxis(categories = error_tbl$Date) %>% 
  hc_add_series(data=error_tbl$actual, type='line', color='red', name='Actual') %>% 
  hc_add_series(data=error_tbl$pred, type='line', color='green', name='Predicted') %>% 
  hc_title(text='Predict')

# 2. -----------Building auto arima model with modeltime::arima_reg()---------------------------
train<-subset(raw, split==TRUE);
test<-subset(raw, split==FALSE);
model_arima<- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Temperature~ Date, train)

modeltime_table(model_arima) %>% modeltime_calibrate(test) %>% 
  modeltime_forecast(actual_data=raw) %>% plot_modeltime_forecast(.interactive=TRUE)

modeltime_table(model_arima) %>% modeltime_calibrate(test) %>% modeltime_accuracy();


# --------3. Forecasting temperatures for next year with model which has lower RMSE.-----------------------
#Prediction for the new data
new_data <- seq(as.Date("1991-01-01"), as.Date("1991-12-31"), "days") %>%
  as_tibble() %>% 
  add_column(Temperature=0) %>% 
  rename(Date=value) %>% 
  tk_augment_timeseries_signature() %>%
  select(-contains("hour"),
         -minute,-second,-am.pm) %>% 
  mutate_if(is.ordered, as.character) %>% 
  mutate_if(is.character,as_factor)

# Forecast ----
new_data_h2o <- new_data %>% as.h2o()

#predictions for the next year
new_predictions <- h2o_leader %>% 
  h2o.predict(new_data_h2o) %>% 
  as_tibble() %>%
  add_column(Date=new_data$Date) %>% 
  select(Date,predict) %>% 
  rename(Temperature=predict)

raw %>% 
  bind_rows(new_predictions) %>% 
  mutate(categories=c(rep('Actual',nrow(raw)),rep('Predicted',nrow(new_predictions)))) %>% 
  hchart("line", hcaes(Date, Temperature, group = categories)) %>% 
  hc_title(text='Forecast the for next year') %>% 
  hc_colors(colors = c('red','green'))






