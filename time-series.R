


# Time series

#A popular method for forecasting is the *autoregressive integrated moving average* model; short [ARIMA](https://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average) model. This kind of model consists of three building blocks which parametrised by the three indeces *p, d, q* as ARIMA(p, d, q):
  
  
#- **auto-regressive / p:** we are using past data to compute a regression model for future data. The parameter 
#*p* indicates the range of *lags*; e.g. ARIMA(3,0,0) includes *t-1*, *t-2*, and *t-3* values in the regression 
#to compute the value at *t*.

#- **integrated / d:** this is a *differencing* parameter, which gives us the number of times we are subtracting 
#the current and the previous values of a time series. Differencing removes the change in a time series in that 
#it stabilises the mean and removes (seasonal) trends. This is necessary since computing the lags (e.g. difference 
#between time *t* and time *t-1*) is most meaningful if large-scale trends are removed. A time series where the 
#variance (or amount of variability) (and the autocovariance) are time-invariant (i.e. don change from day to day) 
#is called *stationary*.


#- **moving average / q:** this parameter gives us the number of previous error terms to include in the regression 
#error of the model.


#Here we will be using the `auto.arima` tool which estimates the necessary ARIMA parameters for each individual 
#time series. In order to feed our data to `auto.arima` we need to turn them into a time-series object using the 
#`ts` tool. We will also add a step for cleaning and outlier removal via the `tsclean` function of the 
#[*forecast* package](https://cran.r-project.org/web/packages/forecast/index.html).




#We use the first *air\_store\_id* ("air_ba937bf13d40fb24") as an example.
air_id = "air_ba937bf13d40fb24"

#In order to test our prediction, we will forecast for an identical time frame as we are ultimately 
#tasked to predict (Apr 23th - May 31st). Here we automatically extract these 39 days from the length of the 
#*test* prediction range and define it as our "prediction length".

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
  rownames_to_column()


#Using this new time series, we now split the data into *training* and *validation* sets.

visits_train <- visits %>% filter(visit_date <= split_date)
visits_valid <- visits %>% filter(visit_date > split_date)


#Now comes the fitting part. As said before, we use the `ts` function to create a time series object 
#and the `tsclean` tool to remove outliers. We also add the weekly frequency. The `stepwise` and `approximation` 
#parameter settings mean that the tool performs a more thorough and precise search over all model parameters. 
#This increases the computing time, but for our small data set this doesn't matter much.

arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)

#Using the fitted ARIMA model we will `forecast` for our "prediction length". We include confidence intervals.

arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))


#Finally, we plot our prediction. The `autoplot` function of the `ggplot2` package creates plots 
#according to the properties of a particular data type; here a time series object. The predicted 
#*visitor* counts are shown in dark blue, with the lighter blues indicating the confidence ranges. 
#We also add the real validation counts in grey:
arima_visits %>%
  autoplot +
  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
  labs(x = "Time [weeks]", y = "log1p visitors vs auto.arima predictions")

plot_auto_arima_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          stepwise = FALSE, approximation = FALSE)

  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(50,95))
  
  arima_visits %>%
    autoplot +
    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "grey40") +
    labs(x = "Time [weeks]", y = "log1p visitors vs forecast")

}


p1 <- plot_auto_arima_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_auto_arima_air_id("air_8e4360a64dbd4c50")
p3 <- plot_auto_arima_air_id("air_1c0b150f9e696a5f")
p4 <- plot_auto_arima_air_id("air_900d755ebd2f7bbd")


# prophet


#The [prophet](https://facebookincubator.github.io/prophet/) forecasting tool is an open-source software 
#developed by [Facebook](https://research.fb.com/prophet-forecasting-at-scale/). It is available for both 
#*R* and Python.

#Prophet utilises an additive regression model which decomposes a time series into (i) a (piecewise) 
#linear/logistic trend, (ii) a yearly seasonal component, (iii) a weekly seasonal component, and (iv) 
#an optional list of important days (such as holidays, special events, ...). It claims to be "robust to 
#missing data, shifts in the trend, and large outliers". Especially the missing data functionality could be
#useful in this competition.


#We will again create a *training* and *validation* set for the same periods as above 
#(i.e before/after Mar 14th). The only differences to our ARIMA approach are that: 
  
#- We don't need to replace `NA` values because prophet knows how to handle those.
#- Prophet expects a data frame with two columns: *ds* for the dates and *y* for the time series variable.

air_id = "air_ba937bf13d40fb24"

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)

#Here we fit the prophet model and make the forecast:
  
# - the parameter *changepoint.prior.scale* adjusts the trend flexibility. Increasing this parameter makes the 
#fit more flexible, but also increases the forecast uncertainties and makes it more likely to overfit to noise. 
#The changepoints in the data are automatically detected unless being specified by hand using the *changepoints* 
#argument (which we don't do here).
                                                                                                                                                                                                                                                                                                                                                        
#- the parameter *yearly.seasonality* has to be enabled/disabled explicitely and allows prophet to notice 
#large-scale cycles. We have barely a year of data here, which is definitely insufficient to find yearly cycles
#and probably not enough to identify variations on the time scales of months. Feel free to test the performance 
#of this parameter. 

proph <- prophet(visits_train, changepoint.prior.scale=0.5, yearly.seasonality=FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast <- predict(proph, future)

#This is the standard prophet forecast plot:

plot(proph, fcast)

#The observed data are plotted as black points and the fitted model, plus forecast, as a blue line. 
#In light blue we see the corresponding uncertainties.

#Prophet offers a decomposition plot, where we can inspect the additive components of the model: 
#trend, yearly seasonality (if included), and weekly cycles:

prophet_plot_components(proph, fcast)

fcast %>%
  as.tibble() %>%
  mutate(ds = date(ds)) %>%
  ggplot(aes(ds, yhat)) + 
  geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
  geom_line(colour = "blue") +
  geom_line(data = visits_train, aes(ds, y), colour = "black") +
  geom_line(data = visits_valid, aes(ds, y), colour = "grey50")


plot_prophet_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors, ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  
  proph <- prophet(visits_train, changepoint.prior.scale=0.5, yearly.seasonality=FALSE)
  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)
  
  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c("Prophet for ", air_id))
  return(p)
}  

p1 <- plot_prophet_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_prophet_air_id("air_8e4360a64dbd4c50")
p3 <- plot_prophet_air_id("air_1c0b150f9e696a5f")
p4 <- plot_prophet_air_id("air_820d1919cbecaa0a")



plot_prophet_air_id_holiday <- function(air_id, use_hday){
  
  air_visits_cut <- air_visits %>%
    filter(visit_date <= ymd("20160531"))
  
  hday <- holidays %>%
    filter(holiday_flg == TRUE) %>%
    mutate(holiday = "holiday") %>%
    select(ds = date, holiday)

  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits_cut$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits_cut$visit_date), max(air_visits_cut$visit_date), 1))
  
  foo <- air_visits_cut %>%
    filter(air_store_id == air_id)

  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors, ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)

  if (use_hday == TRUE){
    
    proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,                   
                     yearly.seasonality=FALSE,
                     holidays = hday)
    ptitle = "Prophet (w/ holidays) for "
    
  } else {
    proph <- prophet(visits_train,
                     changepoint.prior.scale=0.5,
                     yearly.seasonality=FALSE)
    
    ptitle = "Prophet for "
    
  }

  future <- make_future_dataframe(proph, periods = pred_len)
  fcast <- predict(proph, future)

  p <- fcast %>%
    as.tibble() %>%
    mutate(ds = date(ds)) %>%
    ggplot(aes(ds, yhat)) +
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    geom_line(colour = "blue") +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    labs(title = str_c(ptitle, air_id))
  return(p)
}  

p1 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", TRUE)
p2 <- plot_prophet_air_id_holiday("air_5c817ef28f236bdf", FALSE)

#  holt winters

#A more traditional time series filtering and forecasting is the *Holt-Winters* algorithm, as implemented 
#in the [`stats` package](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/HoltWinters.html). 
#This is an exponential smoothing method which uses moving averages to take into account the presence of a trend 
#in the data. Here we define a default seasonal model in a fitting and plotting function:

plot_hw_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    replace_na(list(visitors = median(log1p(foo$visitors)))) %>%
    rownames_to_column()
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  hw.fit <- HoltWinters(tsclean(ts(visits_train$visitors, frequency = 7)))
  hw_visits <- predict(hw.fit, n.ahead = pred_len, prediction.interval = T, level = 0.95) %>%
    as.tibble() %>%
    bind_cols(visits_valid)
  
  
  
  visits_train %>%
    ggplot(aes(visit_date, visitors)) +
    geom_line() +
    geom_ribbon(data = hw_visits, aes(x = visit_date, ymin = lwr, ymax = upr), fill = "light blue") +
    geom_line(data = hw_visits, aes(visit_date, visitors), color = "grey60") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    geom_line(data = hw_visits, aes(visit_date, fit), color = "blue") +
    labs(x = "Time [weeks]", y = "log1p visitors vs predictions") +
    ggtitle("HoltWinters")
  
}

plot_hw_air_id("air_ba937bf13d40fb24")

p1 <- plot_hw_air_id("air_f3f9824b7d70c3cf")
p2 <- plot_hw_air_id("air_8e4360a64dbd4c50")
p3 <- plot_hw_air_id("air_1c0b150f9e696a5f")
p4 <- plot_hw_air_id("air_820d1919cbecaa0a")


#timetk

#The [timetk package](https://cran.r-project.org/web/packages/timetk/index.html) is a recently released tool 
#kit for time series analysis. It integrates well into the tidyverse ecosystem and is specifically designed to 
#work with the tidy "tibble" data frames. Here we briefly describe the `timetk` approach and how we can apply it 
#to our data.

#First, we will create the train and validation data frames in the same way as for the other prediction tools:



air_id = "air_ba937bf13d40fb24"

pred_len <- test %>%
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  distinct(date) %>%
  nrow()

max_date <- max(air_visits$visit_date)
split_date <- max_date - pred_len
all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

foo <- air_visits %>%
  filter(air_store_id == air_id)

visits <- foo %>%
  right_join(all_visits, by = "visit_date") %>%
  mutate(visitors = log1p(visitors)) %>%
  rownames_to_column() %>%
  select(y = visitors,
         ds = visit_date)

visits_train <- visits %>% filter(ds <= split_date)
visits_valid <- visits %>% filter(ds > split_date)

#Then, we use the `tk_augment_timeseries_signature` tool to augment our data frames with time series characteristics. 
#This means that we will add comprehensive time series properties that have been extracted from the *date*. 
#Those new features include for instance the month, day and week of the year, half and quarter of the year; 
#down to minutes and seconds for `date-time` data. Here we show a `glimpse` of the augmented training data: 
  
visits_train_aug <- visits_train %>%
  tk_augment_timeseries_signature()

visits_valid_aug <- visits_valid %>%
  .$ds %>%
  tk_get_timeseries_signature()

glimpse(visits_train_aug)


#Now, the idea behind `timetk` is to use these new features to make predictions based on a regression or classification 
#approach; with standard tools such as linear/logistic regression or (boosted/ensembled) trees. For this example, 
#we will use a simple linear model. This approach can easily be extended to more sophisticated methods.



fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))
pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))

pred_tk <- tibble(
  date  = visits_valid$ds,
  value = pred
)


plot_tk_lm_air_id <- function(air_id){
  
  pred_len <- test %>%
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    distinct(date) %>%
    nrow()
  
  max_date <- max(air_visits$visit_date)
  split_date <- max_date - pred_len
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>%
    mutate(visitors = log1p(visitors)) %>%
    rownames_to_column() %>%
    select(y = visitors,
           ds = visit_date)
  
  visits_train <- visits %>% filter(ds <= split_date)
  visits_valid <- visits %>% filter(ds > split_date)
  # augment train with ts info
  
  visits_train_aug <- visits_train %>%
    tk_augment_timeseries_signature()
  
  # fit lm
  fit_lm <- lm(y ~ ., data = select(visits_train_aug, -c(ds, diff, wday.xts, wday.lbl, year.iso)))
  # augment valid with ts info
  visits_valid_aug <- visits_valid %>%
    .$ds %>%
    tk_get_timeseries_signature()
  
  # predict from lm
  pred <- predict(fit_lm, newdata = select(visits_valid_aug, -c(index, diff, wday.xts, wday.lbl, year.iso)))
  pred_tk <- tibble(
    date  = visits_valid$ds,
    y_pred = pred
  )
  # plot
  p <- pred_tk %>%
    ggplot(aes(date, y_pred)) +
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    geom_line(colour = "blue") +
    labs(title = str_c("timetk for ", air_id))
  return(p)
}  

plot_tk_lm_air_id("air_ba937bf13d40fb24")


