---
date: March 24, 2022
output: html_document
title: Forecasting US Energy Consumption
---

![Forecasting US Energy
Consumption](vertopal_ba67c2cfc70b459c93e5fdc2ec52ea43/d5f8e51ddb1a75e9987cae388cf29db30dad7a62.png)

## Goal:

The goal of this project is to build a model to forecast the US energy
consumption.

## About Data

The data is downloaded from the eia's website which is a U.S Energy
Information Administration and does independent Statistics and Analysis.

https://www.eia.gov/totalenergy/data/browser/xls.php?tbl=T02.01&freq=m

## Importing Modules

``` {.{r}}
install.packages("rlang", repos = "http://cran.us.r-project.org")
install.packages("fpp3", repos = "http://cran.us.r-project.org")
install.packages("readxl", repos = "http://cran.us.r-project.org")
install.packages("tsibble", repos = "http://cran.us.r-project.org")
install.packages("tseries", repos = "http://cran.us.r-project.org")
```

`{r message=FALSE, warning=FALSE} library(tseries) library(fpp3) library(tsibble) library(readxl) library(dplyr) library(knitr)`

``` {.{r}}
#Clearing variables
rm(list=ls())
```

## EDA

`{r message=FALSE, warning=FALSE} #Reading Data data <-read_excel("D:/D_ST/VCU/Spring 2022/DAPT-632 Forecasting/Case/Total Energy Consumption.xlsx")`

``` {.{r}}
#Converting to a tsibble
data$Month = yearmonth(data$Month)
data <- as_tsibble(data, index = Month )
```

``` {.{r}}
#Visualize Data
data %>%
  autoplot(log(TotalEnergyConsumption)) +
  xlab("Month") + 
  ylab("Total Energy Consumed (Trillion BTU)") +
  ggtitle("Total Energy Consumption")
```

It can be seen from the above graph that Energy consumption has a
increasing Trend and have monthly seasonality. From year 2000 to 2021
the trend seems to damped which is because energy industry is changing
very fast as more renewable energy coming into picture and there have
been several national energy-efficiency policy coming into place.

Since, Energy Consumption changed drastically over time period, let's
consider data for last two decades

``` {.{r}}
data<- data %>%
      filter(year(Month) > 2000) %>%
      select(Month,TotalEnergyConsumption )
```

``` {.{r}}
#Visualize Data
data %>%
  autoplot(TotalEnergyConsumption) +
  xlab("Month") + 
  ylab("Total Energy Consumed (Trillion BTU)") +
  ggtitle("Total Energy Consumption")
```

At a glance we can see that there is definitively seasonality in the
Energy Consumption and it dropped drastically in month of April 2020
because of the lock down that the world saw when the Covid-19 hit. But
energy consumption gradually reached the same pace as the business
reopened.

``` {.{r}}
#Visualize Data for 2020
data %>%
  filter(year(Month) == 2020)  %>%
  select(Month,TotalEnergyConsumption )  %>%
  autoplot(TotalEnergyConsumption) +
  xlab("Month") + 
  ylab("Total Energy Consumed (Trillion BTU)") +
  ggtitle("Total Energy Consumption")
```

For year 2020, there is sudden dip in Energy Consumption for month of
April and it started catching up in July.

``` {.{r}}
#Plotting Seasonality
data %>%
  filter(year(Month) > 2014) %>%
  gg_season(TotalEnergyConsumption, period = "year") +
  labs(y = "Total Energy Consumed (Trillion BTU)" ,
       title = "Seasonal plot: Total Energy Consumption")
```

``` {.{r}}
data %>%
  gg_subseries(TotalEnergyConsumption) +
  labs(
    y = "Total Energy Consumed (Trillion BTU)",
    title =  "Seasonal SubSeries: Total Energy Consumption")
```

It is clear from the above gg_subseries plot that the lowest energy
consumption was seen in month April, May, June of year 2020.

``` {.{r}}
#Plotting Lags
data %>%
  gg_lag(TotalEnergyConsumption, geom = "point", lags = c(12,24,36)) +
  labs(x = "lag(TotalEnergyConsumption, k)")

data %>%
  gg_lag(TotalEnergyConsumption, geom = "point") +
  labs(x = "lag(TotalEnergyConsumption, k)")
```

The dots closely aligning to the line shows that here is a strong
positive correlation between the lags.

``` {.{r}}
# Plotting the auto Correlations
data %>%
  ACF(TotalEnergyConsumption, lag_max = 120) %>%
  autoplot() + labs(title="Total Energy Consumed (Trillion BTU)")
```

-   The ACF plot there is 'Trend' in our data since the ACF is slowly
    decreasing as our lags are increasing.
-   The peaks can be observed at lag 12, 24, 36 showing seasonal lags.
-   The alternating(Sinusoidal) and tapering pattern suggests that there
    is higher order auto-regressive term in the data and a AR model will
    be well suited for forecasting this Time Series Data.

``` {.{r}}
#STL Without season window
dcmp <- data %>%
        model(STL(TotalEnergyConsumption))

#STL WITH SEASON WINDOW
Sdcmp <- data %>%
  model(STL(TotalEnergyConsumption ~ season(window= "periodic"), robust = TRUE)) 

#Plotting all components
components(dcmp) %>% autoplot() + xlab("Year")
components(Sdcmp) %>% autoplot() + xlab("Year")
```

The Trend line exhibits a little changing behavior so we will try ETS
with additive trend and Without trend. The Seasonality changes in
magnitude each year so a additive method seems necessary. The Error
changes in magnitude as the series goes along so a multiplicative method
will be used. This leaves us with an ETS(M,N,A) and ET(M,A,A) model.

## ARIMA

The value for D (non-seasonal differencing) and d (seasonal
differencing)

``` {.{r}}
# Number of non seasonal differences required(d)
data %>%
  mutate(log_TEC = (TotalEnergyConsumption)) %>%
  features(log_TEC, unitroot_nsdiffs)
```

``` {.{r}}
# Number of Seasonal differences required(D)
data %>%
  mutate(log_TEC = difference((TotalEnergyConsumption), 12)) %>%
  features(log_TEC, unitroot_ndiffs)
```

Since unitroot_nsdiffs() returns 1 (indicating one seasonal difference
is required), we apply the unitroot_ndiffs() function to the seasonally
differences data. These functions suggest we should do a seasonal
difference only. Therefore, for our ARIMA model D = 0 and d = 1 .

\`\`\`{r warning=FALSE}

# ACF and PACF plot for Seasonally differenced data Total Energy Consumption

data %\>% gg_tsdisplay((difference(TotalEnergyConsumption, 12)),
plot_type='partial') + labs(title="Seasonally differenced Series")


    Let's test to verify that the Time series is not stationary
    - Null Hypothesis (H0) : The time series displays a unit-root.(time series is stationary)
    - Alternate Hypothesis (H1) : There is no unit-root in the time series.(time series is not stationary)

    The p-value of the KPSS test will be 0.01 or 0.1. It is 0.01 if the actual p-value is less than 0.01 and 0.1 
    if it is greater than 0.1

    If p value = 0.01 : Reject the Null : Data is Not Stationary
    If p value = 0.1 : Fail to Reject Null : Data is Stationary


    ```{r}
    # KPPS 
    data %>%
      features((TotalEnergyConsumption), unitroot_kpss)

The p-value is 0.1. Therefore, we fail to reject the null hypothesis,
the data is stationary. Now let's run a bunch of models

## Modeling

``` {.{r}}
# Dividing data set into Test and Train

#Train Set
data_train <- data %>%
              filter(year(Month) != 2021)

#Test Set
data_test <- data %>%
             filter(year(Month) == 2021) 
```

## Fitting Multiple Models

``` {.{r}}
# Fit Model
fit_mul <- data_train %>%
  model(
    #TSLM
    linear = TSLM((TotalEnergyConsumption) ~ trend() + season()),
    #ETS
    ETS = ETS((TotalEnergyConsumption)),
    ETS_Additive = ETS(TotalEnergyConsumption ~ error("A")+trend("A")+season("A")),
    ETS_MAA = ETS((TotalEnergyConsumption) ~ error("M") + trend("A") + season("A")),
    ETS_MNA = ETS((TotalEnergyConsumption) ~ error("M") + trend("N") + season("A")),
    #ARIMA
    arima = ARIMA(TotalEnergyConsumption, stepwise = FALSE, approx = FALSE)
    )
  
# Forecast 
fc_mul <- fit_mul %>%
  forecast(h = 11)

#Plot the Forecast
fc_mul %>%
  autoplot(level = NULL) +
  autolayer(data_test, TotalEnergyConsumption) +
  labs(title = "Comparision of different models for Total Energy Consumption")
```

## Checking Accuracy of Models

``` {.{r}}
# Checking Accuracy of Models
accuracy <-  accuracy(fc_mul, data)
View(accuracy)
```

As per the above table, the 'linear' model has the lowest value of RMSE,
MAE and MAPE followed by 'ETS_MAA'.

### 1st Best Model

``` {.{r}}
best_model_1 <- fit_mul %>% select(linear)  
best_model_1 %>% gg_tsresiduals()
```

The model also has some significant autocorrelation in the residuals,
and the histogram of the residuals shows long right tail and residuals
are not white noise.

``` {.{r}}
report(best_model_1)
```

``` {.{r}}
# Residual Plot against fitted values
augment(best_model_1) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "fitted", y = "Residuals")
  
```

The residuals looks randomly scattered, there seems to be gaps in
residuals.

``` {.{r}}
# ljung_box Test
augment(best_model_1) %>% 
    features(.resid, ljung_box, lag = 24, dof = 12)
```

The p-value of this model comes out to be zero, it implies the model
errors are auto-correlated.

### 2nd Best Model

``` {.{r}}
best_model_2 <- fit_mul %>% select(ETS_MAA)  
best_model_2 %>% gg_tsresiduals()
```

ACF plot shows all the lags are within the bound there are no
significant correlation in residuals and the time series represent white
noise. The histogram also looks perfect except one outlier.

``` {.{r}}
# Residual Plot against fitted values
augment(best_model_2) %>%
  ggplot(aes(x = log(.fitted), y = .resid)) +
  geom_point() + labs(x = "fitted", y = "Residuals")
```

Residuals looks randomly scattered, this means there is no
Heteroscedasticity and residuals are white noise.

``` {.{r}}
# ljung_box Test
augment(best_model_2) %>% 
    features(.resid, ljung_box, lag = 24, dof = 16) 
```

The p-value of this model comes out to be 0.4, which is greater than
0.05, therefore we can conclude that our ETS_MAA model pass the residual
test.

## Cross Validation

\`\`\`{r warning=FALSE} data_sets \<- data %\>%\
stretch_tsibble(.init = 50, .step = 10)

fit_mul_CV \<- data_sets %\>% model( \#TSLM linear =
TSLM((TotalEnergyConsumption) \~ trend() + season()), \#ETS ETS =
ETS((TotalEnergyConsumption)), ETS_Additive = ETS(TotalEnergyConsumption
\~ error("A")+trend("A")+season("A")), ETS_MAA =
ETS((TotalEnergyConsumption) \~ error("M") + trend("A") + season("A")),
ETS_MNA = ETS((TotalEnergyConsumption) \~ error("M") + trend("N") +
season("A")), \#ARIMA arima = ARIMA(TotalEnergyConsumption, stepwise =
FALSE, approx = FALSE) )

fc_mul_CV \<- fit_mul_CV %\>% forecast(h = 11)

accuracy_CV \<- accuracy(fc_mul_CV, data) View(accuracy_CV)



    ### Best Model after Cross Validation

    ```{r}
    best_model_cv <- fit_mul %>% select(arima)  
    best_model_cv %>% gg_tsresiduals()

ACF plot shows all the lags are within the bound there are no
significant correlation in residuals and the time series represent white
noise. The histogram also looks perfect except one outlier.

``` {.{r}}
#Residual Plot against fitted values
augment(best_model_cv) %>%
  ggplot(aes(x = log(.fitted), y = .resid)) +
  geom_point() + labs(x = "fitted", y = "Residuals")
```

Residuals looks randomly scattered, this means there is no
Heteroscedasticity.

``` {.{r}}
#ljung_box Test
augment(best_model_cv) %>% 
    features(.resid, ljung_box, lag = 24, dof = 14)  
```

The p-value of this model comes out to be 0.17, which is greater than
0.05, therefore we can conclude that our Arima model pass the residual
test.

``` {.{r}}
#ljung_box Test
report(best_model_cv) 
```

## Vizualizing Forecast

``` {.{r}}
data_train %>%
  model(ARIMA(TotalEnergyConsumption, stepwise = FALSE, approx = FALSE)) %>%
  forecast(h=11) %>%
  autoplot(data_test, level = NULL) +
  labs(title = "Forecast of US Total Energy Consumption with Arima",
       y = "Trillion BTU")  
 
```

``` {.{r}}
data_train %>%
  model(ARIMA(TotalEnergyConsumption, stepwise = FALSE, approx = FALSE)) %>%
  forecast(h=11) %>%
  autoplot(data_train) +
  labs(x = "Year Month",
       y = "Trillion BTU")
```

## Modelling out the Covid Impact by adding Indicator Variables (ARIMA VS LINEAR)

``` {.{r}}
#Adding Indicator Variable
df<- data %>%
      mutate(Covid_indicator = if_else( month(Month) == 4 & year(Month) == 2020, 1, 0))
```

``` {.{r}}
# Dividing data set into Test and Train

#Train Set
df_train <- df %>%
  filter(year(Month) != 2021)

#Test Set
df_test <- df %>%
  filter(year(Month) == 2021)
```

``` {.{r}}
# Model Fitting
fit_linear = df_train %>% model(TSLM((TotalEnergyConsumption) ~ trend() + season() + Covid_indicator))

fit_arima = df_train %>% model(ARIMA(TotalEnergyConsumption, stepwise = FALSE, approx = FALSE))
```

``` {.{r}}
# Forecasting
fc_linear = fit_linear %>%
            forecast(
              new_data(df_train, 11) %>%
                       mutate(Covid_indicator = 0)
              )


fc_arima = fit_arima %>%
  forecast(h = 11)
```

``` {.{r}}
# Checking Accuracy
x <- bind_rows(
  fit_arima %>% accuracy(),
  fit_linear %>% accuracy(),
  fit_arima %>% forecast(h = 11) %>% accuracy(df),
  fit_linear %>% forecast(new_data(df_train, 11) %>% mutate(Covid_indicator = 0)) %>% accuracy(df)
) %>% select(.model, .type, RMSE,MAE,MAPE,)
View(x)
```

``` {.{r}}
#Vizualizing ARIMA and Linear Forecast
fc_linear %>%
  autoplot(df_test, level = NULL, colour = 'blue') +
  autolayer(fc_arima, level = NULL,colour = 'red')  +
  labs(title = "Forecast of US Total Energy Consumption with Arima",
       y = "Trillion BTU")  +
  guides(colour = guide_legend(title = "Forecast"))
```

CONCLUSION:

The Linear did better on Test dataset because it learned from all the
past data and averaged out the effect of outlier (APRIL 2020). While on
the other hand Arima learned most from it recent past (April 2020).
Arima is going to take into account that the this will persist.

Arima did better on Train set when we did cross validation because not
all of the test test in cross validation had April 2020..

So, both models can be used, depending on the circumstance and we
learned that how a poorly placed outlier can sway our forecasts.
