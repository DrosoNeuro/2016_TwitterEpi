root_path <- "~/Dropbox/UZH_Master/Masterarbeit/TwitterEpi/ExploratoryAnalysis/"
setwd(root_path)
load("twitter_data_aggregated/df_agg_merged.RData")
save(flu_aggregated,file="flu_aggregated_regionl.RData")
nat <- flu_aggregated[statename=="National",]
flu_ar <- ar(nat$sick[1:150])
prediction <- predict(flu_ar,n.ahead=100)
plot(prediction$pred)
plot(nat$sick,type="l")
lines(prediction$pred,col="red")

flu_arima <- arima0(nat$sick[1:150],order=c(1,1,1))
prediction_arima <-  predict(flu_arima,n.ahead=50)
plot(nat$sick,type="l")
lines(prediction_arima$pred,col="red")

#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
require("forecast")
require("tseries")
plot(nat$weekend,log(nat$sick),type="l")

decompose(nat$sick)
stl(nat$sick)
adf.test(nat$sick)
acf(nat$sick)
pacf(nat$sick)

flu_ts <- ts(nat$sick)
nat$clean <- tsclean(flu_ts)
plot(nat$clean)

cdc_nat <- cdc_data[statename=="National"]
cdc_ts <- ts(cdc_nat$sick)
cdc_baseline <- 5480
twitter_baseline <- 5537

nat$sick_ma2 <- ma(nat$sick,order=2)
nat$sick_ma4 <- ma(nat$sick,order=4)
nat$sick_ma10 <- ma(nat$sick,order=10)
nat$sick_ma20 <- ma(nat$sick,order=20)
plot(nat$sick_ma2,type="l")
lines(nat$sick_ma20,col="red")
lines(nat$sick_ma4,col="blue")
plot(nat$sick_ma4,col="blue")

flu_monthly <- ts(na.omit(nat$sick_ma4),frequency=4)
flu_weekly <- ts(na.omit(nat$sick),frequency=7)
flu_biweekly <- ts(na.omit(nat$sick_ma2),frequency=7,start=1)
cdc_weekly <- ts(na.omit(cdc_ts),frequency=7)
cdc_biweekly <- ts(na.omit(ma(cdc_ts,order=2)),frequency=7)

plot(cdc_weekly/cdc_baseline,col="red")
lines(flu_biweekly/twitter_baseline,col="blue")

decomp_cdc <- stl(cdc_weekly,s.window="periodic")
decomp_twitter <- stl(flu_weekly,s.window="periodic")
deseasonal_cdc <- seasadj(decomp_cdc)
deseasonal_twitter <- seasadj(decomp_twitter)

adf.test(flu_weekly)
adf.test(cdc_weekly)
adf.test(deseasonal_cdc)

fit_cdc <- auto.arima(deseasonal_cdc,xreg=deseasonal_twitter)
tsdisplay(residuals(fit_cdc), lag.max=45, 
          main='')

fit_cdc2 <- arima(deseasonal_cdc,order=c(3,0,1),seasonal=list(order=c(0,0,1),period=52),xreg=deseasonal_twitter)
tsdisplay(residuals(fit_cdc2), lag.max=52, 
          main='')

hold <- window(ts(deseasonal_cdc),start=180)
fit_no_holdout <- arima(deseasonal_cdc[-c(180:196)],order=c(3,0,1),seasonal=c(0,0,1),xreg=deseasonal_twitter[-c(180:196)])
fcast_no_holdout <- predict(fit_no_holdout,n.ahead=17,xreg=deseasonal_twitter[-c(180:196)],newxreg=deseasonal_twitter[180:196])
fcast_no_holdout <- forecast(fit_no_holdout,xreg=deseasonal_twitter[180:196])
plot(seq(1,196),deseasonal_cdc,type="l")
lines(seq(180,196),fcast_no_holdout$pred,col="blue")
plot(fcast_no_holdout$pred, main=" ")
lines(ts(deseasonal_cdc,frequency=1)[180:195],col="red")
lines(ts(deseasonal_cdc))

fit_cdc3 <- arima(deseasonal_cdc,order=c(3,0,1),seasonal=list(order=c(0,0,1),period=52))
tsdisplay(residuals(fit_cdc2), lag.max=52, 
          main='')
hold <- window(ts(deseasonal_cdc),start=180)
fit_no_holdout <- arima(deseasonal_cdc[-c(180:196)],order=c(3,0,1),seasonal=list(order=c(0,0,1),period=52))
fcast_no_holdout <- forecast(fit_no_holdout,h=17)
plot(fcast_no_holdout)
lines(seq(1,196),deseasonal_cdc,col="red")


fcast_no_holdout$pred
decomp <- stl(flu_biweekly,s.window="periodic")
decomp_cdc <- stl(cdc_ts,s.windo="periodi")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(deseasonal_cnt,alternative="stationary")
adf.test(flu_monthly,alternative="stationary")
adf.test(flu_biweekly,alternative="stationary")

acf(flu_biweekly,main="")
Pacf(flu_biweekly,main="")

flu_biweekly_d4 <- diff(deseasonal_cnt, differences=4)
adf.test(flu_biweekly_d4,alternative="stationary")

auto.arima(deseasonal_cnt,seasonal=FALSE)

fit0 <- auto.arima(flu_weekly,seasonal=FALSE,xreg=cdc_ts)
fit0 <- arima(flu_weekly,order=c(6,1,1),xreg=cdc_ts)
tsdisplay(residuals(fit0), lag.max=45, 
          main='')

fit <- auto.arima(deseasonal_cnt,seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, 
          main='(5,1,2) Model Residuals')


fit2 <- arima(deseasonal_cnt, order=c(5,1,3))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
fit2

fcast <- forecast(fit2,h=30)
plot(fcast)

hold <- window(ts(flu_weekly),start=180)
fit_no_holdout <- arima(flu_weekly[-c(180:194)],order=c(6,1,1),xreg=cdc_ts[-c(180:194)])

fcast_no_holdout <- predict(fit_no_holdout,n.ahead=14,xreg=cdc_ts[-c(180:194)],newxreg=cdc_ts[180:193])
plot(flu_weekly,type="l")
lines(fcast_no_holdout$pred,col="red")
plot(fcast_no_holdout$pred, main=" ")
lines(ts(deseasonal_cnt))

flu_arima <- arima0(nat$sick[1:150],order=c(6,2,1))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')


cdc_nat
cdc_sick <- ts(cdc_nat$sick)
