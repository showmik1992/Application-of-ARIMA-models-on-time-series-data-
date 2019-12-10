install.packages(forecast)
library("forecast");library("ggplot2");library("readxl")
library("tseries")

library(TSPred)

md_weather_data<-read_excel("POBF2eDataFiles/MagdeburgWeather.xlsx")

md_weather_ts<-ts(md_weather_data[,2],start=c(1989,1),frequency = 12)
plot(md_weather_ts)


#### Exploratory data analysis : 
decompose(md_weather_ts)
plot(decompose(md_weather_ts))
adf.test(md_weather_ts)
#partition of the data
md_weather_train<-window(md_weather_ts,start=c(1989,1),end=c(2015,12))
md_weather_test<-window(md_weather_ts,start=c(2016,1),end=c(2019,5))

#####seasonal naive forecast : 
md_temp_snaive<-snaive(md_weather_train,h=48)
summary(md_temp_snaive)
###comparision with naive forecast : 
forecastnaive<-function(x,h){forecast(snaive(x),h=h)}
###point forecast for naive : 
forecastnaive_pfc<-function(x,h){forecast(snaive(md_weather_ts),h=12)}
summary(forecastnaive_pfc())

####snaive vs test data plot :
plotarimapred(md_weather_test,md_temp_snaive,xlim = c(2014,2020),xlab = "Years",ylab = "Avg_temp")
#### finding forecast error for naive 
md_weather_ts%>%tsCV(forecastfunction = forecastnaive,h=12)%>%window(start=c(2016,1))->testfcesnaive
mean(testfcesnaive^2,na.rm=TRUE)%>%sqrt()
#rmse=9.356

####finding the correct state space model using ETS : 
fit_md_weather<-ets(md_weather_train)
summary(fit_md_weather)
#### |(A,N,A)model, parameters : alpha = 0.1169 , gamma = 1e-04 , RMSE=1.905
checkresiduals(fit_md_weather)#null hypothesis is not rejected . The model fits the provided data

ets_forecast<-forecast(fit_md_weather,h=48)
summary(ets_forecast)

##finding rolling forecast error and point forecast using the ets model 
forecast_md_weath_ets<-function(x, h){forecast(ets(x,model="ANA",alpha=0.1169,gamma = 1e-04), h=h)}
#determining the point forecast : 
forecast_md_weath_ets_pfc<-function(x, h){forecast(ets(md_weather_ts,model="ANA",alpha=0.1169,gamma = 1e-04), h=12)}
summary(forecast_md_weath_ets_pfc())
###determining the rolling forecast error
md_weather_ts%>%tsCV(forecastfunction = forecast_md_weath_ets,h=12)%>%window(start=c(2016,1))->testfcets 
mean(testfcets^2,na.rm=TRUE)%>%sqrt()

#RMSE = 9.2267

####plot : test data vs selected ets model : 
plotarimapred(md_weather_test,ets_forecast,xlim = c(2014,2020),xlab = "Years",ylab = "Avg_temp")






##############################################################################################

#### determining forecast using ARIMA model 
###exploratory data analysis : 
ggAcf(md_weather_ts,lag.max = 48)
ggPacf(md_weather_ts)
####try out models : 
ndiffs(md_weather_train) ; nsdiffs(md_weather_train)
################################################
ARIMA1<-auto.arima(md_weather_train,trace=TRUE,ic='bic')
summary(ARIMA1)

####model selection :  Arima(0,0,1)(1,1,0):ARIMA(1,0,1)(2,1,0):Arima(3,0,1)(4,1,0)

temp_ARIMA1<-Arima(md_weather_train,order=c(0,0,1),seasonal = c(1,1,0)) 
summary(temp_ARIMA1)#AIC=1436.47 #BIC=1447.7 #RMSE=2.338081
temp_ARIMA2<-Arima(md_weather_train,order=c(1,0,1),seasonal=c(2,1,0))
summary(temp_ARIMA2)#AIC=1400.23 #BIC=1418.94 #RMSE=2.1819
temp_ARIMA3<-Arima(md_weather_train,order = c(3,0,1),seasonal=c(4,1,0))
summary(temp_ARIMA3)# AIC=1363.18 BIC=1396.16 #rmse=2.0068


##### residual diagnostics 
checkresiduals(temp_ARIMA1,lag=48)  
#exists a outlier
Box.test(temp_ARIMA1$residuals,type="Ljung-Box") #pvalue is greater than five percent so no autocorrelation problem
jarque.bera.test(temp_ARIMA1$residuals) #normally distributed
##PLOT DOESNOT SHOW ANY INCREASED FUNNEL SHAPES , SO NO TRANSFRMATION REQUIRED
ggPacf(temp_ARIMA1$residuals)###polyanna effect doesnot exists and shows significant spikes
##further ar terms needed to be added


###finding the forecast error for temp_ARIMA1
forecastarima<-function(x,h){forecast(x,model=temp_ARIMA1,h=h)}
md_weather_ts%>%tsCV(forecastarima,h=12)%>%window(start=c(2016,1))->testfce
mean(testfce^2,na.rm=TRUE)%>%sqrt()
#RMSE test = 2.3365
#point forecast : 
forecastarima1<-function(x,h){forecast(Arima(md_weather_ts,model = temp_ARIMA1,h=12))}
summary(forecastarima1())
plot(forecastarima1())
####plotting the actual test data with the ARIMA(0,0,1)(1,1,0) model : 
plotarimapred(md_weather_test,temp_ARIMA1,xlim = c(2014,2019),xlab = "Years",ylab="Avg_temp")

###residual diagnostic for temp_ARIMA3 (arima(3,0,2)(4,1,0))
checkresiduals(temp_ARIMA3)
Box.test(temp_ARIMA3$residuals,type="Ljung-Box")
jarque.bera.test(temp_ARIMA3$residuals)
ggPacf(temp_ARIMA3$residuals)
#still exists significant spikes in lag 24 and 36 so more AR terms needed to be added
####################

###Finding forecast error and point forecast with temp_arima3  : 
forecastarima2<-function(x,h){forecast(x,model=temp_ARIMA3,h=h)}
md_weather_ts%>%tsCV(forecastarima2,h=12)%>%window(start=c(2016,1))->testfce2
mean(testfce2^2,na.rm=TRUE)%>%sqrt()
#rmse=1.8440
####point forecast : 
forecastarima2<-function(x,h){forecast(Arima(md_weather_ts,model = temp_ARIMA3,h=12))}
summary(forecastarima2())
###plot against the test data : 
plotarimapred(md_weather_test,temp_ARIMA3,xlim = c(2014,2019),range.percent = 0.05,xlab = "Years",ylab="Avg_temp")



###extra 
checkresiduals(temp_ARIMA2)
Box.test(temp_ARIMA2$residuals,type="Ljung-Box")#no autocorrelation problem exist
jarque.bera.test(temp_ARIMA2$residuals)
ggPacf(temp_ARIMA2$residuals,lag=48) #still exists significant spike on lag 24,36,48
#transformation is required 

md_weather_clean<-tsclean(md_weather_ts)
md_weather_clean_train<-window(md_weather_clean,start=c(1989,1),end=c(2015,12))
auto.arima(md_weather_clean)
temp_ARIMA_clean1<-Arima(md_weather_clean_train,order=c(3,0,0),seasonal = c(4,1,0))
summary(temp_ARIMA_clean1)#AIC=1436.47 #BIC=1447.7 #RMSE=2.338081
checkresiduals(temp_ARIMA_clean1)


