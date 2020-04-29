#Tuberculosis Death Data
data <- read.csv(file.choose())
library(forecast)
#Creating the time seires 
#Male Death
tseries_male <- ts(data$No..of.TB.Death.for.Male,frequency = 18, start = c(1995,1))
#Female Death 
tseries_female <- ts(data$No..of.TB.Death.for.Female, frequency = 18, start = c(1995,1))
#Overall Death 
tseries_overall <- ts(data$Total.No..of.TB.Death,frequency = 18,start = c(1995,1))

#Graphs 
graph1_male <-plot(tseries_male,xlab="Year", ylab="No. of Male Deaths")
graph1_male
graph1_female <- plot(tseries_female, xlab="Year", ylab="No. of Female Deaths")
graph1_female
graph1_overall <- plot(tseries_overall,xlab="Year", ylab="No. of Total Deaths")
graph1_overall

#AUTO ARIMA 
#Male 
arima_male <- auto.arima(tseries_male)
forecast_male <- forecast(arima_male, h=5*18)
graph2_male <- plot(forecast_male,xlab = "Year",ylab = "No.of Male Deaths")
#Female 
arima_female <- auto.arima(tseries_female)
forecast_female <- forecast(arima_female,h=5*18)
graph2_female <- plot(forecast_female,xlab = "Year",ylab = "No.of Female Deaths")
#Overall 
arima_overall <- auto.arima(tseries_overall)
forecast_overall<- forecast(arima_overall,h=5*18)
graph2_overall <- plot(forecast_overall,xlab = "Year",ylab = "No. of Total Death")
#MEAN 
mean_male <- meanf(tseries_male)
accuracy(mean_male)
mean_overall <- meanf(tseries_overall) 
accuracy(mean_overall)
mean_female <- meanf(tseries_female)
accuracy(mean_female)

























