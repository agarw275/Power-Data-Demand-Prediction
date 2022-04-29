library("reshape2")
#install.packages("ggplot2")                  # Install ggplot2 package
library("ggplot2")
#install.packages("plotly") 
library('plotly')
#install.packages("forecast") 
library('forecast')
library(lubridate)
library(readxl)
setwd("C:/Users/Taro/OneDrive - purdue.edu/Documents/Purdue/ABA/Assignments")

power_data <- read_excel('PowerData.xlsx')
decimal_date(ymd("2006-12-27"))
Consumption.TS <-ts(power_data$kW_Gen,start = c(2014,40),end = c(2015,30),frequency = 52)
plot(Consumption.TS,main = 'Power Consumption',ylab="Kw Generation")



Consumption.TS <-ts(power_data$kW_Gen, start = decimal_date(as.Date("2014-10-5")), end = decimal_date(as.Date("2015-7-19")),frequency = 52)
plot(Consumption.TS,main = 'Power Consumption',ylab="Kw Generation")


Cloud.TS <-ts(power_data$Cloud_Cover,start = c(2014,40),end = c(2015,30),frequency = 52)
plot(Cloud.TS,main = 'Cloud Cover',ylab="Cloud Cover")
Rail_trend <- ma((Cloud.TS ),order = 8,centre = T )
plot((Rail_trend ))
#


nValid <- 8                                                               # set holdout size
nTrain <- length(Cloud.TS) - nValid                                     # set training size
train.ts <- window(Cloud.TS, start = c(2014,40), end = c(2015,22))  #partion data
valid.ts <- window(Cloud.TS, start =  c(2015,23))



train.qm <- tslm(train.ts ~ trend )      
summary(train.qm)                                        #view model summary


