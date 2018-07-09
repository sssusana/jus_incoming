#import libs 
library(ggplot2)
library(readr)
library(fma)
library(reshape2)
library(lubridate)
library(plotly)
library(magrittr)
library(zoo)
library(xts)
library(forecast)
library(TSstudio)
library(tseries)
library(aTSA)
library(ggsci)
library(ggpubr)
library(Rssa)

####import data####
incoming <- read.csv("~/Documents/GitHub/jus_incoming/Incoming.csv", sep=";")
class(incoming)
incoming <- incoming[2]
View(incoming)

head(incoming)
colnames(incoming)
class(incoming)

incoming_ts <- ts(incoming, start=c(2007, 1), end=c(2017,12), frequency = 12)
start(incoming_ts)
end(incoming_ts)
print(incoming_ts)

####Plotting####
text1 <- "Average Number of Proceedings"
par(mfrow=c(1,1))
par(cex.axis=0.9)
axis(1, at = seq(from=2007, by = 0.25))
inc_plot <-plot.ts(incoming_ts, main="Judicial Proceedings from 2007 to 2017", 
                     xlab="Date", ylab="Incoming Proceedings", type="l", col='black', axes = TRUE, )

abline(h=mean(incoming_ts), col='yellow', lwd='2')
legend("bottomright", inset = .05, text1,col='yellow',lty=1,ncol=1,cex=0.5,lwd=2.5)



####Seasonality Ana Decomposition####
#Analysing the variation by year, month, and monthly boxplots 
ts_seasonal(incoming_ts, type = 'box')
#Other tests & all
ts_seasonal(incoming_ts, type = 'normal')
ts_seasonal(incoming_ts, type = 'cycle')
ts_seasonal(incoming_ts, type = 'all')

#Heatmap
ts_heatmap(incoming_ts)
#Surface plot
ts_surface(incoming_ts)
ts_lags(incoming_ts, n_row = 6, lag.max = 36)


decompose(incoming_ts)
ts_decompose(incoming_ts, showline = T )
decompose(incoming_ts)

lambdaincoming<-BoxCox.lambda(incoming_ts)
# as box cox is <> 0,we're going to apply a box cox transformation to log data.

plot(diff(BoxCox(incoming_ts, lambdaincoming)))
acf(diff(BoxCox(incoming_ts, lambdaincoming)))
pacf(diff(BoxCox(incoming_ts, lambdaincoming)))

arima011011<-arima(x=incoming_ts,order = c(0,1,1), seasonal = c(0,1,1))
auto.arima(incoming_ts, stepwise = FALSE, trace = TRUE, max.order = 4)

#Test set vs train set 

datared <- window(incoming_ts, start=c(2007,1), end=c(2017,12))

h_out <- 24
split_incomingTS <- ts_split(incoming_ts, sample.out = h_out)
length(train)
length(test)


train <- split_incomingTS$train
train
test <- split_incomingTS$test
test

snaive(incoming_ts, h=6)

model1 <- forecast::forecast(auto.arima(train), h=12)

model2 <- forecast::forecast(arima(train, order = c(0,1,1), seasonal = c(0,1,1)), h=24)
model2_diag <- arima(x = incoming_ts, order = c(0,1,1), seasonal = c(0,1,1))

plot.ts(incoming_ts)
lines(incoming_ts-model2$residuals, col='red')

check_res(model2)
par(mfrow=c(1,1))

axis(2, seq(0, 40000, 10000), las=2)
plot(model2, fcol = "#1E4ABB" ) ##shadecols = c('#BEE5A5', '#76C3AC'))

#Residuals Diagnosis
plot(model2$residuals)
tsdiag(model2_diag)


model3 <- Arima(train, order = c(0,0,1), seasonal = c(0,0,1))
model3_fc <- forecast(model3, lead = h_out)

test_forecast(actual = incoming_ts, forecast.obj = model2, test = test)


teste1<-auto.arima(incoming_ts)
teste1$residuals
teste1$sigma2
teste1$aic


##Aplication of SSA
incoming_ts<-zooreg(1:132, frequency = 12, start = c(2007, 1))
class(co2)
class(incoming)


decomp_ssa <- ssa(incoming_ts)
decomp_ssa$window
decomp_ssa$length

summary(decomp_ssa)  
plot(decomp_ssa, type = "values")
plot(decomp_ssa, type = "vectors") # Eigenvectors
plot(decomp_ssa, type = "paired") # Pairs of eigenvectors
plot(wcor(decomp_ssa)) # w-correlation matrix plot


rec_ssa <-reconstruct(decomp_ssa, groups)


