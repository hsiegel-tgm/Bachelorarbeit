data1 <- createData(251*2,0,0.6,0, seasonality = c(0,1,2,3,4,5,6,7,8,9,10,11), logging=TRUE)
datats1 <- ts(data1,start = c(2019, 1), frequency = getFrequency("trading-daily"))

data2 <- createData(251*2,24,0.6,0, seasonality = c(1,1,1,2,2,3,3,4,4,3,3,2), logging=TRUE)
datats2 <- ts(data2,start = c(2019, 1), frequency = getFrequency("trading-daily"))

data3 <- createData(251*2,17,0.6,0, seasonality = c(1,1,1,0,0,0,1,1,1,0.7,1,1), logging=TRUE)
datats3 <- ts(data3,start = c(2019, 1), frequency = getFrequency("trading-daily"))


plot(datats1,lwd=2,type="l",ylim=c(min(data1,data2,data3),max(data1,data2,data3)+4),main="Simulation\n Seasonality without AR(1) or error",xlab="t",ylab="Simulated values",col="darkorchid1")
lines(datats1,lwd=2,col="darkorange4")
lines(datats2,lwd=2,col="goldenrod")
lines(datats3,lwd=2,col="darkorange2")


text(2019.45, y = 12.5, labels = "seasonality: c(0,1,2,3,4,5,6,7,8,9,10,11)", cex = 0.8, col = "darkorange4", font=2)
text(2019.45, y = 20, labels ="seasonality: c(1,1,1,0,0,0,1,1,1,0.7,1,1)  ",cex=0.8, col = "darkorange2",font=2)
text(2019.45, y = 30, labels ="seasonality: c(1,1,1,2,2,3,3,4,4,3,3,2)    ", cex = 0.8, col = "goldenrod", font=2)





data1x <- createData(251*2,0,0.7,0.7, trend=0.02, seasonality = c(0,1,2,3,4,5,6,7,8,9,10,11), logging=TRUE)
datats1x <- ts(data1x,start = c(2019, 1), frequency = getFrequency("trading-daily"))

data2x <- createData(251*2,24,0.7,0.7, seasonality = c(1,1,1,2,2,3,3,4,4,3,3,2), logging=TRUE)
datats2x <- ts(data2x,start = c(2019, 1), frequency = getFrequency("trading-daily"))

data3x <- createData(251*2,17,0.7,0.7, seasonality = c(1,1,1,0,0,0,1,1,1,0.7,1,1), logging=TRUE)
datats3x <- ts(data3x,start = c(2019, 1), frequency = getFrequency("trading-daily"))


plot(datats1x,lwd=2,type="l",ylim=c(min(data1,data2,data3),max(data1,data2,data3)+4),main="Simulation\n Seasonality with AR(1) and error",xlab="t",ylab="Simulated values",col="darkorchid1")
lines(datats1x,lwd=2,col="darkorange4")
lines(datats2x,lwd=2,col="goldenrod")
lines(datats3x,lwd=2,col="darkorange2")
lines(datats1,lwd=1,col="dimgrey")
lines(datats2,lwd=1,col="dimgrey")
lines(datats3,lwd=1,col="dimgrey")




