data1 <- createData(251,0,0.6,1,trend=-0.05)
datats1 <- ts(data1,start = c(2020, 1), frequency = getFrequency("trading-daily"))
data2 <- createData(251,0,0.6,1,trend=0,logging=TRUE)
datats2 <- ts(data2,start = c(2020, 1), frequency = getFrequency("trading-daily"))
data3 <- createData(251,0,0.6,1,trend=0.05)
datats3 <- ts(data3,start = c(2020, 1), frequency = getFrequency("trading-daily"))

xx<-0:251
trend1 <- xx*0.05
trend2 <- xx*-0.05
trend3 <- xx*0
trend1ts <- ts(trend1,start = c(2020, 1), frequency = getFrequency("trading-daily"))
trend2ts <- ts(trend2,start = c(2020, 1), frequency = getFrequency("trading-daily"))
trend3ts <- ts(trend3,start = c(2020, 1), frequency = getFrequency("trading-daily"))




#data2 <- createData(251,0,1,1,trend=0.05,logging=TRUE)
#datats2 <- ts(data2,start = c(2020, 1), frequency = getFrequency("trading-daily"))

#plot(datats2,lwd=1,type="l",main="Simulation\n Random walk",xlab="t",ylab="Simulated values",col="dodgerblue4")



plot(datats1,lwd=2,type="l",ylim=c(min(data1,data2,data3),max(data1,data2,data3)+2),main="Simulation\n AR(1) with trend",xlab="t",ylab="Simulated values",col="darkorchid1")

lines(trend1ts,lwd=2,col="darkgrey",lty=1)
lines(trend2ts,lwd=2,col="darkgrey",lty=1)
lines(trend3ts,lwd=2,col="darkgrey",lty=1)
lines(datats1,lwd=2,col="dodgerblue4")
lines(datats2,lwd=2,col="dodgerblue2")
lines(datats3,lwd=2,col="deepskyblue")

legend("topleft", legend=c("Positive Trend", "No Trend","Negative Trend","Linear Trend without AR(1) parameter"),col=c("deepskyblue", "dodgerblue2","dodgerblue4","darkgrey"), lty=c(1,1,1,1),text.font=3,cex=0.5)


text(2020.14, y = 5, labels = "mu = 0, phi = 0, sigma = 1", cex = 0.8, col = "deeppink", font=2)
text(2020.16, y = 15, labels = "mu = 10, phi = 0.5, sigma = 1", cex = 0.8, col = "blueviolet",font=2)
text(2020.16, y = 25, labels = "mu = 20, phi = 0.9, sigma = 1", cex = 0.8, col = "darkorchid1", font=2)


