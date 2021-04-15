
#Choosing one random stock
ID<-sample(1:length(Tix), 1)
# Some good IDs include 338 and 5
#ID<-338
ID<-249
choosenStock<- Tix[ID]
stockname<-Names[ID]
message("Stock choosen is: ", choosenStock)
message("Stock Name is: ", stockname)

dat<-getSymbols(choosenStock, from = '2020-01-01',symbol.lookup = TRUE,warnings = FALSE,auto.assign = TRUE)
START_year<-2020
exampleData <- ts(as.ts(get(choosenStock))[,1],start = c(START_year, 1), frequency = 251)
length(exampleData)

#fetching of general data from plot
start_Replicate <- exampleData[1]
end_Replicate <- exampleData[length(exampleData)]
trendReplicate <- (end_Replicate-start_Replicate)/ length(exampleData)
arima_estimator_Replicate <- arima(exampleData , order = c(1, 0, 0))
phiReplicate <- arima_estimator_Replicate$coef[1]
phiReplicate <- 0.9 #manual override of Phi - if Phi >0.95 too close to random walk
sigmaReplicate <- sqrt(arima_estimator_Replicate$sigma2)*2

data1 <- createData(length(exampleData),start_Replicate,phiReplicate,sigmaReplicate/2,trend=trendReplicate,logging=FALSE,type="trading-daily",time_deviation=c(40),deviation_percentage_until_turningpoint=c(0.15),deviation_duration_t=c(40), deviation_maximum_absolute=c(start_Replicate*0.7))



datats <- ts(data1,start = c(START_year, 1), frequency = getFrequency("trading-daily"))

globalmin<-min(exampleData,datats)
globalmax<-max(exampleData,datats)

plot(exampleData,ylab="",ylim=c(globalmin,globalmax),main="Blind comparison \n First data set",col="deeppink4",lwd=2)
plot(datats,ylab="",ylim=c(globalmin,globalmax),main="Blind comparison \n Second data set",col="deeppink4",lwd=2)
#lines(exampleData)

plot(dataTS1,ylab="",ylim=c(globalmin,globalmax),main="Blind comparison \n First data set",col="deeppink4",lwd=2)
plot(dataTS2,ylab="",ylim=c(globalmin,globalmax),main="Blind comparison \n Second data set",col="deeppink4",lwd=2)

globalmin<-min(dataTS2,dataTS1)
globalmax<-max(dataTS2,dataTS1)

dataTS1
