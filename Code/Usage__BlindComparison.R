#install.packages("quantmod") #required to load data from yahoo
#install.packages("rvest") #required to read webpages
library(rvest)
library(quantmod)

#fetching S&P500 Tickers from Wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%   html() %>%   html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%   html_table()
SP500 <- SP500[[1]]
Tix <- SP500$Symbol
Names<-SP500$Security
if(length(Tix)>400){
  message("S&P 500 stock tickers were received, total number of stocks: ", length(Tix))
}

#Choosing one random stock from the Tix
ID<-sample(1:length(Tix), 1)
choosenStock<- Tix[ID]
stockname<-Names[ID]
message("Stock choosen is: ", choosenStock)
message("Stock Name is: ", stockname)

#Retrieving choosen stock data from 2020
dat<-getSymbols(choosenStock, from = '2020-01-01',symbol.lookup = TRUE,warnings = FALSE,auto.assign = TRUE)
START_year<-2020

#casting example Data into timeseries
exampleData <- ts(as.ts(get(choosenStock))[,1],start = c(START_year, 1), frequency = 251)

#fetching of general information from example Data
start_Replicate <- exampleData[1]
end_Replicate <- exampleData[length(exampleData)]
trendReplicate <- (end_Replicate-start_Replicate)/ length(exampleData)
arima_estimator_Replicate <- arima(exampleData , order = c(1, 0, 0))
phiReplicate <- arima_estimator_Replicate$coef[1]
## phiReplicate <- 0.85 #manual override of Phi possible here - if Phi >0.95 too close to random walk
sigmasqReplicate <- sqrt(arima_estimator_Replicate$sigma2)

#simulate data with same length and trend, as well phi and variance
data1 <- createData(length(exampleData),start_Replicate,phiReplicate/1.2,sigmasqReplicate,trend=trendReplicate,logging=FALSE,type="trading-daily")
datats <- ts(data1,start = c(START_year, 1), frequency = getFrequency("trading-daily")) #casing as times series

counter<-1
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(datats,ylim=c(min(datats,exampleData),max(datats,exampleData)),lwd=1,type="l",main="Data Simulation of Stock Data",xlab="t",ylab="Opening Value",col=rgb(0.5,0.5,0.8))
lines(exampleData,col=rgb(0.8,0,0.5))
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Simulated Data"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0.5,0.5,0.8)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()

#
# Idenfication of Crisis - step 1: Moving average
#

#calculating moving average
movingaverage_Data <-c()
movingaverage_Calculated<-c()
movinglength <- round(length(exampleData)/50)  
for(i in 1:length(exampleData)){#looping trough whole data set
  if(length(movingaverage_Data)<movinglength){#for the first few data points, no Moving Average is calculated (yet) 
    movingaverage_Data<-c(movingaverage_Data,exampleData[i])
  }else{#calculating the moving average by creating a vector with the desired length and adding the mean
    for(n in 1:movinglength-1){
      movingaverage_Data[n]<-movingaverage_Data[n+1]
    }
    movingaverage_Data[movinglength]<-exampleData[i]
  }
  movingaverage_Calculated<-c(movingaverage_Calculated,mean(movingaverage_Data)) #adding the mean to the data
}

#saving the picture for step (1)
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(datats,ylim=c(min(datats,exampleData),max(datats,exampleData)),lwd=1,type="l",main="Crisis Simulation Step 1\n Moving Average",xlab="t",ylab="Opening Value",col=rgb(1,1,1))
lines(exampleData,col=rgb(0.8,0,0.5))
dataMOV <- ts(movingaverage_Calculated,start = c(START_year, 1), frequency = getFrequency("trading-daily"))
lines(dataMOV,col=rgb(0.8,0,00),lwd=3)
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Moving Average of Real Data"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0.8,0,0)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()

#
# Idenfication of Crisis - step 2: Detrending
#

#creating a line for the trend (with phi=0 and sigma=0) only a linear function will be the result
trendReal <- createData(length(exampleData),start_Replicate,0,0,trend=trendReplicate,logging=FALSE,type="trading-daily")
trendReal <- ts(trendReal,start = c(START_year, 1), frequency = getFrequency("trading-daily"))
 
#deducting the trend from the moving average data 
diffTrend <- dataMOV - trendReal
diffTrend <- ts(diffTrend,start = c(START_year, 1), frequency = getFrequency("trading-daily"))

#defining the maximum and minimum borders of the data
maxDev<-mean(diffTrend)+sd(diffTrend)
minDev<-mean(diffTrend)-sd(diffTrend)

#saving the picture for step (2)
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(diffTrend,ylim=c(min(minDev,diffTrend),max(maxDev,diffTrend)),col=rgb(0.8,0.1,0.1),lwd=3,ylab="Moving Average minus linear trend",main="Crisis Simulation Step 2\n Detrended Moving Average")
abline(h=maxDev,col=rgb(0.3,0.3,0.3))
abline(h=minDev,col=rgb(0.3,0.3,0.3))
abline(h=mean(diffTrend),col=rgb(0.5,0.5,0.5),lty=2)
counter<-counter+1
dev.off()

#
# Idenfication of Crisis - step 3: Identifying points above / below threshold -> data for triangle complete
#

#setting all value to default
amount_deviation_identified<-0
start_deviation<-c()
max_deviation_pos<-c()
max_deviation_value<-c()
end_devation<-c()
currently_deviation_identified<-FALSE
max_deviation<-0
max_dev_pos<-0
sign_deviation<-c()

for(i in 1:length(diffTrend)){#looping trough detrended Moving average data
  if(diffTrend[i]>maxDev || diffTrend[i]<minDev){ #if the value is above the maximum OR below the minimum -> deviation
    #Checking if currently a deviation is known (then we are INSIDE the current one), if none is currently active -> new deviation reported!
    if(currently_deviation_identified==FALSE){
      currently_deviation_identified<-TRUE
      
      #counter of total deviations identified, just for completness
      amount_deviation_identified<-amount_deviation_identified+1
      
      #Deviation starting at time point i
      start_deviation<-c(start_deviation,i)
      
      #setting initial comparison value for maximum value
      max_deviation<-abs(diffTrend[i])
      max_dev_pos<-i
      
      #identiying boom or bust (sign positive or negative)
      if(diffTrend[i]>maxDev){
        sign_deviation<-c(sign_deviation,maxDev)#boom
      }else{
        sign_deviation<-c(sign_deviation,minDev)#bust
      }
    }
  }else if(currently_deviation_identified==TRUE){#if currently deviation is ongoing but value was NOT identified to be a maximum
    #Deviation ended at time point i
    end_devation<-c(end_devation,i)
    currently_deviation_identified<-FALSE
    
    #calculating the position of the maximum deviation as well as the value
    max_deviation_pos<-c(max_deviation_pos,max_dev_pos)
    if(sign_deviation[length(sign_deviation)]==maxDev){
      max_deviation_value<-c(max_deviation_value,max_deviation)
    }else{
      max_deviation_value<-c(max_deviation_value,-max_deviation)
    }
  }
  #if deviation is identified currently, check if this point is more than the maximum (maximum always saved absolute as difference to trend which is detrended at 0)
  if(currently_deviation_identified==TRUE){ 
    if(max_deviation < abs(diffTrend[i])){
      max_deviation<-abs(diffTrend[i])
      max_dev_pos<-i
    }
  }
}

#saving the picture for step (3) -- looping trough the results to plot the points
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(diffTrend,ylim=c(min(minDev,diffTrend),max(maxDev,diffTrend)),col=rgb(0.8,0.1,0.1),lwd=3,ylab="Moving Average minus linear trend",main="Crisis Simulation Step 3\n Identifiying Deviations (start, extemes, end)")
abline(h=maxDev,col=rgb(0.3,0.3,0.3))
abline(h=minDev,col=rgb(0.3,0.3,0.3))
abline(h=mean(diffTrend),col=rgb(0.5,0.5,0.5),lty=2)
 
for(i in 1:length(start_deviation)){
  points(from_absolute_to_timeline(start_deviation[i]), sign_deviation[i], type = "p",col="darkgoldenrod1",cex=1.3,pch=19)
  points(from_absolute_to_timeline(start_deviation[i]), sign_deviation[i], type = "p",col="black",cex=1.3,pch=1)
}
for(i in 1:length(end_devation)){
  points(from_absolute_to_timeline(end_devation[i]), sign_deviation[i], type = "p",col="darkgoldenrod1",cex=1.3,pch=19)
  points(from_absolute_to_timeline(end_devation[i]), sign_deviation[i], type = "p",col="black",cex=1.3,pch=1)
}
for(i in 1:length(max_deviation_pos)){
  points(from_absolute_to_timeline(max_deviation_pos[i]), max_deviation_value[i], type = "p",col="cornflowerblue",cex=1.3,pch=19)
  points(from_absolute_to_timeline(max_deviation_pos[i]), max_deviation_value[i], type = "p",col="black",cex=1.3,pch=1)
}
counter<-counter+1
dev.off()

#
# Idenfication of Crisis - step 4: Calculating all information required for triangles based on obtained data  
#

midpoints_deviation<-c()
length_deviation<-c()

#calculate midpoint and length
for(i in 1:length(start_deviation)){
  length_deviation<-c(length_deviation,end_devation[i]-start_deviation[i])
  midpoints_deviation<-c(midpoints_deviation,(max_deviation_pos[i]-start_deviation[i])/(end_devation[i]-start_deviation[i]))
}

#calculate real absolute (not based on detrended moving average)
target_deviation_points <-c()
for(i in 1:length(max_deviation_pos)){
  target_deviation_points<-c(target_deviation_points,exampleData[max_deviation_pos[i]])
}

#saving the picture for step (4) -- looping trough ALL results to plot the points
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(diffTrend,ylim=c(min(minDev,diffTrend),max(maxDev,diffTrend)),col=rgb(0.8,0.1,0.1),lwd=1,ylab="Moving Average minus linear trend",main="Crisis Simulation Step 4\n Linear Modelling of Deviations (triangles)")
abline(h=maxDev,col=rgb(0.3,0.3,0.3))
abline(h=minDev,col=rgb(0.3,0.3,0.3))
abline(h=mean(diffTrend),col=rgb(0.5,0.5,0.5),lty=2)

for(i in 1:length(start_deviation)){
  tr1<-trend_deviation_triangle(end_devation[i]-start_deviation[i], maximum_absolute=max_deviation_value[i],initial_height = diffTrend[start_deviation[i]],percentage_until_turningpoint = midpoints_deviation[i], logging=FALSE)
  tr1 <- ts(tr1,start = c(START_year, start_deviation[i]), frequency = getFrequency("trading-daily"))
  lines(tr1,lwd=3,col="darkgreen")
}

for(i in 1:length(start_deviation)){
  points(from_absolute_to_timeline(start_deviation[i]), sign_deviation[i], type = "p",col="darkgoldenrod1",cex=1.3,pch=19)
  points(from_absolute_to_timeline(start_deviation[i]), sign_deviation[i], type = "p",col="black",cex=1.3,pch=1)
}
for(i in 1:length(end_devation)){
  points(from_absolute_to_timeline(end_devation[i]), sign_deviation[i], type = "p",col="darkgoldenrod1",cex=1.3,pch=19)
  points(from_absolute_to_timeline(end_devation[i]), sign_deviation[i], type = "p",col="black",cex=1.3,pch=1)
}
for(i in 1:length(max_deviation_pos)){
  points(from_absolute_to_timeline(max_deviation_pos[i]), max_deviation_value[i], type = "p",col="cornflowerblue",cex=1.3,pch=19)
  points(from_absolute_to_timeline(max_deviation_pos[i]), max_deviation_value[i], type = "p",col="black",cex=1.3,pch=1)
}
counter<-counter+1
dev.off()

#correcting the data again for the selected length of the moving average, as otherwise crisis will start slightly too late
start_deviation<- start_deviation - movinglength
#correcting for possible starts at/below 1 to avoid out of bounds
for(i in 1:length(start_deviation)){
  if(start_deviation[i]<2){ 
    start_deviation[i]<-2
  }
}

#simulating a data set based on the information WITH crisis information
data1 <- createData(length(exampleData),start_Replicate,phiReplicate,sigmasqReplicate,trend=trendReplicate,logging=FALSE,type="trading-daily",time_deviation=start_deviation,deviation_percentage_until_turningpoint=midpoints_deviation,deviation_duration_t=length_deviation,deviation_maximum_absolute=target_deviation_points)
datats <- ts(data1,start = c(START_year, 1), frequency = getFrequency("trading-daily"))

#saving the picture with both data sets in comparing it.
counter<-picture_counter
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(datats,ylim=c(min(datats,exampleData),max(datats,exampleData)),lwd=1,type="l",main="Data Simulation of Stock Data",xlab="t",ylab="Opening Value",col=rgb(0.5,0.5,0.8))
lines(exampleData,col=rgb(0.8,0,0.5)) 
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Simulated Data"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0.5,0.5,0.8)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()

## Randomization
stock1<-sample(2,1)

if(stock1==1){
  dataTS1<-exampleData
  dataTS2<-datats
}else{
  dataTS1<-datats
  dataTS2<-exampleData
}

### Perform your testings here
        plot(dataTS2)
        plot(dataTS1)
        
        lines(dataTS1,col="red")
        length(dataTS1)
        length(dataTS2)
        
        #trend
        (dataTS1[length(dataTS1)] - dataTS1[1])/length(dataTS1)
        (dataTS2[length(dataTS2)] - dataTS2[1])/length(dataTS2)
        
        min(dataTS2)
        max(dataTS2)
        
        install.packages("tseries")
        library("tseries")
        adf.test(dataTS2, alternative = c("stationary"))
        adf.test(dataTS1, alternative = c("stationary"))
        
        datax <- createData(length(exampleData),0,0,1,trend=0,logging=FALSE,type="trading-daily")
        datatsx <- ts(datax,start = c(START_year, 1), frequency = getFrequency("trading-daily"))
        adf.test(datatsx, alternative = c("stationary"))
        adf.test(arima.sim(n = 251, list(order=c(1,0,0),ar = c(0.8897),sd = sqrt(0.1796))))
        
        sd(dataTS1)
        sd(dataTS2)
        
        mean(dataTS1)
        mean(dataTS2)
        
        median(dataTS1)
        median(dataTS2)
        
        acf(dataTS1) 
        acf(dataTS2)  
        
        install.packages("lmtest")
        library("lmtest")
        grangertest(dataTS1,dataTS2, order = 1)
        grangertest(dataTS2,dataTS1,order = 1)
        
        fitARIMA <- arima(dataTS2, order=c(1,0,0))
        fitARIMA
        fitARIMA <- arima(dataTS1, order=c(1,0,0))
        fitARIMA
        
        plot(diff(dataTS2),ylab="Differences", main="Differences Data Set 2")
        plot(diff(dataTS1),ylab="Differences", main="Differences Data Set 1")

### Resolution
if(stock1==1){
  message("Data TS1 is example Data (Real Data) and data TS2 is simulated")
}else{
  message("Data TS2 is example Data (Real Data) and data TS1 is simulated")
}
