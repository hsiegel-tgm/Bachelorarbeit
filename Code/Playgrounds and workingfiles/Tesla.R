#install.packages("quantmod") #required to load data from yahoo
require("quantmod")

#Retrieving Tesla Stock
choosenStock<-"TSLA"
stockname<-"Tesla"
dat<-getSymbols(choosenStock, from = '2019-01-01', to='2020-12-31',symbol.lookup = TRUE,warnings = FALSE,auto.assign = TRUE)
exampleData <- ts(as.ts(TSLA)[,1],start = c(2019, 1), frequency = 251)

#How it would look without dividing in two trends:

simulation <- createData(length(exampleData),exampleData[1],0.9,sqrt(sd(exampleData)),trend=(exampleData[length(exampleData)]-exampleData[1])/length(exampleData),logging=FALSE,type="trading-daily")
simulation <- ts(simulation,start = c(2019, 1), frequency = getFrequency("trading-daily"))

counter<-9
png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(exampleData,lwd=1,type="l",main="Data Simulation of Stock Data\n Tesla Stock",xlab="t",ylab="Opening Value",col=rgb(0.8,0,0.5))
lines(simulation,col=rgb(0,130/255,130/255),lwd=1)
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Simulation (Basic)"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0,130/255,130/255)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()

#Dividing into two straight trends (phi and sd = 0) for purposes of showing the logic
data_pt1_trend_only <- createData(251,exampleData[1],0,0,trend=(exampleData[251]-exampleData[1])/251,logging=FALSE,type="trading-daily")
data_pt2_trend_only <- createData(length(exampleData)-251,exampleData[251],0,0,trend=(exampleData[length(exampleData)]-exampleData[251])/(length(exampleData)-251),logging=FALSE,type="trading-daily")
datats_trend <- ts(c(data_pt1_trend_only,data_pt2_trend_only),start = c(2019, 1), frequency = getFrequency("trading-daily"))

png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(exampleData,lwd=1,type="l",main="Data Simulation of Stock Data\n Tesla Stock",xlab="t",ylab="Opening Value",col=rgb(0.8,0,0.5))
lines(datats_trend,col=rgb(0,54/255,200/255),lwd=2)
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Trend Divided"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0,54/255,200/255)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()

#Simulating data for real (phi = 0.9, sd according to Tesla Data)
data_pt1 <- createData(251,exampleData[1],0.9,sqrt(sd(exampleData[1:251])),trend=(exampleData[251]-exampleData[1])/251,logging=FALSE,type="trading-daily")
data_pt2 <- createData(length(exampleData)-251,exampleData[251],0.95,sqrt(sd(exampleData[251:length(exampleData)])),trend=(exampleData[length(exampleData)]-exampleData[251])/(length(exampleData)-251),logging=FALSE,type="trading-daily")
datats2 <- ts(c(data_pt1,data_pt2),start = c(2019, 1), frequency = getFrequency_AR1("trading-daily"))

png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/Comparison/picture_",choosenStock,"_",counter,".png",sep=""))
plot(exampleData,lwd=1,type="l",main="Data Simulation of Stock Data\n Tesla Stock",xlab="t",ylab="Opening Value",col=rgb(0.8,0,0.5))
lines(datats_trend,col=rgb(0,54/255,200/255),lwd=1)
lines(datats,col=rgb(30/255,130/255,14/255),lwd=2)
lines(simulation,col=rgb(0.8,0.8,0.8))
legend("topleft",legend=c(paste("Real Data (",stockname,")",sep=""), "Trend Divided","Simulated Data (Divided Trend)","Simulated Data (Basic)"),text.font=4,col=c(rgb(0.8,0,0.5), rgb(0,54/255,200/255),rgb(30/255,130/255,14/255),rgb(0.8,0.8,0.8)),  lty=1:1,cex=0.6)
counter<-counter+1
dev.off()
