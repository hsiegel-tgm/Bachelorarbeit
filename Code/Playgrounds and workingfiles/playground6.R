data <- get_random_innovations(250,0,5) 
plot(data,main="Random innovations", ylab="Custom SD",xlab="", pch=16,col="chartreuse3")
simdata <-createData(250,0,0,innovation_type="custom", custom_innov = data)
plot(ts(simdata,frequency=250),main="Resulting Simulated data\n Random Innovations",ylab="Simulated Values",lwd=1,col="aquamarine4")
plot(diff(ts(simdata,frequency=250)),main="Differences of data\n Random Innovations",ylab="Difference of Simulated Values",lwd=1,col="cadetblue4")
points(ts(1.96*data,frequency=250),col="chartreuse3",pch=16,cex=0.7)
points(ts(-1.96*data,frequency=250),col="chartreuse3",pch=16,cex=0.7)
abline(h=0,col="black",lwd=1)

simdata2 <-createData(250,0,0,2,innovation_type="constant")
plot(ts(simdata2,frequency=250),main="Resulting Simulated data\n Random Innovations",ylab="Simulated Values",lwd=1,col="aquamarine4")
plot(diff(ts(simdata2,frequency=250)),main="Differences of data\n Random Innovations",ylab="Difference of Simulated Values",lwd=1,col="cadetblue4")
points(ts(1.96*rep(sd(simdata),250),frequency=250),col="chartreuse3",pch=16,cex=0.7)
points(ts(-1.96*rep(sd(simdata),250),frequency=250),col="chartreuse3",pch=16,cex=0.7)
points(ts(rep(1.96*sqrt(sd(simdata)),250),frequency=250),col="red",pch=16,cex=0.7)
points(ts(rep(-1.96*sqrt(sd(simdata)),250),frequency=250),col="red",pch=16,cex=0.7)





 
simdata <-createData(500,0,0.9,innovation_type="trend-increasing", sd_min=0.5, sd_max=2.5)
plot(ts(simdata,frequency=500,start=2019),main="Resulting AR(1) Simulated data\n Increasing Innovations",ylab="Simulated Values",lwd=1,col="coral3")
plot(diff(ts(simdata,frequency=500,start=2019)),main="Differences of data\n Increasing Innovations",ylab="Difference of Simulated Values",lwd=1,col="coral1")

innov<-c(rep(1,200),rep(3,100),rep(0.4,200))
simdata <-createData(500,0,0.9,innovation_type="custom", custom_innov =innov)
plot(ts(simdata,frequency=500,start=2019),main="Resulting AR(1) Simulated data\n Custom Innovations (medium, high, low)",ylab="Simulated Values",lwd=1,col="hotpink4")
plot(diff(ts(simdata,frequency=500,start=2019)),main="Differences of data\n Increasing Innovations (medium, high, low)",ylab="Difference of Simulated Values",lwd=1,col="hotpink3")

differences<-diff(exampleData)
plot(abs(differences))/2
innov<-(((abs(differences))))
simdata <-createData(299,0,0.8,innovation_type="custom", custom_innov =innov)
plot(ts(simdata,frequency=299,start=2020),main="Resulting AR(1) Simulated data\n Custom Innovations (medium, high, low)",ylab="Simulated Values",lwd=1,col="hotpink4")
plot(diff(ts(simdata,frequency=299,start=2020)),main="Differences of data\n Increasing Innovations (medium, high, low)",ylab="Difference of Simulated Values",lwd=1,col="hotpink3")

lines(diff(exampleData),col="grey",cex=0.6,lwd=3)
lines(diff(ts(simdata,frequency=299,start=2020)),col="black")

globalmin<-min(diff(simdata),diff(exampleData))
globalmax<-max(diff(simdata),diff(exampleData))
plot(diff(ts(simdata,frequency=299,start=2020)),ylim=c(globalmin,globalmax),main="Blind Comparison\n Differences Plot 1",ylab="Differences",lwd=1,col="mediumpurple3")

plot(diff(exampleData),ylim=c(globalmin,globalmax),main="Blind Comparison\n Differences Plot 2",ylab="Differences",lwd=1,col="mediumpurple3")

