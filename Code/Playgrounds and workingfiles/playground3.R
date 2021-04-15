plot(c(0,0,0,0,0,trend_deviation_triangle(10, 10,0,0.5),0,0,0,0,0),ylim=c(-2,11),ylab="",xlab="",main="Representation of Triangle logic",type="l",lty=1,lwd=2, col="gray1")
abline(h=0,col="gray65",lwd=2,lty=3)
points(10,10,col="forestgreen",pch=16)    
abline(v=10,col="dodgerblue3",lwd=1,lty=2)
abline(v=5,col="firebrick1",lwd=1,lty=2)
abline(v=15,col="firebrick1",lwd=1,lty=2)
points(5,0,col="darkmagenta",pch=16)    

text(12, y = 10, labels ="maximum: 10", cex = 0.8, col = "forestgreen", font=3,bg="white")
text(6, y = -0.5, labels ="start: 0", cex = 0.8, col = "darkmagenta", font=3,bg="white")
text(12.3, y = -1.4, labels ="percentage until \nturning point 50%", cex = 0.8, col = "dodgerblue3", font=3,bg="white")
text(10, y = 0.6, labels ="duration: 10", cex = 0.8, col = "firebrick1", font=3,bg="white")


#rgb definition
red <- c(255,255,255,127,0,0,0,0,0,127,255,255)/255
green <- c(0,127,255,255,255,255,255,127,0,0,0,0)/255
blue <- c(0,0,0,0,0,127,255,255,255,255,255,127)/255
red2 <- red
green2 <- green
blue2 <- blue
length(red)
length(green)
length(blue)

library(colorspace)
install.packages("colorspace") 
library(colorspace)

desaturate(rgb(0,0,1),amount=0.5)

plot(c(0,0,0,0,0,trend_deviation_triangle(100, 10,0,0.5),0,0,0,0,0),ylim=c(-15,15),ylab="",xlab="",main="Representation of Triangle logic\n Different values for percentage until midpoint",type="l",lty=1,lwd=2, col="gray1")
percentages<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
for (k in 1:11){
  lines(c(0,0,0,0,0,trend_deviation_triangle(100, -10,0,percentages[k]),0,0,0,0,0),col=lighten(rgb(red[k],green[k],blue[k]),amount=0.45),lwd=2)
  lines(c(0,0,0,0,0,trend_deviation_triangle(100, 10,0,percentages[k],logging=TRUE),0,0,0,0,0),col=rgb(red[k],green[k],blue[k]),lwd=2)
  text(k*10-5, y = 11.5 + (k%%2)*2 , labels =percentages[k], cex = 0.8, col = rgb(red[k],green[k],blue[k]), font=3,bg="white")
  text(k*10-5, y = -11.5 - (k%%2)*2 , labels =percentages[k], cex = 0.8, col=lighten(rgb(red[k],green[k],blue[k]),amount=0.35), font=3,bg="white")
  
} 
abline(h=0,col="dimgray",lwd=3)
text(0,8.5,label="positive crisis",srt=90,col="dimgray",font=3)
text(0,-8.5,label="negative crisis",srt=90,col="dimgray",font=3)

########## Now more fun fun fun ##########


data3 <- createData(251,0,0.6,1,time_deviation=c(51),deviation_percentage_until_turningpoint=c(0.5),deviation_duration_t=c(150),deviation_maximum_absolute=c(20))
datats3 <- ts(data3,start = c(2020, 1), frequency = getFrequency("trading-daily"))


k<-8
data3 <- createData(251,0,0.6,1,time_deviation=c(51),deviation_percentage_until_turningpoint=c(percentages[k]),deviation_duration_t=c(150),deviation_maximum_absolute=c(20))
datats3 <- ts(data3,start = c(2020, 1), frequency = getFrequency("trading-daily"))
plot(datats3,xlab="t",ylab="Simulated Values",main="Simulation incl. Crisis\n Examples for AR(1) with distinct crisis",type="l",lty=1,lwd=2, col="white")

dat1<-c(rep(0,51),trend_deviation_triangle(150,20,0,0.7),rep(0,50))
length(dat1)
datts1 <- ts(dat1,start = c(2020, 1), frequency = getFrequency("trading-daily"))

lines(datts1,col="gray",lwd=2)
lines(datats3,col=lighten(rgb(red[k],green[k],blue[k]),amount=0),lwd=2)
text(2020.23,17,cex=0.8,label="AR(1) process with:\n phi = 0.6, mu = 0, sigma = 1 \n \n crisis starting at t = 50 with duration = 150 \n percentage until midpoint: 70%",col=lighten(rgb(red[k],green[k],blue[k]),amount=0))



percentages<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
light<-c(1,0,1,1,1,1,1,1,1,1,1,1)
lwdx<-c(2,2,1,1,1,2,1,1,1,1,2,1)
for (k in 1:11){
  data3 <- createData(251,0,0.6,1,time_deviation=c(51),deviation_percentage_until_turningpoint=c(percentages[k]),deviation_duration_t=c(150),deviation_maximum_absolute=c(20))
  datats3 <- ts(data3,start = c(2020, 1), frequency = getFrequency("trading-daily"))
  lines(datats3,col=lighten(rgb(red[k],green[k],blue[k]),amount=light[k]),lwd=lwdx[k])
 # text(k*10-5, y = 11.5 + (k%%2)*2 , labels =percentages[k], cex = 0.8, col = rgb(red[k],green[k],blue[k]), font=3,bg="white")
#  text(k*10-5, y = -11.5 - (k%%2)*2 , labels =percentages[k], cex = 0.8, col=lighten(rgb(red[k],green[k],blue[k]),amount=0.35), font=3,bg="white")
  
} 

abline(h=0,col="dimgray",lwd=3)
text(0,8.5,label="positive crisis",srt=90,col="dimgray",font=3)
text(0,-8.5,label="negative crisis",srt=90,col="dimgray",font=3)



