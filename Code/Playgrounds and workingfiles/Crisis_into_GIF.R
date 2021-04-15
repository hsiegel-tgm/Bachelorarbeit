#install.packages("magick") 
library(magick)


phi_start<-0
redc<-c(255/255,252/255,028/255,003/255,003/255,148/255,252/255)
greenc<-c(015/255,236/255,252/255,252/255,102/255,003/255,3/255)
bluec<-c(003/255,003/255,003/255,223/255,252/255,252/255,173/255)


for(i in 1:7){
  phi_start <- 0.15*i-0.15
  #without crisis
  data_no_crisis <- createData(251,10,phi_start,0.7)
  data_no_crisis <- ts(data_no_crisis,start = c(2020, 1), frequency = getFrequency("trading-daily"))
  plot(data_no_crisis)
  
  #with crisis
  data_crisis <- createData(251,10,phi_start,0.7,time_deviation=c(30,180),deviation_percentage_until_turningpoint=c(0.5,0.85),deviation_duration_t=c(50,40), deviation_maximum_absolute=c(0,20))
  data_crisis <- ts(data_crisis,start = c(2020, 1), frequency = getFrequency("trading-daily"))
  plot(data_crisis)
  
  minPlot<-min(data_crisis,data_no_crisis)
  maxPlot<-max(data_crisis,data_no_crisis)
  
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/crisisGIF/picture_0",i+10,"_1_CN.png",sep=""))
  plot(data_no_crisis,ylim=c(minPlot,maxPlot),type="l",lwd=1,main=paste("Simulated Data (phi = ",phi_start,")\n Without Crisis",sep=""),xlab="t",ylab="Simulated Data",col=rgb(redc[i],greenc[i],bluec[i]))
  dev.off()
  
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/crisisGIF/picture_0",i+10,"_2_C.png",sep=""))
  plot(data_crisis,ylim=c(minPlot,maxPlot),type="l",lwd=1,main=paste("Simulated Data (phi = ",phi_start,")\n With Crisis",sep=""),xlab="t",ylab="Simulated Data",col=rgb(redc[i],greenc[i],bluec[i]))
  lines(data_no_crisis,col=rgb(0.8,0.8,0.8))
  lines(data_crisis,col=rgb(redc[i],greenc[i],bluec[i]))
  dev.off()
}
 

#animate 
imgs <- list.files("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/crisisGIF", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1,loop = 0)
img_animated
image_write(image = img_animated,path = "C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/crisis_2.gif")
