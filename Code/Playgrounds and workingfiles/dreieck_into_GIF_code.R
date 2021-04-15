##
## Requires dreieck.R with functions trend_deviation_triangle AND trend_deviation_line
##

install.packages("magick") 
library(magick)


plot(trend_deviation_triangle(100,-100,10,0.5),type="l",lwd=2,main="Triangle Simulation",xlab="t",ylab="value",col=rgb(0.9,0.5,0.8))

redc<-c(1,1,0.75,0.75,0.5,0.5,0.25,0.25,0,0,0,0)
greenc<-c(0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1,1)
bluec<-c(0,0,0,0,0,0,0,0,0,0.25,0.5)

i<-1
i<-i+1
plot(c(rep(0,each=20),trend_deviation_triangle(100,100,10,0.1*(i-1))),type="l",lwd=2,main="Triangle Simulation",xlab="t",ylab="value",col=rgb(redc[i],bluec[i],greenc[i]))

sign<-"pos"
max<-30
for(i in 1:11){
  #datused <- dataresults[,i]
  #datats <- ts(datused,start = c(2019, 1), frequency = 251)
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/dreieckGIF/picture_",sign,"_0",i+10,".png",sep=""))
  plot(c(rep(0,each=20),trend_deviation_triangle(100,max,0,0.1*(i-1)),rep(0,each=20)),type="l",lwd=2,main="Triangle Simulation - Boom",xlab="t",ylab="value",col=rgb(redc[i],bluec[i],greenc[i]))
  dev.off()
}
sign<-"neg"
max<--30
for(i in 1:11){
  #datused <- dataresults[,i]
  #datats <- ts(datused,start = c(2019, 1), frequency = 251)
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/dreieckGIF/picture_",sign,"_0",i+10,".png",sep=""))
  plot(c(rep(0,each=20),trend_deviation_triangle(100,max,0,0.1*(i-1)),rep(0,each=20)),type="l",lwd=2,main="Triangle Simulation - Crisis",xlab="t",ylab="value",col=rgb(bluec[i],redc[i],greenc[i]))
  dev.off()
}


#animate 
imgs <- list.files("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/dreieckGIF", full.names = TRUE)
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 1,loop = 0)
img_animated
image_write(image = img_animated,path = "dreieck.gif")
