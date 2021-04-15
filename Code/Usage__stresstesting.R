install.packages("broom")
library(broom) 

#setting mains for plots depending on type
#1 = normal // 2 = arima.sim // 3 = trend // 4 = crisis  //  5 = seasonality // 6 = non constant innovations
mains<-c("Estimation Results of Simulated Time Series \n Used for simulation: Created Function","Estimation Results of Simulated Time Series \n Used for simulation: Arima.Sim")
mains<-c(mains, "Estimation Results of Simulated Time Series \n Used for simulation: Created Function with trend")
mains<-c(mains, "Estimation Results of Simulated Time Series \n Used for simulation: Created Function with crisis")
mains<-c(mains, "Estimation Results of Simulated Time Series \n Used for simulation: Created Function with seasonality")
mains<-c(mains, "Estimation Results of Simulated Time Series incl Seasonality \n Used for simulation: Created Function with seasonality")
mains<-c(mains, "Estimation Results of Simulated Time Series  \n Used for simulation: Created Function with trend in innovations")
mains<-c(mains, "Estimation Results of Simulated Time Series  \n Used for simulation: Created Function with random innovations")
mains2<-c("Absolute Estimation Deviation of Phi \n Used for simulation: Created Function")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi \n Used for simulation: Arima.Sim")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi \n Used for simulation: Created Function with trend")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi \n Used for simulation: Created Function with crisis")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi \n Used for simulation: Created Function with seasonality")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi incl Seasonality \n Used for simulation: Created Function with seasonality")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi  \n Used for simulation: Created Function with trend in innovations")
mains2<-c(mains2, "Absolute Estimation Deviation of Phi  \n Used for simulation: Created Function with random innovations")

#measuring run time
start_time <- Sys.time()

#Calling the function once for each option!
callMe(1500,1)
callMe(1500,2)
callMe(1500,3)
callMe(1500,4)
callMe(1500,5)
callMe(1500,6)
callMe(1500,7)
callMe(1500,8)

#measuring time, printing required time
end_time <- Sys.time()
end_time - start_time

#
callMe <- function(samplingsize=200,counter_weiche=1){
  #setting all vectors and variables to defaults
  overallinbounds_ML <-c()
  overallinbounds_CSS <-c()
  overallinbounds_CSSML <-c()
  
  overalldelta_ML <-c()
  overalldelta_CSS <-c()
  overalldelta_CSSML <-c()
  
  #specification of lengths of data sets which are requested
  nsteps<-c(100,250,500,750,1000,1500,2000,2500) 
  
  #performing the logic for each different data set length
  for(i in 1:length(nsteps)){
    #setting defaults
    inbounds_ML <-0 
    inbounds_CSS<-0
    inbounds_CSSML<-0 
    errors<-0

    #drawing random sigma and phi
    for(j in 1:samplingsize){
      phi<-sample(1:100,1)/100
      sigma<-sample(1:250,1)/100
      
      #this is required for the arima.sim as it does not accept phi = 1 (and then to always compare same with same)
      if(phi==1){
        phi<-0.99999
      }
      
      #Data creation depending on the choosen data characteristics
      if (counter_weiche==1) { 
        #simple AR(1) process with custom function
        data <- createData(nsteps[i],0,phi,sigma) 
      } else if (counter_weiche==2) {  
        #simple AR(1) process with arima.sim function
        data <- arima.sim(model=list(ar=c(phi)),sd=sigma, n=nsteps[i])
      } else if  (counter_weiche==3) {
        #AR(1) process with trend (see bachelor thesis for details of trend specification)
        data <- createData(nsteps[i],0,phi,sigma,trend=sample(1:100,1)/1000)
      } else if  (counter_weiche==4) { 
        #AR(1) process with crisis (see bachelor thesis for details of crisis specification)
        data <- createData(nsteps[i],0,phi,sigma,deviation_duration_t=c(sample(1:round(nsteps[i]*0.3),2)),deviation_maximum_absolute=c(sample(-4:4,2)),deviation_percentage_until_turningpoint=c(sample(0:100,2))/100,time_deviation=c(sample(1:round(nsteps[i]/2),1),sample(round(0.6*nsteps[i]):nsteps[i],1)))
      } else if  (counter_weiche==5) { 
        #AR(1) process with crisis (see bachelor thesis for details of seasonalities specification
        data <- createData(nsteps[i],0,phi,sigma, seasonality = sample(1:20,12)/2)
      } else if  (counter_weiche==7) {
        #AR(1) process with increasing trend (see bachelor thesis for details of innovation specification
        data <- createData(nsteps[i],0,phi,sigma,innovation_type="trend-increasing",sd_min=sample(1:100,1)/100,sd_max=sample(101:250,1)/100)
      } else if  (counter_weiche==8) {
        #AR(1) process with random trend (see bachelor thesis for details of innovation specification
        data <- createData(nsteps[i],0,phi,sigma,innovation_type="random",sd_min=sample(1:100,1)/100,sd_max=sample(101:250,1)/100)
      }  
      
      if(length(data)>9){
        result=tryCatch({
            #estimation 1st order arima
            fitArima_ML<-arima(data,order=c(1,0,0),method="ML")
            fitArima_CSS<-arima(data,order=c(1,0,0),method="CSS")
            fitArima_CSSML<-arima(data,order=c(1,0,0),method="CSS-ML")

        },error=function(error_condition){
          #message("myerror:    ",error_condition)
          errors<-errors+1
        })

        if(is.nan(tidy(fitArima_ML)$std.error[1])||is.nan(tidy(fitArima_CSS)$std.error[1])||is.nan(tidy(fitArima_CSSML)$std.error[1])){
          errors <- errors+1
          #message("Arima error in estimation // loop ID: ",j)
        }else{
          #retrieving estimates for ML option
          phiEstimate_1 <-fitArima_ML$coef[1]
          sdEstimate_1 <- tidy(fitArima_ML)$std.error[1]
          
          #calculating CI for ML option
          CI_lower_1 <- fitArima_ML$coef[1]-1.96*tidy(fitArima_ML)$std.error[1]
          CI_upper_1 <- fitArima_ML$coef[1]+1.96*tidy(fitArima_ML)$std.error[1]
          
          #calculating if phi used for simulation is within CI
          if(phi>=CI_lower_1 && phi<=CI_upper_1){
            inbounds_ML = inbounds_ML +1
          }
          
          #adding result to overall ML vales if phi used for simulation is within CI
          if(phiEstimate_1>0){ # else error case 
            overalldelta_ML <- c(overalldelta_ML,(phi-phiEstimate_1))
          }
          
          #retrieving estimates for CSS option
          phiEstimate_2 <-fitArima_CSS$coef[1]
          sdEstimate_2 <- tidy(fitArima_CSS)$std.error[1]
          
          #calculating CI for CSS option
          CI_lower_2 <- fitArima_CSS$coef[1]-1.96*tidy(fitArima_CSS)$std.error[1]
          CI_upper_2 <- fitArima_CSS$coef[1]+1.96*tidy(fitArima_CSS)$std.error[1]
          
          #calculating if phi used for simulation is within CI
          if(phi>=CI_lower_2 && phi<=CI_upper_2){
            inbounds_CSS = inbounds_CSS +1
          }
          
          #adding result to overall CSS vales if phi used for simulation is within CI
          if(phiEstimate_2>0){ # else error case 
            overalldelta_CSS <- c(overalldelta_CSS,(phi-phiEstimate_2))
          }
          
          #retrieving estimates for CSSML option
          phiEstimate_3 <-fitArima_CSSML$coef[1]
          sdEstimate_3 <- tidy(fitArima_CSSML)$std.error[1]
          
          #calculating CI for CSSML option
          CI_lower_3 <- fitArima_CSSML$coef[1]-1.96*tidy(fitArima_CSSML)$std.error[1]
          CI_upper_3 <- fitArima_CSSML$coef[1]+1.96*tidy(fitArima_CSSML)$std.error[1]
          
          #calculating if phi used for simulation is within CI
          if(phi>=CI_lower_3 && phi<=CI_upper_3){
            inbounds_CSSML = inbounds_CSSML +1
          }
          
          #adding result to overall CSSML vales if phi used for simulation is within CI
          if(phiEstimate_3>0){ # else error case 
            overalldelta_CSSML <- c(overalldelta_CSSML,(phi-phiEstimate_3))
          }
        }
      }
    }
    
    #calculating the % of inbounds for each method (not taking into consideration the "errors")
    overallinbounds_ML <- c(overallinbounds_ML,inbounds_ML/(samplingsize-errors))
    overallinbounds_CSS <- c(overallinbounds_CSS,inbounds_CSS/(samplingsize-errors))
    overallinbounds_CSSML <- c(overallinbounds_CSSML,inbounds_CSSML/(samplingsize-errors))
    
    #Output indicating if a deviation was identified or not (in case inconclusive in the graphical view)
    if(overallinbounds_ML[i]==overallinbounds_CSS[i]&&overallinbounds_CSS[i]==overallinbounds_CSSML[i]){
      message("No deviation between methods found for N = ",nsteps[i])
    }else{
      message("Deviation between methods found for N = ",nsteps[i])
    }
  }

  #calculations preparing for plots (min / maximum values, ...)
  x<-nsteps
  y<-overallinbounds_ML
  globalmin<-min(overallinbounds_ML,overallinbounds_CSS,overallinbounds_CSSML)
  globalmax<-max(overallinbounds_ML,overallinbounds_CSS,overallinbounds_CSSML)
  
  #saving results into pictures
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/datausage5/picture_",samplingsize,"_",round(phi,2)*100,"_",round(sigma,2)*100,"_",counter_weiche,".png",sep=""))
  plot(x,y,xlab="n (data points per time series)",ylim=c(globalmin,globalmax),ylab="Real Phi within Estimation CI (%)", main=mains[counter_weiche],col="white")
  points(x=nsteps,y=overallinbounds_ML,col=rgb(235/255,52/255,164/255),pch=20,cex=3)
  points(x=nsteps,y=overallinbounds_CSS,col=rgb(59/255,217/255,48/255),pch=20,cex=2)
  points(x=nsteps,y=overallinbounds_CSSML,col=rgb(1/255,1/255,1/255),pch=20,cex=1)
  legend("bottomright", legend=c("ML", "CSS","CSSML"), col=c(rgb(235/255,52/255,164/255),rgb(59/255,217/255,48/255),rgb(1/255,1/255,1/255)), pch=20:20, cex=1.2)
  dev.off()
  
  png(file=paste("C:/Users/h.siegel/OneDrive - Accenture/EBG/TPL/99_Personal_Info/Personal_Hannah/BA/img/datausage5/picture_boxplot",samplingsize,"_",round(phi,2)*100,"_",round(sigma,2)*100,"_",counter_weiche,".png",sep=""))
  boxplot(cbind(overalldelta_ML,overalldelta_CSS,overalldelta_CSSML),names=c("ML","CS","CSSML"),main=mains2[counter_weiche],ylab="Deviation (Simulated Phi - Estimated Phi)")
  dev.off()
}


