plot(arima.sim(model=list(ar=c(-1)), n=251,sd=1), main="Example arima.sim")
plot(ts(createData(251,0,-1.01,1), frequency = 1), main="Example Data Simulation Thesis")

plot(diff(arima.sim(model=list(ar=c(0.8)), n=251,sd=0.5)))
plot(diff(ts(createData(251,0,0.8,0.5), frequency = 1)))

start_time <- Sys.time()
x<-arima.sim(model=list(ar=c(0.8)), n=10000,sd=1)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
x<-ts(createData(10000,0,0.8,1), frequency = 1)
end_time <- Sys.time()
end_time - start_time


inv<-c()
for(k in 1:100){
  inv <- c(inv,k/100)
}

ns2 = rnorm(n)*(1:n)
plot.ts(ns2, main = "non stationary in variance")

nsteps<-c(251)
i<-1
#data <- createData(251,0,0,0,deviation_duration_t=c(sample(1:40,2)),deviation_maximum_absolute=c(sample(1:15,2)),deviation_percentage_until_turningpoint=c(sample(0:100,2))/100,time_deviation=c(sample(1:150,1),sample(160:251,1)))
deviation_duration_tx<-c(sample(1:nsteps[i]*0.2,2))
deviation_maximum_absolutex<-c(sample(-6:6,2))
deviation_percentage_until_turningpointx <- c(sample(0:100,2))/100
time_deviationx<-c(sample(1:nsteps[i]/2,1),sample(round(0.6*nsteps[i]):nsteps[i],1))


data <- createData(nsteps[i],0,0,0,deviation_duration_t=deviation_duration_tx,deviation_maximum_absolute=deviation_maximum_absolutex,deviation_percentage_until_turningpoint=deviation_percentage_until_turningpointx,time_deviation=time_deviationx) 

data <- createData(nsteps[i],0,0,0,deviation_duration_t=c(sample(1:round(nsteps[i]*0.2),2)),deviation_maximum_absolute=c(sample(-6:6,2)),deviation_percentage_until_turningpoint=c(sample(0:100,2))/100,time_deviation=c(sample(1:round(nsteps[i]/2),1),sample(round(0.6*nsteps[i]):nsteps[i],1)))
plot(ts(data,frequency=251))


install.packages("stargazer")
library(stargazer)
data <- createData(1000,0,0.6,2) 
arimam <- arima(data,order=c(1,0,0))
stargazer(arimam)

CI_lower_1 <- arimam$coef[1]-1.96*tidy(arimam)$std.error[1]
CI_upper_1 <- arimam$coef[1]+1.96*tidy(arimam)$std.error[1]

CI_lower_1
CI_upper_1




n<-1000
 deviation_duration_t
 c(sample(1:round(n*0.3),2))
 
 deviation_maximum_absolute
 boxplot(sample(-4:4,2))
 
 deviation_percentage_until_turningpoint
 min(sample(0:100,100)/100)
 
 
 time_deviation
 c(sample(1:round(n/2),1),sample(round(0.6*n):n,1))


 boxplot(sample(1:100,50)/100)
 boxplot(sample(101:250,50)/100)

 












