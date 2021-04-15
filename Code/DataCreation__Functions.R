#########################################################################################################################################################
##################################################################### FUNCTIONS ######################################################################### 
############################################################### H. Siegel (h1554048() ################################################################### 
#########################################################################################################################################################

#########################################################################################################################################################
#################                               Function createData                                                      ################################
#                                                                                                                                                       #
# This function creates an AR(1)-process based on multiple specifications. It returns a list of values, which is not yet a timeseries object            #
#                                                                                                                                                       #
# Required Parameters:                                                                                                                                  #
#    n = total length of the data set                                                                                                                   #
#    phi = value for auto-regressive parameter (order 1)                                                                                                #
#                                                                                                                                                       #
# Optional Parameters:                                                                                                                                  #
#    mu = long-running mean of the process (default 0)                                                                                                  #
#    sigmasq = variance of the error (default 1)                                                                                                        #
#    trend = trend in mu for each iteration (default 0 - no trend)                                                                                      #
#    type = frequency type, must be known for seasonality (default trading-daily)                                                                       #
#    logging = if set to TRUE, information messages will be printed for the user, such as warnings and progress notes (default FALSE)                   #
#    seasonality = vector of defined absolute seasonality values - must match the assumptions based on the frequency type (default empty)               #
#                  if no vector is specified, no seasonality is assumed                                                                                 #
#    mu = long-running mean of the process (default 0)                                                                                                  #
#                                                                                                                                                       #
# Parameters required for deviation / shocks / crisis                                                                                                   #
#    time_deviation = vector of starting times of the deviations (triangles) - can be one or multiple; default empty                                    #
#    deviation_duration_t = duration of each respective crisis                                                                                          #
#    deviation_percentage_until_turningpoint = specification of percentage until when minimum / maximum is reached (defines the form of the triangle)   #                                                                                           #
#    deviation_maximum_absolute = absolute minimum / maximum of the respective crisis - defines if boom or bust                                         #
#                                                                                                                                                       #
# Parameters required for special innovations:                                                                                                          #
#    innovation_type = type used for innovations (constant, trend-decreasing, trend-increasing, random, custom)                                         #
#                      constant requires ONLY sigmasq                                                                                                   #         
#                      trend-decreasing/trend-increasing requires sd_min and sd_max and ignores sigmasq                                                 #         
#                      random requires requires sd_min and sd_max and ignores sigmasq                                                                   #         
#                      custom requires ONLY custom_innov and ignores sigmasq                                                                            #         
#    custom_innov = custom innovations (variance) which are to be used (default empty)                                                                  #
#    sd_min/sd_max = specifying the minimum and maximum sd desired as sigma for the error when random innovations OR a trend in innovations is used     # 
#                                                                                                                                                       #
#                                                                                                                                                       #
# Returns: Simulated data set                                                                                                                           #
#                                                                                                                                                       #
#########################################################################################################################################################

#Function 
createData <- function(n,mu=0,phi,sigmasq=1,trend=0,type="trading-daily",logging=FALSE,seasonality=c(),time_deviation=c(),deviation_percentage_until_turningpoint=c(),deviation_duration_t=c(), deviation_maximum_absolute=c(),innovation_type="constant",custom_innov=c(),sd_min=0, sd_max=2) {
  #preparing return vector
  dataset_artificial <- c()
  
  #calculating variance based on sd
  var_min<-sd_min*sd_min
  var_max<-sd_max*sd_max
  
  #Providing intial user information
  if(logging==TRUE){
    message("Welcome. I am starting your data process...")
    message(paste("You choose ",type," for performing your calculations",sep=""))
    if(type=="trading-daily"){
      message(paste("One year will have 251 observations. With your choosen n of ",n,", this will be ",round(n/251,digits=2)," years.",sep=""))
      message(paste("Seasonality expected monthly, with 12 distinct season corrections (absolute). Your seasonality provided has ",length(seasonality), " seasons provided.",sep=""))
    } else if (type=="quaterly"){
      message(paste("One year will have 4 observations. With your choosen n of ",n,", this will be ",round(n/4,digits=2)," years.",sep=""))
      message(paste("Seasonality expected quaterly, with 4 distinct season corrections (absolute). Your seasonality provided has ",length(seasonality), " seasons provided.",sep=""))
    } else if (type=="daily"){
      message(paste("One year will have 365 observations. With your choosen n of ",n,", this will be ",round(n/365,digits=2)," years.",sep=""))
      message(paste("Seasonality expected monthly, with 12 distinct season corrections (absolute). Your seasonality provided has ",length(seasonality), " seasons provided.",sep=""))
    }else if (type=="yearly"){
      message(paste("One year will have 1 observation. With your choosen n of ",n,", this will be ",round(n/1,digits=2)," years.",sep=""))
      message(paste("No Seasonality expected.",sep=""))
    }else if (type=="monthly"){
      message(paste("One year will have 12 observations. With your choosen n of ",n,", this will be ",round(n/12,digits=2)," years.",sep=""))
      message(paste("Seasonality expected monthly, with 12 distinct season corrections (absolute). Your seasonality provided has ",length(seasonality), " seasons provided.",sep=""))
    }
  }
  
  #Checking if a seasonality must be applied
  seasonality_provided <-assess_Seasonality(type,seasonality)
  if(logging==TRUE){
    message(paste("Seasonality was confirmed to be: ",seasonality_provided,sep=""))
  }
  
  #Checking if a custom innoavtion vector (one that must be checked at every iteration) was wished & must be applied
  custom_innov_provided <- FALSE
  
  if(innovation_type=="constant"){
    #setting no innovation trend
    var_trend <- 0
  }else if(innovation_type=="trend-increasing"){
    #calculating the innovation trend (linear)
    var_trend <- (var_max - var_min)/n 
    #setting the starting value to the minimum variance
    sigmasq <- var_min
  }else if(innovation_type=="trend-decreasing"){
    #calculating the innovation trend (linear, decreasing)
    var_trend <- -(var_max - var_min)/n 
    #setting the starting value to the maximum variance
    sigmasq <-var_max
  }else if(innovation_type=="random"){
    #calling random innovation function to receive random innovations which are constant for some time
    custom_innov <- get_random_innovations(n,var_min,var_max) 
    #setting no innovation trend
    sd_trend <- 0
    #setting custom innovations provided to TRUE so that as of now the custom_innov vector is used
    custom_innov_provided <- TRUE
    if(logging==TRUE){
      message(paste("Random innovations were created"))
    }
  }else if(innovation_type=="custom"){
    #setting no innovation trend
    sd_trend <- 0
    #Checking if the length of the custom innovations matches the n, and if so then they are accepted
    if(length(custom_innov)==n){
      #setting custom innovations provided to TRUE so that as of now the custom_innov vector is used
      custom_innov_provided <- TRUE
    }else{
      sigmasq<-0
      if(logging==TRUE){
        message(paste("Custom innovations not matching length -> constant variance of 0 used"))
      }
    }
  }
  
  #For the first iteration, beta_0 is calculated based on mu and phi
  beta <- betaCalc(mu,phi,trend,0)
  if(logging==TRUE){
    message("First Beta was calculated to be: ", beta)
  }

  #Calculating the value for Y_1 -> if seasonality is to be considered, this was included. Otherwise, the first starting point will be at the specified mean + an error 
  if(seasonality_provided==TRUE){
    datapoint <-  mu+get_saisonal_effect(type,seasonality,1) + rnorm(1,0,sigmasq)
  }else{
    datapoint <-  mu + rnorm(1,0,sigmasq)
  }
  #adding the first point to the data set
  dataset_artificial <- c(dataset_artificial,datapoint)
  #setting the Y_t-1 value initially
  Ytm1_value <- datapoint
  
  #setting the counter for the deviations - this is the counter for the deviation vector, and will just be increased for each deviation!
  counter_deviation <-1
  #checking if a deviation (triangle information for crisis) is desired; if yes, deviation_given will be TRUE
  deviation_given <- FALSE
  if(length(time_deviation)>0){
    deviation_given <- TRUE
    if(logging==TRUE){
      message(paste("Deviation request was received. Total deviations expected: ",length(time_deviation),sep=""))
    }    
  }
  #defaulting all further used parameters for the crisis / deviation calculations
  deviation_active <- FALSE
  deviation_data <- c()
  deviation_active_counter<-0
  
  #Looping from 2 to the total length. First value was already created, therefore starting at 2
  for(i in 2:n) {
    #checking if deviation (e.g. crisis / boom) is expected to start here
    if(deviation_given && i == time_deviation[counter_deviation]){
      if(logging==TRUE){
        message("deviation requested at ",i) 
      }
      #fetching deviation data - creation of triangle based on requested deviation
      deviation_data <- trend_deviation_triangle(deviation_duration_t[counter_deviation],deviation_maximum_absolute[counter_deviation],initial_height=Ytm1_value,percentage_until_turningpoint=deviation_percentage_until_turningpoint[counter_deviation], logging=logging)
      if(logging==TRUE){
        message("deviation data retreived with length: ",length(deviation_data))
      } 
      
      #setting the CURRENT deviation to active, with initial values
      deviation_active <- TRUE
      deviation_active_counter<-1
      initial_height_deviation <- Ytm1_value
      deviation_length <- length(deviation_data)
      counter_deviation<-counter_deviation+1 #increasing overall counter to as of now check for NEXT deviation

      #error checking: should deviation data be too small or have errors - deviation will be ignored
      if(length(deviation_data)<2 || is.na(deviation_data)){
        if(logging==TRUE){
          message("Error detected. Last ongoing deviation not accounted for.")
        } 
        deviation_active<-FALSE #ignore deviation
      }
      
      #to avoid array out of bounds error in next iteration
      if(length(time_deviation)==counter_deviation-1){
        counter_deviation<-length(time_deviation)#setting counter to last item in deviation vector
      }
    }
    
    #Adjusting the mu based on potential seasonality effect, if applicable
    if(seasonality_provided==TRUE){
      adjusted_mu <-  mu + get_saisonal_effect(type,seasonality,i) #get_saisonal_effect used to retrieve absolute value on seasonality at this point t
    }
    else{
      adjusted_mu <-  mu
    }
    
    #Adjusting the mu based on potential crisis data (absolute), if applicable
    if(deviation_active==TRUE){
      adjusted_mu <- adjusted_mu + deviation_data[deviation_active_counter] - initial_height_deviation #adding the crisis absolute value - the initial value, so that only the Delta of the crisis is added
      deviation_active_counter <- deviation_active_counter+1 #increasing counter
      #checking if deviation was fully "used up", if yes, deviation was finished and deviation_active set to FALSE
      if(deviation_active_counter==deviation_length){
        deviation_active<-FALSE
        if(logging==TRUE){
          message("deviation data ended at point ",i)
        }
      }
    }
    
    #Calculation of Data Point via AR(1) process; calling helper function betaCalc
    datapoint <-  phi * Ytm1_value + betaCalc(adjusted_mu,phi,trend,i) # also the trend will be considered here
      
    #Addition of the error in this separate step 
    if(custom_innov_provided == FALSE){
      datapoint <- datapoint + rnorm(1,0,(sigmasq+sd_trend*i))
    }else{
      datapoint <- datapoint + rnorm(1,0,(custom_innov[i]))
    }
    
    #adding the data point to the data set  
    dataset_artificial <- c(dataset_artificial,datapoint)
    
    #saving the data point as Ytm1 value for the next iteration
    Ytm1_value <- datapoint

    #Update regarding calculation progress for user, if desired
    if(logging==TRUE && i%%(n/4) == 0){
      message("Calculation done: ",((i/n))*100,"%")
    }
  }
  
  if(logging==TRUE){
    message("Now I am done. Returning data set with total length of: ",length(dataset_artificial))
  }
  
  #returning simulated data set.
  return(dataset_artificial)
}

###################################################################################################################################################
#################                               Function assess_Seasonality                                                      ##################
#                                                                                                                                                 #
# This function assesses if the provided seasonality vector matches the assumption taken based on the frequency.                                  #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    type = frequency type (trading-daily, daily, monthly, quaterly and yearly)                                                                   #
#    seasonality = seasonality vector, required to confirm the length of the vector                                                               #
#                                                                                                                                                 #
# Returns:                                                                                                                                        #
#    TRUE = seasonality vector length matches the assumption taken based on the frequency                                                         #
#    FALSE = sseasonality vector length does not match the assumption taken based on the frequency                                                #
#                                                                                                                                                 #
###################################################################################################################################################
assess_Seasonality <- function(type,seasonality){
  seasonality_correct <- FALSE
  if(type=="trading-daily" && length(seasonality)==12){
    seasonality_correct<- TRUE 
  } else if (type=="quaterly" && length(seasonality)==4){
    seasonality_correct<- TRUE
  } else if (type=="daily" && length(seasonality)==12){
    seasonality_correct<- TRUE 
  }else if (type=="monthly" && length(seasonality)==12){
    seasonality_correct<- TRUE 
  }
  return(seasonality_correct)
}

###################################################################################################################################################
#################                               Function getFrequency                                                      ########################
#                                                                                                                                                 #
# This function returns the number of data points per year for the given frequency types                                                          #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    type = default trading-daily, frequency type (trading-daily, daily, monthly, quaterly and yearly)                                            #
#                                                                                                                                                 #
###################################################################################################################################################
getFrequency <- function(type="trading-daily") {
  if(type=="trading-daily"){
    freq <- 251
  } else if (type=="quaterly"){
    freq <- 4
  } else if (type=="daily"){
    freq <- 365
  }else if (type=="yearly"){
    freq <- 1
  }else if (type=="monthly"){
    freq <- 12
  }else{
    freq <- NA
  }
  return(freq)
}

###################################################################################################################################################
#################                               Function trend_abs                                                      ###########################
#                                                                                                                                                 #
# This function returns the absolute value of a trend depending on t (time passed) and the increase of each step                                  #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    trend = increase of each step t                                                                                                              #
#    t = time passed since trend in place                                                                                                         #
#                                                                                                                                                 #
###################################################################################################################################################
trend_abs <- function(trend,t) {
  return(trend*t)
}    

###################################################################################################################################################
#################                               Function get_random_innovations                                        ###########################
#                                                                                                                                                 #
# This function returns a vector of random innovations based on a specified length.                                                               #
# The random innovations are between the min and maximum specified values, and change by chance, whereas the probability grows with each step     #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    n = length of the desired vector of innovations                                                                                              #
#    sd_min = minimum value of the innovations                                                                                                    #
#    sd_max = maximum value of the innovations                                                                                                    #
#                                                                                                                                                 #
###################################################################################################################################################
get_random_innovations <- function(n,sd_min, sd_max) {
  
  # preparing the return vector
  innov<-c()
  
  # calculating the stepwise increase of threshold based on n, so that around 4-6 different values occur within the specified n 
  prob_step<-100/(n*4)
  
  #setting the intial innovation right between the minimum and the maximum value (by adding the spread to the minimum value)
  current_innov <- ((sd_max - sd_min)/2)+sd_min
  
  #setting initial duration of initial innovation to 1
  current_duration<-1
  
  for(i in 1:n){
    # calculating a random value between 1 and 100 - this will be compared with the treshold
    random_draw<-sample(100,1)
    
    # calculating the threshold value; the longer (current_duration) the span of same innovation is, the higher the treshold 
    comparison_value <- round(prob_step*current_duration)

    #checking if the comparison value was reached (as the comparison value grows - the other value must be below not above the comparison value)
    if(random_draw <= comparison_value){
      #calculating next applicable duration (between min and max)
      current_innov<-sample((sd_max - sd_min)*10,1)/10+sd_min
      #resetting innovation length
      current_duration<-0
    }
    
    #adding innovation information (current chosen innovation)
    innov<-c(innov,current_innov)
    
    #increasing duration to one more
    current_duration <- current_duration+1
  }
   
  #return vector with length n
  return(innov)
}    

###################################################################################################################################################
#################                               Function get_saisonal_effect                                                      #################
#                                                                                                                                                 #
# This function returns the absolute value of the seasonality depending on which Sn corresponds to a certain point in time t.                     #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    type = desired mean of the AR(1) process                                                                                                     #
#    seasonality = vector of seasonality absolutes                                                                                                #
#    startseason = specified first season where first point of the time series t was happening, default to 1                                      #
#    t = point in time where seasonality shall be returned, usually used as a counter                                                             #
#                                                                                                                                                 #
# Assumed Length of each seasonality:                                                                                                             #
#     daily: every 30 days                                                                                                                        #
#     trading daily: every 21 days                                                                                                                #
#     monthly: every time                                                                                                                         #
#     quaterly: every time                                                                                                                        #
#                                                                                                                                                 #
###################################################################################################################################################
get_saisonal_effect<- function(type, seasonality, t, startseason=1){
  addition <- 0 #only required for monthly and quaterly, will be set below
  
  if(type=="trading-daily"){
    #one year has 12 repeats, so if the seasonality calculated based on t will surpass 12, it will be regarded as a new year
    seasonalities_per_year<-12
    
    #seasonality length is 21, therefore it changes every 21 repeats / every 21 steps of t
    season_length <- 21
 
  } else if (type=="quaterly"){
    #one year has 4 repeats, so if the seasonality calculated based on t will surpass 4, it will be regarded as a new year
    seasonalities_per_year<-4
    
    #seasonality length is 1, therefore it changes every repeats / at every step of t
    season_length <- 1
    
    #correction variable needed as repeat is only 1
    addition <- -1 
  } else if (type=="daily"){
    #one year has 12 repeats, so if the seasonality calculated based on t will surpass 12, it will be regarded as a new year
    seasonalities_per_year<-12
    
    #seasonality length is 30, therefore it changes every 30 repeats / every 30 steps of t
    season_length <- 30
 
  }else if (type=="monthly"){
    #one year has 12 repeats, so if the seasonality calculated based on t will surpass 12, it will be regarded as a new year
    seasonalities_per_year<-12
    
    #seasonality length is 1, therefore it changes every repeats / at every step of t
    season_length <- 1
    
    #correction variable needed as repeat is only 1
    addition <- -1
  }
  
  #calculation of respective currently active seasonality (Sn) within vector
  indicator <- ((t-t%%season_length) / season_length  - trunc((t-t%%season_length) / season_length / seasonalities_per_year)*seasonalities_per_year)+addition

  #fetching the absolute value from the provided seasonality vector, returning this absolute value
  value <- seasonality[indicator+startseason]
  return(value)
}

###################################################################################################################################################
#################                               Function betaCalc                                                                 #################
#                                                                                                                                                 #
# This function calculates the required value for beta_0 based on the AR(1) parameters                                                            #
# If a trend was specified, the function calculates the beta_0 considering the trend as well                                                      #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    mu = desired mean of the AR(1) process                                                                                                       #
#    phi = specified auto regressive parameter for Y_t-1.                                                                                         #
#    trend = specified trend of each iteration, default zero                                                                                      #
#    t = required to calculate the absolute trend, usually used as a counter                                                                      #
###################################################################################################################################################
betaCalc <- function(mu,phi,trend=0,t=0) {
  #calculating the absolute required trend (based on the trend that the data should increase at each step and t, the counter)
  trd_abs <- trend_abs(trend,t)
  betaR <- (mu+trd_abs) - phi*(mu+trd_abs)
  return(betaR)
}  


###################################################################################################################################################
#################                               Function trend_deviation_triangle                                                 #################
#                                                                                                                                                 #
# This function returns absolute data points for a triangle. It was used to create crisis data.                                                   #
#                                                                                                                                                 #
# Parameters:                                                                                                                                     #
#    duration_t = total data points retrieved; should be integer                                                                                  #
#    maximum_absolute = target point at the highest or lowest point of the triangular shape.                                                      #
#    initial_height = starting AND end-point for this triangular shape, default 0                                                                 #
#    percentage_until_turningpoint = indicating, after which percentage of duration, the maximum value should be reached, examples see below:     #
#           if 0: triangle will immediately jump to maximum_absolute value and decay / increase stepwise to inital_height value                   #
#           if 0.5: triangle will go into one direction for the first half, reaching mid point in the middle and then into the other direction.   #
#           if 1: triangle will decay / increase stepwise to maximum_absolute but not return to initial_height value                              #
#    logging = default FALSE, indicates if messages are desired that explain code and status                                                      #
###################################################################################################################################################
trend_deviation_triangle <- function(duration_t, maximum_absolute,initial_height = 0,percentage_until_turningpoint = 0.5, logging=FALSE) {
  datapoints_final<-c()
  if(is.na(duration_t)||is.na(maximum_absolute)||is.na(initial_height)||is.na(percentage_until_turningpoint)){
    if(logging == TRUE){
      message("Error expected, imperfect information received")
      datapoints_final<-c()
    }
  }else{
    #First logging - indicating initialization of function
    if(logging == TRUE){
      message(paste("Initializing trend deviation.",sep=""))
    }
    
    #calculating midpoint
    midpoint_t <- round(duration_t*percentage_until_turningpoint)
    
    if(logging == TRUE){
      message(paste("Midpoint was calculated to be at t = ",midpoint_t,sep=""))
    }
    
    ##
    ## Checking for general edge solutions of the "triangle" (with one 90 degree angle in the two angles that lie on the horizontal axis)
    ##
    end_point_identified <- FALSE
    start_point_identified <- FALSE
    
    #checking for "endpoint" solution (percentage until midpoint 100 or rounding results to this)
    if(midpoint_t == duration_t){
      if(logging == TRUE){
        message(paste("Endpoint solution identified. Result: only one slope from starting value towards target value will be produced, not a triangle form",sep=""))
      }
      end_point_identified <- TRUE
    }
    
    #checking for "startpoint" solution (percentage until midpoint 0 or rounding results to this)
    if(midpoint_t == 0){
      if(logging == TRUE){
        message(paste("Startpoint solution identified. Result: with t=1 the target value will be produced, then decaying back to startingpoint, not a triangle form",sep=""))
      }
      start_point_identified <- TRUE
    }
    
    # Calling the support function trend_deviation_line based on inputs.
  
    if(end_point_identified == TRUE){
      if(logging == TRUE){
        message(paste("As endpoint was identifed, calling trend_deviation_line function with parameters:  duration_t = ",duration_t," // maximum_absolute = ",maximum_absolute," // initial_height = ",initial_height,sep=""))
      }
      #Calling function with the overall expected number of data points (duration_t), requesting that it increases just until the given maximum and specifiying it starts at the initial given value
      datapoints_final <-trend_deviation_line(duration_t, maximum_absolute, initial_height)
    } else if (start_point_identified == TRUE){
      if(logging == TRUE){
        message(paste("As startpoint was identifed, calling trend_deviation_line function with parameters:  duration_t = ",duration_t," // maximum_absolute = ",initial_height," // initial_height = ",maximum_absolute,sep=""))
      }
      #Calling function with the overall expected number of data points (duration_t), requesting that it starts at the target value immediatley and it increase just until the given initial height value (backwards call)
      datapoints_final <- trend_deviation_line(duration_t, initial_height, maximum_absolute)
    } else{
      if(logging == TRUE){
        message(paste("As usual triangle was identifed, calling trend_deviation_line FIRST with parameters:  duration_t = ",midpoint_t," // maximum_absolute = ",maximum_absolute," // initial_height = ",initial_height,sep=""))
        message(paste("As usual triangle was identifed, calling trend_deviation_line SECOND with parameters:  duration_t = ",duration_t-midpoint_t," // maximum_absolute = ",initial_height," // initial_height = ",maximum_absolute,sep=""))
      }
      #Calling function twice. First section from initial value to maximum with the steps until midpoint. Second from maximum value back to initial heigth with the remaining steps after midpoint
      line1 <- trend_deviation_line(midpoint_t, maximum_absolute, initial_height)
      line2 <- trend_deviation_line(duration_t-midpoint_t, initial_height,maximum_absolute)
      #adding these two lines together
      datapoints_final <- c(line1,line2)
    }
    
    #returning data sets
    if(logging == TRUE){
      message(paste("Returning data with length of ",length(datapoints_final),sep=""))
    }
  }

  return(datapoints_final)
}

################################################################################################################################
#################                               Function trend_deviation_line                               ####################
#                                                                                                                              #
# This function returns absolute data points for a line between two specified heights and a given length t.                    #
# It was used to create one positive or negative slope of the triangle.                                                        #
#                                                                                                                              #
# Parameters:                                                                                                                  #
#    duration_t = total data points retrieved (points on line); should be integer                                              #
#    maximum_absolute = target point at the end of this line; targetpoint will be included in return data set as last entry    #
#    initial_height = starting point from this line; startingpoint is NOT included in return data set                          #
################################################################################################################################
trend_deviation_line <- function(duration_t, maximum_absolute,initial_height = 0) {
  line_data <-c() # creating empty vector
  t_x <- duration_t #- 1 ##would we calculate -1 here, then the first point would be the initial height. This is however NOT done, thus this is just a variable re-declaration ## 
  h_y <- maximum_absolute - initial_height #calculating the overall height that needs to be changed
  step_each <- h_y/t_x #calculating the change of trend in each t
  
  #line_data <- c(line_data,initial_height) # adding the first point which would be the initial height. This is however NOT done, as this code is commented out
  last <- initial_height #starting at the initial height
  
  for(i in 1:t_x) { #loop from 1 to t
    line_data <- c(line_data,last+step_each) #taking one "step" towards the maximum; adding this to the vector
    last <- last+step_each #saving last step taken in order to start from there in the next iteration
  }
  return(line_data) # returning the straight line
}

###################################################################################################################################
#################                               Function from_absolute_to_timeline                               ##################
#                                                                                                                                 #
# This function returns the corresponding time format (e.g. 2020.02 / 2020.08, ...) from t (integer / counter) and a start value. #
# The function especially considers the frequency type, default here is trading daily.                                            #
# For example, if start is 2020 with frequency = 12 and t = 6, then the returned value will be 2020.5                             #
#                                                                                                                                 #
# Parameters:                                                                                                                     #
#    absolute_t = total data points retrieved (points on line); should be integer                                                 #
#    type = defined frequency types (daily, trading-daily, monthly, quaterly, yearly)                                             #
#    start = starting point from the time series, usually a year, can be also a time decimal (e.g. 2020.4)                        #
###################################################################################################################################
from_absolute_to_timeline<-function(absolute_t,type="trading-daily",start=2020){
  #Requesting the frequency per Year information (e.g. 251 for trading-daily)
  frequencyperYear<-getFrequency(type)
  
  #returning the absolute value, that is the start year specified plus the absolute value t divided by the frequency to obtain the decimal information (e.g. 2020+0.5 = 2020.5 = 30.06.2020)
  return(start+absolute_t/frequencyperYear)
}