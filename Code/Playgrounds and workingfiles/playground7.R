
trends<-c()
dontdo<-c(66)
for (i in 1:length(Tix)){
  choosenStock<- Tix[i]
  if(i == 66 || i == 79){
    
  }else{
  out <- tryCatch(
    {
      # Just to highlight: if you want to use more than one 
      # R expression in the "try" part then you'll have to 
      # use curly brackets.
      # 'tryCatch()' will return the last evaluated expression 
      # in case the "try" part was completed successfully
      
      dat<-getSymbols(choosenStock, from = '2020-01-01',symbol.lookup = TRUE,warnings = FALSE,auto.assign = TRUE)
      START_year<-2020
      exampleData <- ts(as.ts(get(choosenStock))[,1],start = c(START_year, 1), frequency = 251)
      start_Replicate <- exampleData[1]
      end_Replicate <- exampleData[length(exampleData)]
      trendReplicate <- (end_Replicate-start_Replicate)/ length(exampleData)
      trends<-c(trends,trendReplicate)
      # The return value of `readLines()` is the actual value 
      # that will be returned in case there is no condition 
      # (e.g. warning or error). 
      # You don't need to state the return value via `return()` as code 
      # in the "try" part is not wrapped inside a function (unlike that
      # for the condition handlers for warnings and error below)
    } 
  )    }
}

mean(trends)
