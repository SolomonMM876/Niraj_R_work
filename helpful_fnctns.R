###Catch NA values###
catch.na<-function(x){
  miss<-any(is.na(x))
  if(miss){
    message("The following observations are missing:")
    print(which(is.na(x)))
  }
  return(miss)
}

###Calculate Confidence Interval#####

calculateCI <- function(x, alpha =0.05){
  xbar <- mean(x)
  s <- sd(x)
  n <- length(x)
  half.width <- qt(1-alpha/2, n-1)*s/sqrt(n)
  # Confidence Interval
  CI <- c(xbar - half.width , xbar + half.width)
  return(CI)
}

se.sd.mean.n<-function(i){
  i<-na.omit(i)
  seresult<-sd(i)/sqrt(length(i))
  sdresult<-sd(i)
  meanresult<-mean(i)
  lengthresult<-length(i)
  #store results in vector
  vec<-c(seresult,sdresult,meanresult,lengthresult)
  names(vec)<- c('SE','SD','Mean','n')
  
  return(vec)
}