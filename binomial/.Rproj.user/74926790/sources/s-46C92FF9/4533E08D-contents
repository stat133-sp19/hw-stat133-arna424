##Private Auxilary Functions

# title aux_mean
# description calculates mean
# param success: number of trials
# param trials: probability value
# return mean using trials and probability
aux_mean<-function(trials,prob){
  return(trials*prob)
}

# title aux_variance
# description calculates variance
# param success: number of trials
# param trials: probability value
# return variance using trials and probability
aux_variance<-function(trials,prob){
  return(trials*prob*(1-prob))
}

# title aux_mode
# description calculates mode
# param success: number of trials
# param trials: probability value
# return mode using trials and probability
aux_mode<-function(trials,prob){
  if (trials%%2 !=0) {
    return(c(round(trials*prob+prob),round(trials*prob+prob-1)))
  } else{
    return(round(trials*prob+prob))
  }
}

# title aux_skewness
# description calculates skewness
# param success: number of trials
# param trials: probability value
# return skewness using trials and probability
aux_skewness<-function(trials,prob){
  numerator<-1-2*prob
  denominator<-sqrt(trials*prob*(1-prob))
  return(numerator/denominator)
}

# title aux_kurtosis
# description calculates kurtosis
# param success: number of trials
# param trials: probability value
# return kurtosis using trials and probability
aux_kurtosis<-function(trials,prob){
  numerator<-1-6*prob*(1-prob)
  denominator<-trials*prob*(1-prob)
  return(numerator/denominator)
}
