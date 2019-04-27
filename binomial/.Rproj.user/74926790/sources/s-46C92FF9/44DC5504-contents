##Private Checker Functions

# title check_prob
# description determines if prob value is valid
# param prob: probability value
# return True or raises error (if prob is not between 0 and 1)
check_prob<-function (prob) {
  if (prob>=0 & prob<=1) {
    return (T)
  } else {
    stop("invalid prob value")
  }
}

# title check_trials
# description determines if trials value is valid
# param trials: number of trials
# return True or raises error (if trials is not non-negative)
check_trials<-function(trials){
  if (trials>=0){
    return(T)
  } else{
    stop('invalid trials value')
  }
}

# title check_success
# description determines if success value is valid
# param success: number of successes
# param trials: number of trials
# return True or raises error (if success>trials or success<0)
check_success<-function(success, trials){
  if (success >=0 & success<=trials){
    return(T)
  } else if (success<0){
    stop("invalid success value")
  } else{
    stop("success cannot be greater than trials")
  }
}
