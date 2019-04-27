#' @title bin_distribution
#' @description calculates probability distribution in a table
#' @param trials: total number of trials
#' @param prob: probability of success
#' @return data.frame of success and probability
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution<-function(trials,prob){
  success<-c(0:trials)
  probability<-c()
  for (i in c(0:trials)){
    probability[i+1]<-bin_probability(i,trials,prob)
  }
  dat<-data.frame(success,probability)
  class(dat)<-c("bindis","data.frame")
  return(dat)
}

#' @export
plot.bindis<-function(dat){
  barplot(
    height=dat$probability,
    names.arg = dat$success,
    xlab="successes",
    ylab="probability"
  )
}
