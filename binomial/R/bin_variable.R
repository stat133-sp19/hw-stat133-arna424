#' @title bin_variable
#' @description extracts trials and probability values after using bin_cumulative
#' @param trials: total number of trials
#' @param prob: probability of success
#' @return list of trials and prob values
#' @export
#' @examples
bin_variable<-function(trials,prob){
  if(check_trials(trials) & check_prob(prob)) {
    l<-list(trials=trials,prob=prob)
    class(l)<-c("binvar")
    return(l)
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}

#' @export
print.binvar<-function(object){
  print('Binomial Variable')
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n- number of trials:",object[[1]])
  cat("\n- probability of success:",object[[2]])
  invisible(object)
}

#' @export
summary.binvar<-function(object){
  trials<-object[[1]]
  prob<-object[[2]]
  l<-list(trials=trials,
          prob=prob,
          mean=aux_mean(trials,prob),
          variance=aux_variance(trials,prob),
          mode=aux_mode(trials,prob),
          skewness=aux_skewness(trials,prob),
          kurtosis=aux_kurtosis(trials,prob))
  class(l)<-c("summary.binvar")
  return(l)
  invisible(object)
}

#' @export
print.summary.binvar<-function(object){
  print('Summary Binomial')
  cat("\n")
  cat("\n")
  cat("Parameters")
  cat("\n- number of trials:",object[[1]])
  cat("\n- probability of success:",object[[2]])
  cat("\n")
  cat("\n")
  cat("Measures")
  cat("\n- number of trials:",object[[3]])
  cat("\n- number of trials:",object[[4]])
  cat("\n- number of trials:",object[[5]])
  cat("\n- number of trials:",object[[6]])
  cat("\n- number of trials:",object[[7]])
  invisible(object)
}
