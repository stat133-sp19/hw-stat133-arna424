#' @title bin_mean
#' @description calculates mean
#' @param trials: number of trials
#' @param prob: probability value
#' @return mean using trials and probability
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean<-function(trials,prob){
  if (check_trials(trials) & check_prob(prob)){
    return(aux_mean(trials,prob))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}

#' @title bin_variance
#' @description calculates variance
#' @param success: number of trials
#' @param trials: probability value
#' @return variance using trials and probability
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance<-function(trials,prob){
  if (check_trials(trials) & check_prob(prob)){
    return(aux_variance(trials,prob))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}

#' @title bin_mode
#' @description calculates mode
#' @param success: number of trials
#' @param trials: probability value
#' @return mode using trials and probability
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode<-function(trials,prob){
  if (check_trials(trials) & check_prob(prob)){
    return(aux_mode(trials,prob))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}

#' @title bin_skewness
#' @description calculates skewness
#' @param success: number of trials
#' @param trials: probability value
#' @return skewness using trials and probability
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness<-function(trials,prob){
  if (check_trials(trials) & check_prob(prob)){
    return(aux_skewness(trials,prob))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}

#' @title bin_kurtosis
#' @description calculates kurtosis
#' @param success: number of trials
#' @param trials: probability value
#' @return kurtosis using trials and probability
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis<-function(trials,prob){
  if (check_trials(trials) & check_prob(prob)){
    return(aux_kurtosis(trials,prob))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  }
}
