#' @title bin_probability
#' @description calculates number of combinations after checking if it is a valid calculation
#' @param success: total number of successes
#' @param trials: total number of trials
#' @param prob: probability of success
#' @return binomial probability or raise an error if parameters will not work
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability<-function(success,trials,prob){
  if (check_trials(trials) & check_prob(prob) & check_success(success,trials)){
    return(bin_choose(trials,success)*prob^success*(1-prob)^(trials-success))
  } else if (!check_trials(trials)){
    stop('invalid trials value')
  } else if (!check_prob(prob)){
    stop('invalid prob value')
  } else if (!check_success(success,trials)){
    stop('invalid success value')
  }
}
