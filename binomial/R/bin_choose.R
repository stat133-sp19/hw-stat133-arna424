#' @title bin_choose
#' @description calculates number of combinations - how many different ways you can choose k items from n items set without repetition and without order
#' @param n: total number of items
#' @param k: number of items to be chosen
#' @return n choose k
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose<-function(n,k){
  if (k>n){
    stop("k cannot be greater than n")
  } else{
    return(factorial(n)/(factorial(k)*factorial(n-k)))
  }
}
