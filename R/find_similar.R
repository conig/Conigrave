#' find_similar
#'
#' Takes in two vectors of strings. This function will return strings in the second set, which are similar to those in the first.
#' @param x a vector of strings.
#' @param y a vector of strings. Strings will be returned from y which are similar to those in x
#' @return a vector containing strings from y, which are similar to those in x.
#' @export find_similar
#' @importFrom stringdist stringdist
#' @importFrom magrittr %>%

find_similar = function(x, y, threshold = 4) {
  # if (length(x) > 1) {
    results = lapply(x, function(i) {
      y[stringdist(i, y) < threshold]
      
    })  %>%  unlist
  # } else{
  #   results = y[stringdist(x, y) < threshold]
  # }
  results[order(results)] %>%
    unique %>%
    return()
}
