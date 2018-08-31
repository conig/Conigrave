#' find_similar
#'
#' Takes in two vectors of strings. This function will return strings in the second set, which are similar to those in the first.
#' @param x a vector of strings.
#' @param y a vector of strings. Strings will be returned from y which are similar to those in x
#' @param threshold a numeric. Strings in y will be returned if they are less than this distance away from x
#' @return a vector containing strings from y, which are similar to those in x.
#' @export find_similar
#' @importFrom stringdist stringdist
#' @importFrom magrittr %>%

find_similar = function(x, y, threshold = 4) {
  results = lapply(x, function(i) {
    y[stringdist(i, y) < threshold]
  })  %>%  unlist
  results[order(results)] %>%
    unique %>%
    return()
}