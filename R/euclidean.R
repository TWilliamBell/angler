#' Euclidean Norm
#' 
#' The way you calculate distances in R^n, aka the square root of the sum of squares.
#'
#' @param vector numerical vector of numbers
#'
#' @examples
#' euclidean(c(1,2,3.2))
euclidean <- function(vector) {
  (sum(vector^2))^0.5
}