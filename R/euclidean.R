#' Euclidean Norm
#'
#' The way you calculate distances in R^n, aka the square root of the sum of squares.
#'
#' @export
#'
#' @param vector numerical vector of numbers
#'
#' @examples
#' all.equal(euclidean(c(3, 4)), 5)
#' > TRUE

euclidean <- function(vector) {
  (sum(vector^2))^0.5
}
