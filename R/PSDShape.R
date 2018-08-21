#' Pooled Standard Deviations of Shape
#' 
#' The square root of the total variance of the coordinate's covariance matrix after overlaying male and female coordinates on top of each other.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#'
#' @export

PSDShape <- function(Coords, Sex) {
  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }
  Data <- data.frame(Sex)
  Data$Coords <- Coords
  LM <- lm(Coords ~ 1 + Sex, data = Data)
  DiffFM <- LM$coef[2, ]
  Coords[as.character(Sex) == "f", ] <- Coords[as.character(Sex) == "f", ] - DiffFM
  PooledSD <- sqrt(sum(diag(cov(Coords))))
  PooledSD
}
