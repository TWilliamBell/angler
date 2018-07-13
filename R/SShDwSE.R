#' Calculate the Standard Error of the SShD (and SSD)
#'
#' The SShD or SSD is worth very little without a measure of error, this function runs a bootstrap in order to get a value for their standard errors.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Size Optional size argument, a numerical vector giving the size of each individual as a scalar quantity
#' @param rep The number of times that you wish the resampling to be done, defaults to 1000 but more is better except for very small samples.
#'
#' @export
#'
#' @examples
#' SShDwSE(Coords, Sex, Size, rep = 10000, Zeroed = FALSE)

SShDwSE <- function(Coords, Sex, Size = NULL, rep = 1000, ...) {
  Boot <- replicate(rep, bootSShD(Coords, Sex, Size, ...))
  if (!is.null(Size)) {
    SShD <- mean(unlist(Boot[1, ]))
    SShD.SE <- sd(unlist(Boot[1, ]))
    SSD <- mean(unlist(Boot[2, ]))
    SSD.SE <- sd(unlist(Boot[2, ]))
  }
  else {
    SShD <- mean(Boot)
    SShD.SE <- sd(Boot)
  }
  SShD.Results <- data.frame(Boot.SShD = SShD, SShD.SE = SShD.SE)
  if (!is.null(Size)) {
    SShD.Results$Boot.SSD <- SSD
    SShD.Results$SSD.SE <- SSD.SE
  }
  SShD.Results
}
