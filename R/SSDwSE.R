#' SSD with Standard Error estimated via a Bootstrap
#'
#' @param Size a numerical vector giving the size of each individual as a scalar quantity
#' @param Sex a character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the Size vector
#' @param Strata a character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SSD, individuals should be in the same order as the Size vector
#' @param rep number of times to perform the resampling procedure
#'
#' @export

SSDwSE <- function(Size, Sex, rep = 1000) {
  SSDBooted <- bootSSD(Size = Size, Sex = Sex, rep)
  SE <- sd(SSDBooted$t)
  M <- mean(SSDBooted$t)
  Results <- data.frame(SSD.Mean = M, SSD.SE = SE)
  Results
}
