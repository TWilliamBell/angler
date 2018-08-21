#' Calculate stratified Standard Errors for SShD and SSD
#'
#' The SShD mean is the raw Procrustes Distance without adjusting by a permutation test (the default for SShD calculations in this package performs the Permutation Test), and the SShD standard error is the standard error of that estimator.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#' @param Size A numerical vector giving the size of each individual as a scalar quantity, individuals should be in the same order as the rows of the Coords
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD, individuals should be in the same order as the rows of the Coords
#' @param rep Number of resamplings you wish to do
#'
#' @export

StratSShDwSE <- function(Coords, Sex, Size = NULL, Strata, rep = 1000, print.progress = T, logSSD = F, ...) {
  ## Does SShD and SSD bootstraps
  SShD <- StratSShDBoot(Coords, Sex, Strata, rep = rep, print.progress = print.progress)
  Results <- data.frame(Factors = SShD$Factors, SShD.Mean = SShD$SShD.Mean, SShD.SE = SShD$SShD.SE)
  if (!is.null(Size)) {
    SSD <- StratSSDBoot(Size, Sex, Strata, rep, log = logSSD, ...)
    Results$SSD.Mean <- SSD$SSD.Mean, 
    Results$SSD.SE <- SSD$SSD.SE
  }
  Results
}
