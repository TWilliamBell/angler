#' Calculate stratified Standard Errors for SShD and SSD
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD
#' @param rep Number of resamplings you wish to do
#'
#' @export

StratSShDSSDwSE <- function(Coords, Sex, Size, Strata, rep = 1000) {
  ## Does SShD and SSD bootstraps
  SShD <- StratSShDBoot(Coords, Sex, Strata, rep)
  SSD <- StratSSDBoot(Size, Sex, Strata, rep)
  Results <- data.frame(Factors = SShD$Factors, SShD.Mean = SShD$SShD.Mean, SShD.SE = SShD$SShD.SE, SSD.Mean = SSD$SSD.Mean, SSD.SE = SSD$SSD.SE)
  Results
}
