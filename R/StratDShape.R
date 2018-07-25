#' Effect Size of Sexual Shape Dimorphism by Stratum
#' 
#' Roughly based on Cohen's d, a common effect size statistic, another way of representing sexual shape dimorphism that is more readily comparable between studies, calculated by group.
#'
#' @param Strata 
#' @param Coords 
#' @param Sex 
#'
#' @export

StratDShape <- function(Strata, Coords, Sex) {
  Results <- StratSShD(Coords, Strata, Sex)$SShD/StratPooledSD(Strata, Coords, Sex)
  Results
}