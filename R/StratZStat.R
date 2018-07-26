#' Calculate Geomorph's Z-Statistic for Sexual Shape Dimorphism within Groups
#'
#' @param Strata 
#' @param Coords 
#' @param Sex 
#'
#' @export

StratZStat <- function(Strata, Coords, Sex) {
  Levels <- levels(Strata)
  n <- nlevels(Strata)
  Results <- rep(NA_real_, n)
  for (i in 1:n) {
    level <- Levels[i]
    StratCoord <- Coords[Strata == level, , drop = F]
    StratSex <- Sex[Strata == level]
    Results[i] <- geomorphZStat(StratCoord, StratSex)
  }
  Results
}