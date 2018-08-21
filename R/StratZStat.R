#' Calculate Geomorph's Z-Statistic for Sexual Shape Dimorphism within Groups
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD, individuals should be in the same order as the rows of the Coords
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
    Results[i] <- ZStat(StratCoord, StratSex)
  }
  Results
}
