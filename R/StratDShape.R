#' Effect Size of Sexual Shape Dimorphism by Stratum
#' 
#' Roughly based on Cohen's d, a common effect size statistic, another way of representing sexual shape dimorphism that is more readily comparable between studies, calculated by group.
#' 
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD, individuals should be in the same order as the rows of the Coords
#'
#' @export

StratDShape <- function(Strata, Coords, Sex) {
  Results <- StratSShD(Coords, Strata, Sex)$SShD/StratPooledSD(Strata, Coords, Sex)
  Results
}
