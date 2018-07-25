#' Effect Size of Sexual Shape Dimorphism
#' 
#' Roughly based on Cohen's d, a common effect size statistic, another way of representing sexual shape dimorphism that is more readily comparable between studies.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @export

DShape <- function(Coords, Sex) {
  SShD <- SShDLM(Coords, Sex, Zeroed = T)
  PSD <- PooledSDShape(Coords, Sex)
  if (PSD == 0) {
    warning("Pooled standard deviation is zero.  Sample size too small?")
    return(NA_real_)
  }
  Effect <- SShD/PSD
  Effect
}