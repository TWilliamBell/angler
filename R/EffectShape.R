#' Effect Size of Sexual Shape Dimorphism
#' 
#' A common effect size statistic, another way of representing sexual shape dimorphism that is possibly more readily comparable between studies.  Roughly analogous to the effect size measure D, but don't quote me on that.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#'
#' @export

EffectShape <- function(Coords, Sex) {
  SShD <- SShDLM(Coords, Sex, Zeroed = T)
  PSD <- PSDShape(Coords, Sex)
  if (PSD == 0) {
    warning("Pooled standard deviation is zero.  Sample size too small?")
    return(NA_real_)
  }
  Effect <- SShD/PSD
  Effect
}
