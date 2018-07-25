#' SShD and its Standard Error via a Bootstrap
#'
#' @param Coords  Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param rep  The number of times that you wish the resampling to be done, defaults to 1000 but more is better except for very small samples.
#'
#' @export

SShDwSE <- function(Coords, Sex, rep = 1000) {
  SShDBooted <- bootSShDLM(Coords = Coords, Sex = Sex, rep)
  SE <- sd(SShDBooted$t)
  M <- mean(SShDBooted$t)
  Results <- data.frame(SShD.Mean = M, SShD.SE = SE)
  Results
}