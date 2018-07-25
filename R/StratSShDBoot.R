#' Standard Error of SShD
#'
#' @param Coords  Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param NonSexFactor 
#' @param rep  The number of times that you wish the resampling to be done, defaults to 1000 but more is better except for very small samples.
#'
#' @export

StratSShDBoot <- function(Coords, Sex, NonSexFactor, rep = 1000, print.progress = T) { 
  ## Does stratified bootstraps for SShD
  NonSexFactor <- as.factor(NonSexFactor)
  Levels <- levels(NonSexFactor)
  n <- nlevels(NonSexFactor)
  SE <- rep(NA_real_, n)
  M <- rep(NA_real_, n)
  for (i in 1:n) {
    level <- Levels[i]
    SShDBooted <- bootSShDLM(Coords = Coords[NonSexFactor == level, , drop = F], Sex = Sex[NonSexFactor == level], rep)
    SE[i] <- sd(SShDBooted$t)
    M[i] <- mean(SShDBooted$t)
    if (isTRUE(print.progress)) {
      PrintProgress(i, n)
    }
  }
  Results <- data.frame(Factors = Levels, SShD.Mean = M, SShD.SE = SE)
  Results
}
