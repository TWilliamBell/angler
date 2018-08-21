#' Stratified Standard Error calculator for SSD
#'
#' @param Size a numerical vector giving the size of each individual as a scalar quantity
#' @param Sex a character or factor vector recording sex for each individual as 'm' or 'f', individuals must be in the same order as the Size vector
#' @param Strata a character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SSD, individuals must be in the same order as the Size vector
#' @param rep number of times to perform the resampling procedure
#'
#' @export

StratSSDBoot <- function(Size, Sex, Strata, rep = 1000, log = F, ...) {
  ## Does stratified bootstraps for SSD
  Strata <- as.factor(Strata)
  Levels <- levels(Strata)
  n <- nlevels(Strata)
  SE <- rep(NA_real_, n)
  M <- rep(NA_real_, n)
  for (i in 1:n) {
    level <- Levels[i]
    SSDBooted <- bootSSD(Size = Size[Strata == level], Sex = Sex[Strata == level], rep, log = log, ...)
    SE[i] <- sd(SSDBooted$t)
    M[i] <- mean(SSDBooted$t)
  }
  Results <- data.frame(Factors = Levels, SSD.Mean = M, SSD.SE = SE)
  Results
}
