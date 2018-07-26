#' Calculate the Pooled Standard Deviation for Size for each Group
#'
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @export

StratPSDSize <- function(Strata, Size, Sex) {
  Levels <- levels(Strata)
  n <- nlevels(Strata)
  Results <- rep(NA_real_, n)
  for (i in 1:n) {
    level <- Levels[i]
    StratSize <- Size[Strata == level]
    StratSex <- Sex[Strata == level]
    Results[i] <- PooledSDSize(StratSize, StratSex)
  }
  Results
}