#' Within-Group SShD calculation
#'
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Zeroed a logical value stating whether you wish to correct the SShD for the variation among individuals regardless of sex (defaults to TRUE)
#'
#' @examples
#' StratSShD(Strata, Coords, Sex, T)
#' StratSShD(Strata, Coords, Sex, Zeroed = FALSE)

StratSShD <- function(Strata, Coords, Sex, Zeroed = TRUE) {
  Factors <- levels(Strata)
  Sex <- as.character(Sex)
  SShDs <- rep(NA_real_, length(Factors))
  for (i in 1:length(Factors)) {
    level <- Factors[i]
    StratCoord <- Coords[Strata == level, , drop = F]
    StratSex <- Sex[Strata == level]
    if (!is.element("m", StratSex) | !is.element("f", StratSex)) {
      SShDs[i] <- NA_real_
      warning(paste("Sex data not available for at least one sex for ", i, "."
                    , sep = ""))
    }
    else {SShDs[i] <- SShDLM(StratCoord, StratSex, Zeroed)}
  }
  Results <- data.frame(Factors = Factors, SShD = SShDs)
  Results
}
