#' Within-Group SShD and SSD Calculation with Standard Error (via Bootstrap)
#'
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SShD and SSD
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param print.progress A logical vector, if TRUE, updates you on the progress of your bootstraps
#' @param ... rep can be used to change the number of resamplings done for the Bootstraps, and Zeroed can be passed to SShDLM()
#'
#' @export
#'
#' @examples
#' StratSShDwSE(Strata, Coords, Sex, Size = NULL, print.progress = TRUE, rep = 1000, Zeroed = FALSE)

StratSShDwSE <- function(Strata, Coords, Sex, Size = NULL, print.progress = TRUE, ...) {
  Factors <- levels(Strata)
  Sex <- as.character(Sex)
  n <- length(Factors)
  if (is.null(Size)) {
    spots <- 4
    }
  else { spots <- 2 }
  SShDs <- rep(list(rep(NA_real_, spots)), n)
  for (i in 1:n) {
    level <- Factors[i]
    StratCoord <- Coords[Strata == level, , drop = F]
    StratSex <- Sex[Strata == level]
    if (!is.element("m", StratSex) | !is.element("f", StratSex)) {
      warning(paste("Sex data not available for at least one sex for ", i, "."
                    , sep = ""))
    }
    else {SShDs[[i]] <- SShDwSE(StratCoord, StratSex, Size, ...)}
    if (isTRUE(print.progress)) {
      PrintProgress(i, n)
    }
  }
  Results <- data.frame(Factors)
  Results$SShD.Mean <- nthelementoflists(SShDs, 1)
  Results$SShD.SE <- nthelementoflists(SShDs, 2)
  if (!is.null(Size)) {
    Results$SSD.Mean <- nthelementoflists(SShDs, 3)
    Results$SSD.SE <- nthelementoflists(SShDs, 4)
  }
  Results
}
