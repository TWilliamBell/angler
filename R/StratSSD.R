#' Within-Group SSD Calculation
#'
#' @param Strata A character or factor vector giving the factor by which we wish to divide individuals into groups before calculating SSD
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Log Defaults to FALSE, in which case you use the Lovich and Gibbons SDI to calculate SSD, if changed to TRUE you use the log ratio.
#' @param ... If Log is TRUE, then you may pass arguments to the SSDLog function this way.
#'
#' @export
#'
#' @examples
#' StratSSD(Strata, Size, Sex, Log = TRUE)
#' StratSSD(Strata, Size, Sex, Log = TRUE, positiveF = TRUE, base = 2)

StratSSD <- function(Strata, Size, Sex, Log = FALSE, ...) {
  Levels <- levels(Strata)
  SSDs <- rep(NA_real_, length(Levels))
  for (i in 1:length(Levels)) {
    level <- Levels[i]
    if (Log == FALSE) {
      SSDs[i] <- SSDLM(Size[Strata == level], Sex[Strata == level])
    }
    else if (Log == TRUE) {
      SSDs[i] <- SSDLog(Size[Strata == level], Sex[Strata == level], ...)
    }
    else {stop("Invalid Log argument, must be T/F.")}
  }
  StratSSD <- data.frame(Factors = Levels, SSD = SSDs)
  StratSSD
}
