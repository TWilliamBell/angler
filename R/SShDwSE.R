#' @export

SShDwSE <- function(Coords, Sex, Size = NULL, rep = 1000, ...) {
  Boot <- replicate(rep, bootSShD(Coords, Sex, Size, ...))
  if (!is.null(Size)) {
    SShD <- mean(unlist(Boot[1, ]))
    SShD.SE <- sd(unlist(Boot[1, ]))
    SSD <- mean(unlist(Boot[2, ]))
    SSD.SE <- sd(unlist(Boot[2, ]))
  }
  else {
    SShD <- mean(Boot)
    SShD.SE <- sd(Boot)
  }
  SShD.Results <- data.frame(SShD.Mean = SShD, SShD.SE = SShD.SE)
  if (!is.null(Size)) {
    SShD.Results$SSD.Mean <- SSD
    SShD.Results$SSD.SE <- SSD.SE
  }
  SShD.Results
}
