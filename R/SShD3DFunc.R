#' Calculating Sexual Shape Dimorphism
#'
#' This function directly calculates SShD.
#' @param Coords Three-dimensional array of coordinates from geometric morphometric analysis (see gpagen() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Zeroed a logical value stating whether you wish to correct the SShD for the variation among individuals regardless of sex (defaults to TRUE)
#'
#' @export
#'
#' @examples
#' SShD3DFunc(Coords, Sex)
#' SShD3DFunc(Coords, Sex, Zeroed = TRUE)
#' SShD3DFunc(Coords, Sex, TRUE)

SShD3DFunc <- function(Coords, Sex, Zeroed = TRUE) {

  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }

  FData <- Coords[ , , as.character(Sex)=='f', drop = FALSE]
  MData <- Coords[ , , as.character(Sex)=='m', drop = FALSE]

  ConsensusF <- apply(FData, c(1,2), mean) ## Average male and female coordinates
  ConsensusM <- apply(MData, c(1,2), mean)

  DiffFM <- ConsensusF-ConsensusM

  SShD <- euclidean(DiffFM)

  if (isTRUE(Zeroed)) {
    Zero <- replicate(1000, zero3DSShD(Coords, Sex))
    return(SShD-mean(Zero))
  }

  SShD
}
