#' Calculating Sexual Shape Dimorphism
#'
#' This function directly calculates SShD.  However it may be a poor estimator for unbalanced designs where there are more of one sex than the other, SShDLM() is more robust.
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Zeroed a logical value stating whether you wish to correct the SShD for the variation among individuals regardless of sex (defaults to TRUE)
#'
#' @export
#'
#' @examples
#' SShDFunc(Coords, Sex, Zeroed = TRUE)
#' SShDFunc(Coords, Sex, TRUE)

SShDFunc <- function(Coords, Sex, Zeroed = TRUE) {

  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }

  FData <- Coords[as.character(Sex)=='f', , drop = FALSE]
  MData <- Coords[as.character(Sex)=='m', , drop = FALSE]

  ConsensusF <- colMeans(FData) ## Average male and female coordinates
  ConsensusM <- colMeans(MData)

  DiffFM <- ConsensusF-ConsensusM

  SShD <- euclidean(DiffFM)

  if (isTRUE(Zeroed)) {
    Zero <- replicate(1000, zeroSShD(Coords, Sex))
    return(SShD-mean(Zero))
  }

  SShD
}
