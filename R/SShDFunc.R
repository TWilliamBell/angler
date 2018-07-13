#' Calculating Sexual Shape Dimorphism
#'
#' This function directly calculates SShD.
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Zeroed a logical value stating whether you wish to correct the SShD for the variation among individuals regardless of sex (defaults to TRUE)
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
  
  ConsensusF <- apply(FData, 2, mean) ## Average male and female coordinates
  ConsensusM <- apply(MData, 2, mean)
  
  DiffFM <- ConsensusF-ConsensusM
  
  SShD <- euclidean(DiffFM)
  
  if (isTRUE(Zeroed)) {
    Zero <- replicate(1000, zeroSShD(Coords, Sex))
    return(SShD-mean(Zero))
  }
  
  SShD
}