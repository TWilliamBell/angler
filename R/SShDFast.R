#' Calculating Sexual Shape Dimorphism Fast
#'
#' This function directly calculates SShD in the fastest manner (accuracy lost).
#' 
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @examples 
#' SShDFast(Coords, Sex)

SShDFast <- function(Coords, Sex) {
  
  FData <- Coords[as.character(Sex)=='f', , drop = FALSE]
  MData <- Coords[as.character(Sex)=='m', , drop = FALSE]
  
  ConsensusF <- apply(FData, 2, mean) ## Average male and female coordinates
  ConsensusM <- apply(MData, 2, mean)
  
  DiffFM <- ConsensusF-ConsensusM
  
  SShD <- euclidean(DiffFM)
  SShD
}