#' Calculate SShD using the lm() function
#' 
#' All of this program's built in SShD functions that perform bootstrap analysis on the data use the lm() function for some of its nice properties.  This function allows you to reproduce the results of the lm() function-based SShD calculation for a single sample.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#' @param Zeroed a logical value stating whether you wish to correct the SShD for the variation among individuals regardless of sex (defaults to TRUE)

SShDLM <- function(Coords, Sex, Zeroed = T) {
  
  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }
  
  Data <- data.frame(Sex)
  Data$Coords <- Coords
  LM <- lm(Coords ~ Sex + 1, data = Data)
  DiffFM <- LM$coef[2, ]
  SShD <- euclidean(DiffFM)
  if (isTRUE(Zeroed)) {
    zero <- zeroSShDLM(Coords, Sex)
    return(SShD-mean(zero))
  }
  SShD
}