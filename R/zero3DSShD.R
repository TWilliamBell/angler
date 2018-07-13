#' Calculates Shape Diversity regardless of Sex
#'
#' Determines the shape diversity in the population in a way that can be used to provide a benchmark for how much sexual shape dimorphism there is over and above the overall shape diversity.
#'
#' @param Coords Three-dimensional array of coordinates from geometric morphometric analysis (see gpagen() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @export
#'
#' @examples
#' zero3DSShD(Coords, SexVec)

zero3DSShD <- function(Coords, Sex) {
  Sex <- sample(Sex, length(Sex))
  NullSShD <- SShD3DFunc(Coords, Sex, Zeroed = FALSE)
  NullSShD
}
