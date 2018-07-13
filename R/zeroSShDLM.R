#' Calculates Shape Diversity regardless of Sex
#'
#' Determines the shape diversity in the population in a way that can be used to provide a benchmark for how much sexual shape dimorphism there is over and above the overall shape diversity.
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @export
#'
#' @examples
#' zeroSShDLM(Coords, Sex)

zeroSShDLM <- function(Coords, Sex) {
  Sex <- sample(Sex, length(Sex))
  NullSShD <- SShDLM(Coords, Sex, Zeroed = F)
  NullSShD
}
