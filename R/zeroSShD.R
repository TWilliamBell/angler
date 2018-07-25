#' Calculates Shape Diversity regardless of Sex
#'
#' SShD is strictly non-negative, and even in the absence of sexual dimorphism, SShD will likely be positive.  We can try to adjust it for SShD by calculating what SShD 'would' be in the absence of sexual dimorphism (by permuting the sexual of the individuals) and then subtracting that from SShD.  Most of angler's SShD functions do this by default ('Zeroed' arguments).
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f'
#'
#' @export
#'
#' @examples
#' zeroSShD(Coords, Sex)

zeroSShD <- function(Coords, Sex) {
  Sex <- sample(Sex, length(Sex))
  NullSShD <- SShDFast(Coords, Sex)
  NullSShD
}
