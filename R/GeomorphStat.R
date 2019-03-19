#' Geomorph's Statistic for Sexual Shape Dimorphism
#'
#' @param Coords Two-dimensional array of coordinates from geometric morphometric analysis (see two.d.array() in geomorph for correct formatting)
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the rows of the Coords
#' 
#' @export

GeomorphStat <- function(Coords, Sex) {
  Z <- SShDLM(Coords, Sex, Zeroed = T)/ZeroSD(Coords, Sex)
  Z
}
