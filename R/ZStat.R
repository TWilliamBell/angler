#' Geomorph's Z-statistic for Sexual Shape Dimorphism
#'
#' @param Coords 
#' @param Sex 
#'
#' @export

ZStat <- function(Coords, Sex) {
  Z <- SShDLM(Coords, Sex, Zeroed = T)/ZeroSD(Coords, Sex)
  Z
}