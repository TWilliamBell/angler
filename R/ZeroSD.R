#' @export

ZeroSD <- function(Coords, SexVec) { 
  Zeroes <- replicate(1000, zeroSShD(Coords, SexVec))
  GeomorphDenom <- sd(unlist(Zeroes))
  GeomorphDenom
}