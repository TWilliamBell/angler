#' Sexual Size Dimorphism Calculator
#'
#' Calculates the SDI measure of sexual size dimorphism (Lovich and Gibbons 1992).  In order to calculate the log ratio instead use SSDLog.
#'
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Sex character or factor vector of individual's sexes given as 'f' for female and 'm' for male, individuals should be in the same order as the Size vector
#'
#' @export
#'
#' @examples
#' SSDFunc(Size, Sex)

SSDFunc <- function(Size, Sex) {
  Sex <- as.character(Sex)
  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }
  MSize <- mean(Size[Sex == 'm'])
  FSize <- mean(Size[Sex == 'f'])
  if (FSize >= MSize) {
    SSD <- (FSize/MSize)-1
  }
  else {
    SSD <- -((MSize/FSize)-1)
  }
  SSD
}
