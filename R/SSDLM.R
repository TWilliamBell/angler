#' Sexual Size Dimorphism Calculator
#'
#' Calculates the SDI measure of sexual size dimorphism (Lovich and Gibbons 1992).  In order to calculate the log ratio instead use SSDLog.  Compared to SSDFunc, this is less susceptible to unbalanced designs.
#'
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Sex character or factor vector of individual's sexes given as 'f' for female and 'm' for male
#'
#' @export
#'
#' @examples
#' SSDFunc(Size, Sex)

SSDLM <- function(Size, Sex) {
  Data <- data.frame(Size, Sex)
  DiffFM <- lm(Size ~ Sex, Data)$coef[2]
  FSize <- mean(Size[Sex == 'f'])
  MSize <- mean(Size[Sex == 'm'])
  if (FSize >= MSize & DiffFM >= 0) {
    MSize <- FSize - DiffFM
    SSD <- FSize/MSize - 1
  }
  else if (FSize >= MSize & DiffFM < 0) {
    MSize <- FSize + DiffFM
    SSD <- FSize/MSize - 1
  }
  else if (MSize >= FSize & DiffFM >= 0) {
    MSize <- FSize + DiffFM
    SSD <- -(MSize/FSize - 1)
  }
  else if (MSize >= FSize & DiffFM < 0) {
    MSize <- FSize - DiffFM
    SSD <- -(MSize/FSize - 1)
  }
  else {warning("Something went wrong when calculating SSD.  Is data formatted correctly (see ConvertSexFormat())? Does the data have both 'm' and 'f' individuals?")}
  unname(SSD)
}
