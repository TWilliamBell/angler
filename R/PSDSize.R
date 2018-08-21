#' Pooled Standard Deviation for Size with respect to Sex
#' 
#' The standard deviation of size after removing the difference between the sexes and normalizing the individual size to have a mean of 1.
#'
#' @param Size A numerical vector giving the size of each individual as a scalar quantity
#' @param Sex A character or factor vector recording sex for each individual as 'm' or 'f', individuals should be in the same order as the Size vector
#'
#' @export

PSDSize <- function(Size, Sex) {
  Sex <- as.character(Sex)
  if (!is.element("m", Sex) | !is.element("f", Sex)) {
    return(NA_real_)
    warning("No data found for at least one sex.")
  }
  Size <- Size/mean(Size)
  DiffFM <- SSDFunc(Size, Sex)
  Size[Sex == "f"] <- Size[Sex == "f"] - DiffFM
  PooledSD <- sd(Size)
  PooledSD
}
