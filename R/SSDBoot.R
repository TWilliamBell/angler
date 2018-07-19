#' @export

SSDBoot <- function(Sex, Indices, Size) { ## Bootstrap statistic for SSD
  SSD <- SSDFunc(Size[Indices], Sex[Indices])
  SSD
}