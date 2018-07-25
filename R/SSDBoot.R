#' @export

SSDBoot <- function(Sex, Indices, Size) { ## Bootstrap statistic for SSD
  SSD <- SSDLM(Size[Indices], Sex[Indices])
  SSD
}
