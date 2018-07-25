#' @export

SSDBoot <- function(Sex, Indices, Size, log = F, ...) { ## Bootstrap statistic for SSD
  if (isFALSE(log)) {
    SSD <- SSDLM(Size[Indices], Sex[Indices])
  }
  else {
    SSD <- SSDLog(Size[Indices], Sex[Indices], ...)
  SSD
}
