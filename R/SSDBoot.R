#' @export

SSDBoot <- function(Sex, Indices, Size, SSDlog = F, ...) { ## Bootstrap statistic for SSD
  if (!isTRUE(log)) {
    SSD <- SSDLM(Size[Indices], Sex[Indices])
  }
  else {
    SSD <- SSDLog(Size[Indices], Sex[Indices], ...)
  SSD
}
