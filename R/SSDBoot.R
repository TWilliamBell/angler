SSDBoot <- function(Sex, Indices, Size, log = F, ...) { ## Bootstrap statistic for SSD
  if (isFALSE(log)) {
    SSD <- SSDLM(Size[Indices], Sex[Indices])
  }
  else if (isTRUE(log)) {
    SSD <- SSDLog(Size[Indices], Sex[Indices], ...)
  }
  else {stop("log argument must be a logical value.")}
  SSD
}
