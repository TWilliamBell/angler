#' @export

bootSSD <- function(Size, Sex, reps = 1000, log = NULL, ...) { ## Bootstrap for SSD
  SSDBooted <- boot(Sex, SSDBoot, reps, Size = Size, log = log, ...)
  SSDBooted
}
