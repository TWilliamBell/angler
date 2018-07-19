#' @export

bootSSD <- function(Size, Sex, reps = 1000, ...) { ## Bootstrap for SSD
  SSDBooted <- boot(Sex, SSDBoot, reps, Size = Size)
  SSDBooted
}