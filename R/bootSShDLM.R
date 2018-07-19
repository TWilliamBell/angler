#' @export

bootSShDLM <- function(Coords, Sex, reps = 1000, ...) { ## Does bootstraps for SShD
  SShDBooted <- boot(Sex, SShDBoot, reps, Coords = Coords, ...)
  SShDBooted
}