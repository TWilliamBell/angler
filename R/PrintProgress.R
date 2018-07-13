PrintProgress <- function(i, n) {
  if (i%%10 == 1 & i != 11) {
    cat("Completed the ", i, "st out of ", n, " levels.\n", sep = "")
  }
  else if (i%%10 == 2 & i != 12) {
    cat("Completed the ", i, "nd out of ", n, " levels.\n", sep = "")
  }
  else if (i%%10 == 3 & i != 13) {
    cat("Completed the ", i, "rd out of ", n, " levels.\n", sep = "")
  }
  else {
    cat("Completed the ", i, "th out of ", n, " levels.\n", sep = "")
  }
}