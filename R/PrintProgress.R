#' @export

PrintProgress <- function(i, n) {
  if (i == 1) {
    cat("|", paste0(rep("=", 60 %/% n), col = ""), sep = "")
  }
  else if (i==n) {
    cat(paste0(rep("=", 60 %/% n), "|", col = ""), sep = "")
  }
  else {
    cat(rep("=", 60 %/% n), sep = "")
  }
}
