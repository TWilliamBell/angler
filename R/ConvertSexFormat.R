#' Convert Sex Data to format used by our Functions
#'
#' @param Sex A character or factor vector (vector produced will be the same type) that includes the sex of each individual.
#' @param female.string How sex for females is recorded currently (defaults to "F").
#' @param male.string How sex for males is recorded currently (defaults to "M").
#'
#' @export
#'
#' @examples
#' data(pupfish) ## In geomorph.
#' ConvertSexFormat(pupfish$Sex)

ConvertSexFormat <- function(Sex, female.string = "F", male.string = "M") {

  Sex <- as.character(Sex)

  for (i in 1:length(Sex)) {
    if (Sex[i] == female.string) {
      Sex[i] <- "f"
    }
    else if (Sex[i] == male.string) {
      Sex[i] <- "m"
    }
    else {cat(paste("Cannot convert the ", i, "th term to the standardized format, \nconvert manually?"))}
  }

  Sex <- as.factor(Sex)
  Sex
}
