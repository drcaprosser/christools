#' Format a vector of numbers as a grammatical list
#'
#' @param x A numeric vector.
#' @param digits An optional number of decimal places to round to before
#'   formatting.
#' @return A single character string.
#' @examples
#' nice_list_of_numbers(c(1, 2, 3))
#' nice_list_of_numbers(c(1.234, 5.678), digits = 2)
#' @export
nice_list_of_numbers <- function(x, digits = NULL) {
  if (!is.null(digits)) {
    x <- round(x, digits)
  }

  x <- as.character(x)
  n <- length(x)

  if (n == 0) {
    return("")
  }

  if (n == 1) {
    return(x)
  }

  if (n == 2) {
    return(paste(x, collapse = " and "))
  }

  paste0(
    paste(x[-n], collapse = ", "),
    ", and ",
    x[n]
  )
}
