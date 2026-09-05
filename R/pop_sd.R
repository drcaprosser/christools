#' Calculate a population standard deviation
#'
#' @param x A numeric vector.
#' @param na.rm Logical; should missing values be removed?
#' @return A numeric value giving the population standard deviation of x.
#' @examples
#' pop_sd(c(1, 2, 3, 4))
#' @export
pop_sd <- function(x, na.rm = FALSE) {
  sqrt(
    var(x, na.rm = na.rm) *
      (sum(!is.na(x)) - 1) /
      sum(!is.na(x))
  )
}
