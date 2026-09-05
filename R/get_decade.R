#' Get the decade for a date
#'
#' @param x A date or date-time vector.
#' @return A numeric vector giving the first year of the decade for each value
#'   in `x`.
#' @examples
#' get_decade(as.Date(c("1974-06-01", "1989-01-01", "2026-09-05")))
#' @importFrom lubridate year
#' @export
get_decade <- function(x) {
  year(x) - year(x) %% 10
}
