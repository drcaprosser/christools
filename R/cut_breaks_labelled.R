#' Cut a vector using labelled intervals
#'
#' Creates an ordered factor from explicit break points, with labels showing
#' the interval boundaries.
#'
#' @param x A numeric vector to cut into intervals.
#' @param breaks A numeric vector of at least two strictly increasing break
#'   points.
#' @param digits The number of decimal places to use in interval labels.
#' @param sep A character string used between the lower and upper boundaries in
#'   interval labels.
#' @param last_plus Logical; should the final interval label use a plus sign
#'   instead of displaying its upper boundary?
#' @param open_ended Logical; should the final interval extend to infinity?
#' @return An ordered factor containing the labelled intervals.
#' @examples
#' cut_breaks_labelled(
#'   c(3, 17, 42),
#'   breaks = c(0, 10, 20, 50),
#'   last_plus = TRUE
#' )
#' @export
cut_breaks_labelled <- function(
    x,
    breaks,
    digits = 1,
    sep = "–",
    last_plus = FALSE,
    open_ended = FALSE
) {
  if (length(breaks) < 2 ||
      any(!is.finite(breaks)) ||
      any(diff(breaks) <= 0)) {
    stop("breaks must contain at least two strictly increasing numbers.")
  }

  format_break <- function(z) {
    format(
      round(z, digits),
      trim = TRUE,
      scientific = FALSE
    )
  }

  labels <- paste0(
    format_break(head(breaks, -1)),
    sep,
    format_break(tail(breaks, -1))
  )

  if (last_plus) {
    labels[length(labels)] <- paste0(
      format_break(breaks[length(breaks) - 1]),
      "+"
    )
  }

  cut_breaks <- breaks

  if (open_ended) {
    cut_breaks[length(cut_breaks)] <- Inf
  } else {
    cut_breaks[length(cut_breaks)] <-
      cut_breaks[length(cut_breaks)] +
      sqrt(.Machine$double.eps) *
      max(1, abs(cut_breaks[length(cut_breaks)]))
  }

  cut(
    x,
    breaks = cut_breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
  )
}
