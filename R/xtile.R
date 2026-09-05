#' Divide values into quantile intervals
#'
#' A quantile-binning helper modelled on Stata's xtile(), which handles tied
#' values differently from dplyr::ntile().
#'
#' @param x A numeric vector.
#' @param n The number of quantile intervals to request.
#' @param labels Logical; should the intervals be returned as an ordered factor
#'   with labelled boundaries rather than as integer codes?
#' @param digits The number of decimal places to use in interval labels.
#' @param sep A character string used between the lower and upper boundaries in
#'   interval labels.
#' @return An integer vector of interval codes if labels = FALSE, or an
#'   ordered factor with labelled intervals if labels = TRUE.
#' @examples
#' xtile(1:100, n = 4, labels = TRUE)
#' @export
xtile <- function(
    x,
    n = 10,
    labels = FALSE,
    digits = 2,
    sep = "–"
) {
  probs <- seq(0, 1, length.out = n + 1)

  cuts <- quantile(
    x,
    probs = probs,
    na.rm = TRUE,
    type = 2,
    names = FALSE
  )

  cuts <- unique(cuts)

  if (length(cuts) < 2) {
    warning("There are not enough unique values to construct intervals.")
    return(rep(NA, length(x)))
  }

  tiles <- cut(
    x,
    breaks = cuts,
    include.lowest = TRUE,
    labels = FALSE
  )

  if (!labels) {
    return(as.integer(tiles))
  }

  format_cut <- function(z) {
    format(
      round(z, digits),
      trim = TRUE,
      scientific = FALSE
    )
  }

  tile_labels <- paste0(
    format_cut(head(cuts, -1)),
    sep,
    format_cut(tail(cuts, -1))
  )

  factor(
    tiles,
    levels = seq_along(tile_labels),
    labels = tile_labels,
    ordered = TRUE
  )
}
