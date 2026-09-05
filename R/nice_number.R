#' Format numbers for concise reporting
#'
#' Rounds numbers with an absolute value of at least one to a specified
#' number of decimal places, and smaller numbers to a specified number of
#' significant digits. The leading zero before the decimal point can
#' optionally be removed.
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places to retain for values with an
#'   absolute value of at least one.
#' @param small_digits The number of significant digits to retain for values
#'   with an absolute value below one. Defaults to `digits`, except that at
#'   least one significant digit is always retained.
#' @param trim_leading_zero Logical; should the zero before the decimal point
#'   be removed from values between -1 and 1? Defaults to `FALSE`.
#' @return A character vector containing formatted values.
#' @examples
#' nice_number(c(12.345, 0.123, 0.000001122113123))
#' nice_number(
#'   c(12.345, 0.123, 0.000001122113123),
#'   digits = 2,
#'   small_digits = 3,
#'   trim_leading_zero = TRUE
#' )
#' @export
nice_number <- function(
    x,
    digits = 1,
    small_digits = NULL,
    trim_leading_zero = FALSE
) {
  check_integer <- function(value, name, minimum) {
    if (
      !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value != floor(value) ||
      value < minimum
    ) {
      stop(name, " must be a finite integer >= ", minimum, ".")
    }
  }

  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  check_integer(digits, "digits", 0)

  if (is.null(small_digits)) {
    small_digits <- max(1, digits)
  }

  check_integer(small_digits, "small_digits", 1)

  if (
    !is.logical(trim_leading_zero) ||
    length(trim_leading_zero) != 1L ||
    is.na(trim_leading_zero)
  ) {
    stop("trim_leading_zero must be TRUE or FALSE.")
  }

  vapply(x, function(z) {
    if (is.na(z)) {
      return(NA_character_)
    }

    out <- if (abs(z) >= 1) {
      round(z, digits)
    } else {
      signif(z, small_digits)
    }

    out <- format(
      out,
      trim = TRUE,
      scientific = FALSE
    )

    if (trim_leading_zero) {
      out <- sub(
        "^(-?)0\\.",
        "\\1.",
        out
      )
    }

    out
  }, character(1))
}
