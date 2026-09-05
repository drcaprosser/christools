#' Express numbers approximately in words
#'
#' Converts non-negative numbers into short approximate phrases such as
#' "about five", "just under one million", or "a quarter".
#'
#' @param x A numeric vector of non-negative values.
#' @param integer_max The largest value to round to an integer and express
#'   directly in words.
#' @param tolerance The relative difference within which a value is described
#'   as "about" the target.
#' @param nice_breaks Numeric values used to construct rounded targets across
#'   different orders of magnitude.
#' @param fraction_denoms Numeric denominators used when approximating values
#'   below one.
#' @param word_floor The smallest value at which scale names such as
#'   "million" and "billion" are used.
#' @return A character vector containing one approximate phrase for each value
#'   in x.
#' @examples
#' approx_text(c(0, 0.25, 3.2, 1250000))
#' @export
approx_text <- function(x,
                        integer_max = 20,
                        tolerance = 0.05,
                        nice_breaks = c(1, 2, 3, 5),
                        fraction_denoms = c(2, 3, 4, 5, 10, 20, 50, 100),
                        word_floor = 1e6) {
  number_words <- c(
    "zero", "one", "two", "three", "four", "five",
    "six", "seven", "eight", "nine", "ten"
  )

  word_num <- function(n) {
    if (n >= word_floor) {
      scales <- c(
        vigintillion = 1e63,
        novemdecillion = 1e60,
        octodecillion = 1e57,
        septendecillion = 1e54,
        sexdecillion = 1e51,
        quindecillion = 1e48,
        quattuordecillion = 1e45,
        tredecillion = 1e42,
        duodecillion = 1e39,
        undecillion = 1e36,
        decillion = 1e33,
        nonillion = 1e30,
        octillion = 1e27,
        septillion = 1e24,
        sextillion = 1e21,
        quintillion = 1e18,
        quadrillion = 1e15,
        trillion = 1e12,
        billion = 1e9,
        million = 1e6,
        thousand = 1e3
      )

      scale_name <- names(scales)[which(n >= scales)[1]]
      scale_value <- scales[[scale_name]]
      scaled <- n / scale_value

      scaled_text <- if (scaled <= 10 && scaled == round(scaled)) {
        number_words[scaled + 1]
      } else {
        format(round(scaled, 1), trim = TRUE, nsmall = 0)
      }

      paste(scaled_text, scale_name)

    } else if (n >= 0 && n <= 10 && n == round(n)) {
      number_words[n + 1]
    } else {
      format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
    }
  }

  fraction_phrase <- function(denom) {
    dplyr::case_when(
      denom == 2 ~ "a half",
      denom == 3 ~ "a third",
      denom == 4 ~ "a quarter",
      denom == 5 ~ "a fifth",
      TRUE ~ paste0("one in ", denom)
    )
  }

  nearest_nice <- function(z) {
    power_range <- floor(log10(z)) + -2:2
    candidates <- as.vector(outer(nice_breaks, 10^power_range, FUN = "*"))
    candidates[which.min(abs(z - candidates))]
  }

  purrr::map_chr(x, function(z) {
    if (is.na(z)) {
      return(NA_character_)
    }

    if (z < 0) {
      stop("approx_text() currently only handles non-negative values.")
    }

    if (z == 0) {
      return("zero")
    }

    if (z < 1) {
      vals <- 1 / fraction_denoms
      i <- which.min(abs(z - vals))
      target <- vals[i]
      phrase <- fraction_phrase(fraction_denoms[i])

    } else if (z <= integer_max) {
      target <- round(z)
      phrase <- word_num(target)

    } else {
      target <- nearest_nice(z)
      phrase <- word_num(target)
    }

    rel_diff <- (z - target) / target

    qualifier <- dplyr::case_when(
      abs(rel_diff) <= tolerance ~ "about",
      rel_diff < 0 ~ "just under",
      rel_diff > 0 ~ "just over"
    )

    paste(qualifier, phrase)
  })
}
