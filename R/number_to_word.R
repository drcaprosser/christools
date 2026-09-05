#' Convert numbers from zero to ten into words
#'
#' @param x A numeric vector.
#' @return A character vector, with whole numbers from zero to ten converted
#'   to words and all other values converted to character strings.
#' @examples
#' number_to_word(c(0, 5, 10, 11))
#' @export
number_to_word <- function(x) {
  words <- c(
    "zero",
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten"
  )

  dplyr::if_else(
    x == floor(x) & x >= 0 & x <= 10,
    words[x + 1],
    as.character(x)
  )
}
