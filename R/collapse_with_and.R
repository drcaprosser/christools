#' Collapse values into a grammatical list
#'
#' @param x A vector of values.
#' @param sep A character string used to separate the elements.
#' @param connector A character string to place between the final two
#'   elements, such as "and" or "or". Use NULL or "" for no connector.
#' @param serial_comma Logical; whether to include sep before connector
#'   in lists of three or more elements. Defaults to TRUE.
#' @return A single character string.
#' @examples
#' collapse_with_and(c(1, 2, 3))
#' collapse_with_and(c("apples", "bananas", "pears"), connector = "or")
#' collapse_with_and(c("apples", "bananas", "pears"), serial_comma = FALSE)
#' collapse_with_and(c("apples", "bananas"), connector = NULL)
#' @export
collapse_with_and <- function(x,
                              sep = ", ",
                              connector = "and",
                              serial_comma = TRUE) {
  x <- as.character(x)
  n <- length(x)

  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    stop("sep must be a single non-missing character string.",
         call. = FALSE)
  }

  if (length(connector) == 0L) {
    connector <- ""
  } else if (!is.character(connector) ||
             length(connector) != 1L ||
             is.na(connector)) {
    stop("connector must be NULL or a single non-missing character string.",
         call. = FALSE)
  }

  if (!is.logical(serial_comma) ||
      length(serial_comma) != 1L ||
      is.na(serial_comma)) {
    stop("serial_comma must be a single non-missing logical value.",
         call. = FALSE)
  }

  if (n == 0L) {
    return("")
  }

  if (n == 1L) {
    return(x)
  }

  if (!nzchar(connector)) {
    return(paste(x, collapse = sep))
  }

  if (n == 2L) {
    return(paste(x[1], connector, x[2]))
  }

  last_sep <- if (serial_comma) {
    paste0(sep, connector, " ")
  } else {
    paste0(" ", connector, " ")
  }

  paste0(
    paste(x[-n], collapse = sep),
    last_sep,
    x[n]
  )
}
