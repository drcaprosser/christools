#' Mix two hexadecimal colours
#'
#' @param col1 A hexadecimal colour string.
#' @param col2 A hexadecimal colour string.
#' @param alpha Numeric; the mixing proportion. `0` returns `col1`, and `1` returns `col2`.
#' @return A hexadecimal colour string.
#' @examples
#' mix_hex("#E69F00", "#56B4E9")
#' mix_hex("#E69F00", "#56B4E9", alpha = 0.25)
#' @export
mix_hex <- function(col1, col2, alpha = 0.5) {
  colorspace::hex(
    colorspace::mixcolor(
      alpha = alpha,
      color1 = colorspace::hex2RGB(col1),
      color2 = colorspace::hex2RGB(col2)
    )
  )
}
