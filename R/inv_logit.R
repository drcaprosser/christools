#' Transform log-odds into probabilities
#'
#' @param x A number or vector.
#' @examples
#' inv_logit(c(-2, -1, 0, 1, 2))
#' @export
inv_logit <- function(x){
exp(x)/(1 + exp(x))
}
