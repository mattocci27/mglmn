#' Deprecated Functions
#'
#'
#' `maglm()` and `ses.maglm()` are deprecated as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.
#'
#' @export
#' @keywords internal
#' @param x, deprecated parameters
maglm <- function(x, ...){
  warning("`maglm()` is deprecated as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.")
}

#' @export
#' @keywords internal
#' @rdname maglm
#' @param x, deprecated parameters
ses.maglm <- function(x, ...){
  warning("`ses.maglm()` is deprecated as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.")
}
