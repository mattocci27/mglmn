#' Removed Functions
#'
#'
#' `maglm()` and `ses.maglm()` were removed as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.
#'
#' @export
#' @keywords internal
#' @param x, removed parameters
maglm <- function(x, ...){
  warning("`maglm()` was removed as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.")
}

#' @export
#' @keywords internal
#' @rdname maglm
#' @param x, removed parameters
ses.maglm <- function(x, ...){
  warning("`ses.maglm()` was removed as of mglmn 0.2.0. Please use `MuMIn::model.ave()` for univariate analyses instead.")
}
