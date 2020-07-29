#' Utility function
#'
#' Utility function for data manipulation, which is implemented in \link{maglm} and \link{mamglm}.
#'
#' @export
#' @param lhs Numeric vector of dependent variables.
#' @param vars.vec Character vector of independet variables.
#' @param rand.vec Character vector of random variables (default = NULL).
#' @return an object of class '"formula"'
#' @seealso \code{\link{maglm}}, \code{\link{mamglm}}

make.formula <- function(lhs, vars.vec, rand.vec = NULL){
if (is.null(rand.vec)) {
  if (length(vars.vec) < 2)
  return(noquote(paste(lhs, '~', vars.vec)))

  rhs <- vars.vec[1]
  for (v in vars.vec[2:length(vars.vec)])
    rhs <- paste(rhs, '+', v)
    noquote(paste(lhs, '~', rhs))
} else {
  if (length(vars.vec) < 2){
    rhs <- vars.vec[1]
    for (r in rand.vec[1:length(rand.vec)])
      rhs <- paste(rhs, ' + (1|', r, ')' , sep = "")
    return(noquote(paste(lhs, '~',vars.vec, '~', rhs)))
  }

  rhs <- vars.vec[1]
  for (v in vars.vec[2:length(vars.vec)])
    rhs <- paste(rhs, '+', v)
  for (r in rand.vec[1:length(rand.vec)])
    rhs <- paste(rhs, ' + (1|', r, ')', sep = "")
    noquote(paste(lhs, '~', rhs))
  }
}
