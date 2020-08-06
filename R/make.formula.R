# #' Utility function
# #'
# #' Utility function for data manipulation, which is implemented in \link{maglm} and \link{mamglm}.
# #'
# #' @export
# #' @param lhs Numeric vector of dependent variables.
# #' @param vars.vec Character vector of independet variables.
# #' @return an object of class '"formula"'
# #' @seealso \code{\link{maglm}}, \code{\link{mamglm}}

make.formula <- function(lhs, vars.vec){
  if (length(vars.vec) < 2)
  return(as.formula(paste(lhs, '~', vars.vec), env = parent.frame()))
  rhs <- vars.vec[1]
  for (v in vars.vec[2:length(vars.vec)])
    rhs <- paste(rhs, '+', v)
    as.formula(paste(lhs, '~', rhs), env = parent.frame())
}

# #' @export
# #' @rdname make.formula
make.formula2 <- function(vars.vec){
  if (length(vars.vec) < 2)
  return(as.formula(paste('~', vars.vec), env = parent.frame()))

  rhs <- vars.vec[1]
  for (v in vars.vec[2:length(vars.vec)])
    rhs <- paste(rhs, '+', v)
    as.formula(paste('~', rhs))
}
