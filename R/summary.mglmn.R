#' Summarize Model Averaging for Multivariate Models
#'
#' The \code{summary.mamglm} provides a summary of the 
#'
#' @name summary.mglmn
#'
#' @export
#' @method summary mglmn
#' @param object mglmn object.
#' @param top_n Number of models to show.
#' @param digits Number of digits to use for rounding.
#' @param ... additional arguments affecting the summary produced.
#' @return A list of results
#' @return \item{res.table }{data frame with "AIC", AIC of the model, "log.L", log-likelihood of the model, "delta.aic", AIC difference to the best model, "wAIC", weighted AIC to the model, "n.vars", number of variables in the model, and each term.}

summary.mglmn <- function(object, top_n = 3, digits = 3, ...) {

  tb0 <- head(round(object$res.table, digits), top_n)
  rank_name <- attr(object, "rank")
  delta_rank <- paste0("delta.", rank_name)
  wrank <- paste0("w", rank_name)

  models <- numeric(0)
  model <- tb0[,-1:-5]
  for (i in 1:top_n) {
    tmp <- names(model[i, model[i,] == 1])
    if (length(tmp) > 1) {
      rhs <- tmp[1]
      for (v in tmp[2:length(tmp)]) {
        rhs <- paste(rhs, '+', v)
     }
    } else {
      tmp <- model[i,]
      tmp.num <- as.numeric(tmp)
      names(tmp.num) <- names(tmp)
      rhs <- names(tmp.num[tmp.num > 0])
    }
    models <- c(models, rhs)
  }

  tb <- cbind(models, tb0[, c(paste(rank_name), paste(delta_rank), paste(wrank))])

  smry <- list()
  smry$`Top Models` <- tb
  names(smry)[1] <- paste("Top", top_n, "Models")
  smry$`Relative importance`  <- round(object$importance, digits)
  structure(smry, class = "summary.mglmn",
    model_name = deparse(substitute(object)), 
    family = attr(object, "family"),
    print_digits = digits)
}
