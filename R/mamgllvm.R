#' Model averaging for multivariate generalized linear latent variable models
#'
#' Model averaging for multivariate GLLVM based on information theory.
#'
#' @name mamgllvm
#'
#' @export
#' @param data Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
#' @param y Name of 'site x spcies matrix' (col for species, row for sites) (character)
#' @param family the 'family' object used.
#' @param scale Whether to scale independent variables (default = TRUE)
#' @param rank optional
#' @return A list of results
#' @return \item{res.table }{data frame with "AIC", AIC of the model, "log.L", log-likelihood of the model, "delta.aic", AIC difference to the best model, "wAIC", weighted AIC to the model, "n.vars", number of variables in the model, and each term.}
#' @return \item{importance }{vector of relative importance value of each term, caluclated as as um of the weighted AIC over all of the model in whith the term aperars.}
#' @return \item{family }{the 'family' object used.}

#' @references 
#' Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.
#'
#' Niku, J., Warton,  D. I., Hui, F. K. C., and Taskinen, S. (2017). Generalized linear latent variable models for multivariate count and biomass data in ecology. Journal of Agricultural, Biological, and Environmental Statistics, 22:498-522.
#'
#' Niku, J., Brooks, W., Herliansyah, R., Hui, F. K. C., Taskinen, S., and Warton,  D. I. (2018). Efficient estimation of generalized linear latent variable models. PLoS One, 14(5):1-20.
#'
#' Warton, D. I., Guillaume Blanchet, F., O'Hara, R. B., Ovaskainen, O., Taskinen, S., Walker, S. C. and Hui, F. K. C. (2015). So many variables: Joint modeling in community ecology. Trends in Ecology & Evolution, 30:766-779.
#'
#' @seealso\code{\link{maglm}}, \code{\link{ses.mamglm}}, \code{\link{ses.mamgllvm}}
#' @examples
#' #load species composition and environmental data
#' library(mvabund)
#' data(capcay)
#' #use a subset of data in this example to reduce run time
#' env_assem <- capcay$env_assem[, 1:2]
#' freq.abs <- mvabund(log(capcay$abund + 1))
#'
#' #to fit a gaussian regression model to frequency data:
#' mamgllvm(data = env_assem, y = "freq.abs", family = "gaussian")
#'
#' #to fit a binomial regression model to presence/absence data"
#' pre.abs0 <- capcay$abund
#' pre.abs0[pre.abs0 > 0] = 1
#' pre.abs <- mvabund(pre.abs0)
#'
#' mamgllvm(data = env_assem, y = "pre.abs", family = "binomial")
mamgllvm <- function(data, y, family, scale = TRUE, rank = NULL){

  if (!is.null(rank) && rank != "AIC" && rank != "AICc" && rank != "BIC" && 
      rank != "aic" && rank != "aicc" && rank != "bic") {
    stop("Please use 'AIC', 'AICc' or 'BIC' for rank estimates")
  }

  if (scale) data <- as.data.frame(scale(data))
    my.vars <- colnames(data)
    n.vars <- length(my.vars)
    vars.list <- list()
    for (i in 1:n.vars){
    vars.list[[i]] <- combn(my.vars, i)
  }

#numbers of combination for each sample size
  temp <- sapply(vars.list, ncol)

#AIC for each model
  model.aic <- NULL
  log.L <- NULL
  vars <- list()
  k <- 0
  vars2 <- matrix(numeric(n.vars * sum(temp)), nrow = sum(temp), ncol = n.vars)

#number of paremeter = i+1 (one means intercept)
#sample size = nrow(data)
#AICc = -2*logLike + 2*n.para*n.sample/(n.sample-n.para-1)
  for (i in 1:n.vars){
    for (j in 1:temp[i]){
      k <- k + 1
      vars.temp <- vars.list[[i]][, j]
      vars[[k]] <- vars.temp
      #f.str <- make.formula(get(y), vars.temp)
     # f.str <- make.formula(y, vars.temp)
    # data2 <- data[, vars.temp]
      data2 <- as.data.frame(data[, vars.temp])
      colnames(data2) <- vars.temp

    #  message("y")
    #  get(y) %>% print
    #  message("data2")
    #  data2 %>% print
    #  message("family")
    #  print(family)

      fit_fun <- function(){
        gllvm(
          y = get(y),
          X = data2, 
          family = family,
          starting.val = "res")
      }

      fit.temp <- NULL
      fit.temp <- try(fit_fun(), silent = FALSE)

      if (class(fit.temp) == "try-error") {
        message("Use starting.val = 'zero' instead of 'res'")
        fit.temp <- gllvm(
          y = get(y),
          X = data2, 
          family = family,
          starting.val = "zero")
      }

      fit.summary <- summary(fit.temp)

      log.L.temp <- fit.summary$`log-likelihood`
      log.L <- c(log.L, log.L.temp)

      if (is.null(rank) || rank == "AICc" || rank == "aicc") {
        ranks <- fit.summary$AICc
      } else if (rank == "AIC" || rank == "aic") {
        ranks <- fit.summary$AIC
      } else if (rank == "BIC" || rank == "bic") {
        ranks <- fit.summary$BIC
      }
        model.aic <- c(model.aic, ranks)
    }
  }

  min.aic <- min(model.aic)
  delta.aic <- model.aic - min.aic
  wAIC <- exp(-delta.aic / 2) / sum(exp(-delta.aic / 2))

  res <- data.frame(AIC = model.aic, log.L = log.L, delta.aic, wAIC, n.vars = rep(1:n.vars, temp))

##counting vars
#vars2 is matrix filled with 0 (row:sites,col:parameters)
  vars2 <- matrix(numeric(n.vars * sum(temp)), nrow = sum(temp), ncol = n.vars)
  colnames(vars2) <- my.vars
  n.size <- rep(1:n.vars, temp) # number of paremters for each row
  for (i in 1:nrow(vars2)){ # each row (sample)
    for (j in 1:n.vars){ # each column (paramter)
      for (k in 1:n.size[i]){ # upto number of paramters
        if (colnames(vars2)[j] == vars[[i]][k]) vars2[i,j] <- 1
      }
    }
  }

# for exmaple, i=100
# > vars[[100]]
# [1] "LOG..x.1..Exotic.plant.rich"
# [2] "P.mega.Is.p.a"
# [3] "Tot.rain.during.sampling.log..x.0.01."
# these three paramters were used in the 100th analysis
#
# if jth colnames of vars2, which is a parameter name, is identical to vars[[i]][k], which is a paramter name used in the analysis, vars2[i,j] is replaced by 1

  res <- cbind(res, vars2)
  res <- res[order(res$AIC), ]

  rownames(res) <- NULL

#calculating weighted result of explanable variables
  res.temp <- res[, -1:-5]
  res2 <- apply(apply(res.temp, 2,
                      function(x)res$wAIC * x), 2, sum)
  out <- list(res.table = res, importance = res2, family = family)
  structure(out, class = "mglmn")
}
