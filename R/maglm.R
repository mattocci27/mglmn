#' Model averaging for generalized linear models
#'
#' Model averaging for GLM based on information theory.
#'
#' @name maglm
#'
#' @export
#'
#' @param data Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
#' @param y Vector of independent variables.
#' @param family the 'family' object used.
#' @param scale Whether to scale independent variables (default = TRUE)
#' @param AIC.restricted Whether to use AICc (TRUE) or AIC (FALSE) (default = TRUE).
#'
#' @return A list of results
#' @return \item{res.table}{data frame with "AIC", AIC of the model, "log.L", log-likelihood of the model, "delta.aic", AIC difference to the best model, "wAIC", weighted AIC to the model, "n.vars", number of variables in the model, and each term.}
#' @return \item{importance}{vector of relative importance value of each term, caluclated as as um of the weighted AIC over all of the model in whith the term aperars.}
#' @return \item{family}{the 'family' object used.}
#' @return \item{scale}{Whether to scale independent variables (default = TRUE}
#' @return \item{AIC.restricted}{Whether to use AICc (TRUE) or AIC (FALSE) (default = TRUE).}
#'
#' @references Dobson, A. J. (1990) An Introduction to Generalized Linear Models. London: Chapman and Hall.
#' @references Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.
#' @references Nakamura, A., C. J. Burwell, C. L. Lambkin, M. Katabuchi, A. McDougall, R. J. Raven, and V. J. Neldner. (2015) The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach. Journal of Biogeography 42:1406-1417.
#'
#' @seealso \code{\link{mamglm}}, \code{\link{ses.maglm}}, \code{\link{ses.mamglm}}
#'
#' @examples
#' #load species composition and environmental data
#' data(capcay)
#' adj.sr <- capcay$adj.sr
#' env_sp <- capcay$env_sp
#'
#' #to fit a regression model:
#' maglm(data = env_sp, y = "adj.sr", family = "gaussian", AIC.restricted = TRUE)

maglm <- function(data, y, family, scale = TRUE, AIC.restricted = FALSE){
  if (scale) data <- as.data.frame(scale(data))
  my.vars <- colnames(data)
  n.vars <- length(my.vars)
  vars.list <- list()
  for (i in 1:n.vars){
    vars.list[[i]] <-combn(my.vars, i)
  }

#numbers of combination for each sample size
  temp<-sapply(vars.list, ncol)

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
      f.str <- make.formula(y, vars.temp)
      # if (family=="gaussian") fit.temp <- manylm(f.str,data=data)
      # else 
      fit.temp <- glm(f.str, data = data, family = family)

      log.L.temp <- logLik(fit.temp)
      log.L <- c(log.L, sum(log.L.temp))

      if (AIC.restricted){
        #aic is corrected for finite sampe size
        aic.restricted <- sum(-2 * log.L.temp + 2 * nrow(data) * (i + 1)
                              / (nrow(data) - (i + 1) - 1))
        # aic.restricted <- sum(-2*log.L.temp +2*nrow(data)*(i+1)/(nrow(data)-(i+1)-1))
        model.aic <- c(model.aic, aic.restricted)
      } else {
        # aic.unrestricted <- sum(-2*log.L.temp +2*(i+1))
        aic.unrestricted <- sum(-2 * log.L.temp + 2 * (i + 1))
        model.aic <- c(model.aic, aic.unrestricted)
      }
    }
  }

  min.aic <- min(model.aic)
  delta.aic <- model.aic - min.aic
  wAIC <- exp(-delta.aic / 2) / sum(exp(-delta.aic / 2))
  res <- data.frame(AIC=model.aic, log.L=log.L, delta.aic,
                    wAIC, n.vars = rep(1:n.vars, temp))

##counting vars
#vars2 is matrix filled with 0 (row:sites,col:parameters)
  vars2 <- matrix(numeric(n.vars * sum(temp)), nrow = sum(temp), ncol = n.vars)
  colnames(vars2) <- my.vars
  n.size <- rep(1:n.vars, temp) # number of paremters for each row
  for (i in 1:nrow(vars2)) { # each row (sample)
    for (j in 1:n.vars) { # each column (paramter)
      for (k in 1:n.size[i]) { # upto number of paramters
        if (colnames(vars2)[j] == vars[[i]][k]) vars2[i, j] <- 1
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
  res.temp <- res[,-1:-5]
  res2 <- apply(apply(res.temp, 2, function(x)res$wAIC * x), 2, sum)
  list(res.table = res, importance = res2, family = family)
}
