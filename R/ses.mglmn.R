#' Standardized effect size of relative importance values for mamglm
#'
#' Standardized effect size of relative importance values for model averaging GLM.
#'
#' The currently implemented null model shuffles the set of environmental variables across sites, while maintains species composition. Note that the function would take considerable time to execute.
#'
#' @name ses.mglmn
#'
#' @export
#' @param object Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
#' @param top_n Name of 'site x spcies matrix' (col for species, row for sites) (character)
#' @param par Wheter to use parallel computing (default = FALSE)
#' @param runs Number of randomizations.
#' @return A data frame of resluts for each term
#' @return \item{res.obs }{Observed importance of terms}
#' @return \item{res.rand.mean }{Mean importance of terms in null communites}
#' @return \item{res.rand.sd }{Standard deviation of importance of terms in null communites}
#' @return \item{SES }{Standardized effect size of importance of terms (= (res.obs - res.rand.mean) / res.rand.sd)}
#' @return \item{res.obs.rank }{Rank of observed importance of terms vs. null communites}
#' @return \item{runs }{Number of randomizations}
#' @references Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.
#' @references Wang, Y., Naumann, U., Wright, S.T. & Warton, D.I. (2012) mvabund- an R package for model-based analysis of multivariate abundance data. Methods in Ecology and Evolution, 3, 471-474.
#' @references Warton, D.I., Wright, S.T. & Wang, Y. (2012) Distance-based multivariate analyses confound location and dispersion effects. Methods in Ecology and Evolution, 3, 89-101.
#' @references Nakamura, A., C. J. Burwell, C. L. Lambkin, M. Katabuchi, A. McDougall, R. J. Raven, and V. J. Neldner. (2015) The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach. Journal of Biogeography 42:1406-1417.
#' @examples
#' library(mvabund)
#' #load species composition and environmental data
#' data(capcay)
#' #use a subset of data in this example to reduce run time
#' env_assem <- capcay$env_assem[, 1:5]
#' pre.abs0 <- capcay$abund
#' pre.abs0[pre.abs0 > 0] = 1
#' pre.abs <- as.matrix(pre.abs0)
#'
#' fit <- mamglm(data = env_assem, y = "pre.abs", family = "binomial")
#' #to execute calculations on a single core:
#' ses.mglmn(fit, runs=4)
#'
#' \dontrun{
#' #to execute parallel calculations:
#' sfInit(parallel = TRUE, cpus = 4)
#' sfExportAll()
#' ses.mamglm(data = env_assem, y = "pre.abs",
#'            par = TRUE, family = "binomial", runs=4)
#' }

ses.mglmn <- function(object, top_n = 5, par = FALSE, runs = 99){


  # runs<-2#
  null.env.list <- list()
  # data <- env2
  # before<-proc.time()
  for (i in 1:runs){
    data.temp <- object$data
    data.temp$temp <- rnorm(nrow(data.temp))
    data.temp <- data.temp[order(data.temp$temp), ]
    null.env.list[[i]] <- data.temp[, -ncol(data.temp)]
  }
  
  # y <- "pre.abs"
  # family <-"binomial"
  # AIC.restricted=F

  if (attr(object, "model") == "mamglm") {
    res.obs <- mamglm_select(object, top_n = top_n)$importance
    if (par == FALSE) {
      res.rand0 <- sapply(
                      null.env.list,
                      function(x){mamglm_select(
                                    object = object, 
                                    data = x,
                                    top_n = top_n
                                    )$importance})
    } else {
      res.rand0 <- sfSapply(
                      null.env.list,
                      function(x){mamglm_select(
                                    object = object, 
                                    data = x,
                                    top_n = top_n
                                    )$importance})
    }
  } else if (attr(object, "model") == "mamgllvm") {
    res.obs <- mamgllvm_select(object, top_n = top_n)$importance
    if (par == FALSE) {
      res.rand0 <- sapply(
                      null.env.list,
                      function(x){mamgllvm_select(
                                    object = object, 
                                    data = x,
                                    top_n = top_n
                                    )$importance})
    } else {
      res.rand0 <- sfSapply(
                      null.env.list,
                      function(x){mamgllvm_select(
                                    object = object, 
                                    data = x,
                                    top_n = top_n
                                    )$importance})
    }
  }


  res.rand <- t(res.rand0) #tranpose (row <-> column)
  res.rand.mean <- apply(res.rand, 2, mean, na.rm = T)
  res.rand.sd <- apply(res.rand, 2, sd, na.rm = T)
  SES <- (res.obs - res.rand.mean) / res.rand.sd
  res.obs.rank <- apply(rbind(res.obs, res.rand), 2, rank)[1, ]
  data.frame(res.obs, res.rand.mean, res.rand.sd, SES, res.obs.rank, runs)
}


mamglm_select <- function(object, 
                          data = NULL,
                          top_n = top_n) {
  
  models <- summary(object, top_n = top_n)[[1]]$models
  family <- object$family
  rank <- attr(object, "rank")
  y <- object$y
   
  if (is.null(data)) data  <- object$data 

  n_par <- sapply(strsplit(models, "\\+"), length)

  model.aic <- NULL
  log.L <- NULL
  n_samp <- nrow(data)
  n_sp <- ncol(data)
  for (i in 1:top_n) {
    f.str <- noquote(paste0(y, "~", models[i]))
    if (family == "gaussian") fit.temp <- manylm(f.str, data = data)
        else fit.temp <- manyglm(f.str, data = data, family = family)

    log.L.temp <- logLik(fit.temp)
    log.L <- c(log.L, sum(log.L.temp))
     
    # put penalty terms inside the sum
    # because we apply glm manytimes, penalty should be applied manytimes too
    if (is.null(rank) || rank == "AICc" || rank == "aicc") {
      ranks <- sum(-2 * log.L.temp + 2  * n_par[i] * n_samp / (n_samp - n_par[i] - 1))
      rank_name <- "AICc"
    } else if (rank == "AIC" || rank == "aic") {
      ranks <- sum(-2 * log.L.temp + 2 * n_par[i])
      rank_name <- "AIC"
    } else if (rank == "BIC" || rank == "bic") {
      ranks <- sum(-2 * log.L.temp + n_par[i] * log(n_samp))
      rank_name <- "BIC"
    }
      model.aic <- c(model.aic, ranks)
  }

  min.aic <- min(model.aic)
  delta.aic <- model.aic - min.aic
  wAIC <- exp(-delta.aic / 2) / sum(exp(-delta.aic / 2))
  res <- data.frame(AIC = model.aic, log.L = log.L, delta.aic, wAIC, n.vars = n_par)
  colnames(res)[1] <- rank_name
  colnames(res)[3] <- paste0("delta.", rank_name)
  colnames(res)[4] <- waic <- paste0("w", rank_name)

  

  vars <- sapply(models, function(x)strsplit(x, " \\+ "))

##counting vars
#vars2 is matrix filled with 0 (row:sites,col:parameters)
  vars2 <- matrix(numeric(ncol(data) * top_n), nrow = top_n, ncol = ncol(data))
  colnames(vars2) <- colnames(data)
  n.vars <- ncol(data)
  n.size <- sapply(vars, length)

  for (i in 1:nrow(vars2)){ # each row (model number)
    for (j in 1:n.vars){ # each column (paramter)
      for (k in 1:n.size[i]){ # upto number of paramters
        if (colnames(vars2)[j] == vars[[i]][k]) vars2[i,j] <- 1
      }
    }
  }

  res <- cbind(res, vars2)
  res <- res[order(res[,paste(rank_name)]), ]
  rownames(res) <- NULL
#calculating weighted result of explanable variables
  res.temp <- res[, -1:-5]
  res2 <- apply(apply(res.temp, 2,
                      function(x)res[, paste(waic)] * x), 2, sum)
  out <- list(res.table = res, importance = res2, family = family, y = y, data = data)

  structure(out, 
            class = "mglmn",
            family = family,
            model = "mamglm",
            rank = rank_name)
}

mamgllvm_select <- function(object, 
                          data = NULL,
                          top_n = top_n) {
  
  models <- summary(object, top_n = top_n)[[1]]$models
 

  family <- object$family
  rank <- attr(object, "rank")
  y <- object$y
   
  if (is.null(data)) data  <- object$data 

  n_par <- sapply(strsplit(models, "\\+"), length)

  model.aic <- NULL
  log.L <- NULL
  n_samp <- nrow(data)
  n_sp <- ncol(data)
  for (i in 1:top_n) {
    f.str <- make.formula2(y, models[i])

      fit_fun <- function(){
        gllvm(
          y = get(y),
          X = data, 
          formula = formula(f.str),
          family = family,
          starting.val = "res")
      }

      fit.temp <- NULL
      fit.temp <- try(fit_fun(), silent = TRUE)

      if (class(fit.temp) == "try-error") {
        message("Use starting.val = 'zero' instead of 'res'")
        fit.temp <- gllvm(
          y = get(y),
          X = data, 
          formula = formula(f.str),
          family = family,
          starting.val = "zero")
      }

      fit.summary <- summary(fit.temp)

      log.L.temp <- fit.summary$`log-likelihood`
      log.L <- c(log.L, log.L.temp)

      if (is.null(rank) || rank == "AICc" || rank == "aicc") {
        ranks <- fit.summary$AICc
        rank_name <- "AICc"
      } else if (rank == "AIC" || rank == "aic") {
        ranks <- fit.summary$AIC
        rank_name <- "AIC"
      } else if (rank == "BIC" || rank == "bic") {
        ranks <- fit.summary$BIC
        rank_name <- "BIC"
      }
        model.aic <- c(model.aic, ranks)
    }

  min.aic <- min(model.aic)
  delta.aic <- model.aic - min.aic
  wAIC <- exp(-delta.aic / 2) / sum(exp(-delta.aic / 2))
  res <- data.frame(AIC = model.aic, log.L = log.L, delta.aic, wAIC, n.vars = n_par)
  colnames(res)[1] <- rank_name
  colnames(res)[3] <- paste0("delta.", rank_name)
  colnames(res)[4] <- waic <- paste0("w", rank_name)

  
  vars <- sapply(models, function(x)strsplit(x, " \\+ "))

##counting vars
#vars2 is matrix filled with 0 (row:sites,col:parameters)
  vars2 <- matrix(numeric(ncol(data) * top_n), nrow = top_n, ncol = ncol(data))
  colnames(vars2) <- colnames(data)
  n.vars <- ncol(data)
  n.size <- sapply(vars, length)

  for (i in 1:nrow(vars2)){ # each row (model number)
    for (j in 1:n.vars){ # each column (paramter)
      for (k in 1:n.size[i]){ # upto number of paramters
        if (colnames(vars2)[j] == vars[[i]][k]) vars2[i,j] <- 1
      }
    }
  }

  res <- cbind(res, vars2)
  res <- res[order(res[,paste(rank_name)]), ]
  rownames(res) <- NULL
#calculating weighted result of explanable variables
  res.temp <- res[, -1:-5]
  res2 <- apply(apply(res.temp, 2,
                      function(x)res[, paste(waic)] * x), 2, sum)
  out <- list(res.table = res, importance = res2, family = family, y = y, data = data)
  structure(out, 
            class = "mglmn",
            family = family,
            model = "mamgllvm",
            rank = rank_name)
}
