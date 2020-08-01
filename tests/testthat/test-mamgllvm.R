context("test-mamgllvm")

#data(capcay)
#freq.abs <- log(capcay$abund + 1) %>% as.matrix
#
#fit <- gllvm(
#             y = freq.abs,
#             family = "gaussian")
#
#
#
##library(gglvm)
###rm(list=ls())
#source("./R/mamglm.R")
#library(mvabund)
#data(capcay)
#freq.abs <- log(capcay$abund + 1) %>% as.matrix
#
#        fit <- gllvm(
#          y = freq.abs,
#          family = "gaussian",
#          starting.val = "zero")
#
##use a subset of data in this example to reduce run time
#env_assem <- capcay$env_assem[, 1:5]
#freq.abs <- log(capcay$abund + 1) %>% as.matrix
##to fit a gaussian regression model to frequency data:
##y <- freq.abs %>% as.matrix
#mamglm(data = env_assem, y = "freq.abs", family = "gaussian")
#
#quote(y)
#
#q <- "old"
#test <- function () {
##   assign("q", "new", envir = .GlobalEnv)
#  q <- "new"
#}
#
#test()
#q  # display the new value
#
#library(mvabund)
#data(spider)
#spiddat <- mvabund(spider$abund)
#X <- spider$x
##To fit a log-linear model assuming counts are poisson:
#glm.spid <- manyglm(as.matrix(spiddat)~X, family="poisson")
#glm.spid <- manyglm(spiddat~X, family="poisson")


#set.seed(123)
#n_sp <- 20
#n_site <- 15
#k <- 5
#
##beta <- c(1, rnorm(k - 1))
#env <- mapply(rnorm, rep(n_site, k-1))
#env <- cbind(int = rep(1, n_site), env)
#rownames(env) <- paste0("site", 1:n_site)
#colnames(env)[-1] <- paste0("env", 1:(k-1))
#
## species response
#beta1 <- c(0.8, 0, 0.6, -0.8, 0)
#beta2 <- c(0.8, 0.6, -0.6, 0, 0)
##Beta <- mapply(rnorm, rep(n_sp, k), t(beta), 0.1)
#Beta1 <- mapply(rnorm, rep(n_sp/2, k), t(beta1), 0.1)
#Beta2 <- mapply(rnorm, rep(n_sp/2, k), t(beta2), 0.1)
#Beta <- rbind(Beta1, Beta2)
#rownames(Beta) <- paste0("sp", 1:n_sp)
#colnames(Beta) <- colnames(env)
##t(Beta)
#
#lambda <- Beta %*% t(env)
#lambda <- t(lambda)
#
#Y <- matrix(0, nrow = nrow(lambda), ncol = ncol(lambda))
#for (i in 1:nrow(lambda)) {
#  for (j in 1:ncol(lambda)) {
#    Y[i, j]  <- rpois(1, exp(lambda[i, j]))
#  }
#}
##y <- apply(lambda, 1, function(x)rpois(1,exp(x)))
##Y <- matrix(y, nrow = n_site) 
#
##mat <- mapply(function(x)rpois(1,exp(x)), lambda)
##matrix(mat, nrow=15)
#
#colnames(Y) <- colnames(lambda)
#rownames(Y) <- rownames(lambda)
#
#source("./R/mamglm.R")
#library(mvabund)
#fit <- manyglm(Y ~ env1 + env2 + env3 + env4, data = data.frame(env), family = "poisson")
#
#mean(fit$two.loglike/2)
#sum(fit$two.loglike/2)
#
#sum(fit$aic)
#mamglm(data = env[,-1], y = "Y", family = "poisson", rank = "AIC")
#
##library(mglmn)
#fit2 <- mamglm(data = env[,-1], y = "Y", family = "poisson", rank = "AIC")
#
#mamglm(data = env[,-1], y = "Y", family = "poisson")
##system.time(res2 <- ses.mamglm(data = env[,-1], y = "Y", family = "poisson", runs = 99))
##fit2
#
#fit3 <- mamgllvm(data = env[,-1], y = "Y", family = "poisson")
