context("test-mamgllvm")

#library(mvtnorm)
#library(mvabund)
#set.seed(123)
#n_sp <- 20
#n_site <- 30
#k <- 4 # int + 3 env
#
##opt <- c(-1.96, -1, 0, 1, 1.96) 
#opt_fun <- function(quad, opt, int) {
#  list(a = quad,
#      b = -2 * quad * opt,
#      c = quad * opt^2 + int) 
#}
#
#sim_com <- function(n_sp, n_site, k = 4, quad = c(-1, -1, -1), opt = c(0, 0, 0), int = c(1, 1, 1)) {
#
#  env <- rmvnorm(n_site, rep(0, k - 1), diag(k - 1))
#  rownames(env) <- paste0("site", 1:n_site)
#  colnames(env) <- paste0("env", 1:(k-1))
#  env2 <- env^2
#  colnames(env2) <- paste0(colnames(env),"^2")
#  env_ori <- env
#  env <- cbind(env, env2)
#
#  par1 <- opt_fun(quad[1], opt[1], int[1])
#  par2 <- opt_fun(quad[2], opt[2], int[2])
#  par3 <- opt_fun(quad[3], opt[3], int[3])
#  beta <- c(par1$c+par2$c+par3$c, par1$b, par2$b, par3$b, par1$a, par2$a, par3$a)
##beta <- c(par1$a, par2$a, par3$a, par1$b, par2$b, par3$b, par1$c+par2$c+par3$c)
#
#  k2 <- 2 * (k - 1) +1
#  Beta <- rmvnorm(n_sp, beta, diag(k2)*1)
#
#  rownames(Beta) <- paste0("sp", 1:n_sp)
#  colnames(Beta) <- c("Intercept", colnames(env))
#  env_int <- cbind(int = rep(1, n_site), env)
#  lambda <- Beta %*% t(env_int)
#  lambda <- t(lambda)
#
#  Y <- matrix(0, nrow = nrow(lambda), ncol = ncol(lambda))
#  for (i in 1:nrow(lambda)) {
#    for (j in 1:ncol(lambda)) {
#      Y[i, j]  <- rpois(1, exp(lambda[i, j]))
#      #Y[i, j]  <- rnbinom(1, lambda[i, j], size = 100)
#    }
#  }
#  rownames(Y) <- rownames(env)
#  colnames(Y) <- rownames(Beta)
#  #list(samp = Y, env = env_ori, Sig = diag(k - 1), Beta = Beta, Cov_B = diag(k))
#  list(samp = Y, env = env_ori,  Beta = Beta)
#
#}
#
#out1 <- sim_com(n_sp, n_site = 10000, k = 4, 
#               quad = c(-1.5, 0, 0),
#               opt = c(0.8, 0, 0),
#               int = c(4, 0, 0)
#)
#
#rgl::plot3d(out1$samp[,5], out1$env[,1], out1$env[,2])
#
#plot(out1$samp[,5] ~ out1$env[,1])
#plot(out1$samp[,5] ~ out1$env[,2])
#plot(out1$samp[,5] ~ out1$env[,3])
#
#pairs(env)
#
#rgl::plot3d(out1$samp[,4], out1$env[,1], out1$env[,3])
#
#Y <- out1$samp
#env <-out1$env
#Beta <- out1$Beta
#
##source("./R/mamglm.R")
##source("./R/make.formula.R")
#fit1 <- mamglm(data = env, y = "Y", family = "poisson", rank = "AIC")
#summary(fit1)
#
#ses.mglmn(fit1, top_n = 10, runs = 99)
#
