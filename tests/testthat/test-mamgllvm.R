# library(gglvm)
#rm(list=ls())
#source("./R/mamglm.R")
#library(mvabund)
#data(capcay)
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
