#' Standardized effect size of relative importance values for mamglm
#'
#' Standardized effect size of relative importance values for model averaging GLM.
#'
#' The currently implemented null model shuffles the set of environmental variables across sites, while maintains species composition. Note that the function would take considerable time to execute.
#'
#' @name ses.mamglm
#'
#' @export
#' @param data Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
#' @param y Name of 'mvabund' object (character)
#' @param scale Whether to scale independent variables (default = TRUE)
#' @param family the 'family' object used.
#' @param AIC.restricted Wheter to use AICc (TRUE) or AIC (FALSE) (default = TRUE).
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
#' pre.abs <- mvabund(pre.abs0)
#'
#' #to execute calculations on a single core:
#' ses.mamglm(data = env_assem, y = "pre.abs",
#'            par = FALSE, family = "binomial",
#'            AIC.restricted=FALSE,runs=4)
#'
#' \dontrun{
#' #to execute parallel calculations:
#' sfInit(parallel = TRUE, cpus = 4)
#' sfExportAll()
#' ses.mamglm(data = env_assem, y = "pre.abs",
#'            par = TRUE, family = "binomial",
#'            AIC.restricted = FALSE, runs = 4)
#' }

ses.mamglm <- function(data, y, family, scale = TRUE,
                      AIC.restricted = TRUE, par = FALSE, runs = 999){
  if (scale) data <- as.data.frame(scale(data))
  res.obs <- mamglm(data,y = y,scale = scale, family,
                    AIC.restricted = AIC.restricted)$importance
  # runs<-2#
  null.env.list <- list()
  # data <- env2
  # before<-proc.time()
  for (i in 1:runs){
    data.temp <- data
    data.temp$temp <- rnorm(nrow(data.temp))
    data.temp <- data.temp[order(data.temp$temp), ]
    null.env.list[[i]] <- data.temp[, -ncol(data.temp)]
  }

  # y <- "pre.abs"
  # family <-"binomial"
  # AIC.restricted=F
  if (par == FALSE) {
    res.rand0 <- sapply(null.env.list,
                        function(x){mamglm(x, y = y, 
                                           scale = scale, 
                                           family,
                                           AIC.restricted = AIC.restricted)$importance
                  })
  } else {
    res.rand0 <- sfSapply(null.env.list,
                          function(x){mamglm(x,
                                             y = y,
                                             scale = scale,
                                             family,
                                             AIC.restricted = AIC.restricted)$importance
                                             })
  }
  res.rand <- t(res.rand0) #tranpose (row <-> column)
  res.rand.mean <- apply(res.rand, 2, mean, na.rm = T)
  res.rand.sd <- apply(res.rand, 2, sd, na.rm = T)
  SES <- (res.obs - res.rand.mean) / res.rand.sd
  res.obs.rank <- apply(rbind(res.obs, res.rand), 2, rank)[1, ]
  data.frame(res.obs, res.rand.mean, res.rand.sd, SES, res.obs.rank, runs)
}
