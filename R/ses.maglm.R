#' Standardized effect size of relative importance values for mamglm
#'
#' Standardized effect size of relative importance values for model averaging mutlivariate GLM.
#'
#' The currently implemented null model shuffles the set of environmental variables across sites, while maintains species composition. Note that the function would take considerable time to execute.
#'
#' @name ses.maglm
#'
#' @export
#' @param data Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
#' @param y Vector of independent variables.
#' @param family the 'family' object used.
#' @param scale Whether to scale independent variables (default = TRUE)
#' @param AIC.restricted Wheter to use AICc (TRUE) or AIC (FALSE) (default = TRUE). 
#' @param par Wheter to use parallel computing (default = FALSE)
#' @param runs Number of randomizations.
#' @return A data frame of resluts for each term
#' @return \item{res.obs}{Observed importance of terms}
#' @return \item{res.rand.mean}{Mean importance of terms in null communites}
#' @return \item{res.rand.sd}{Standard deviation of importance of terms in null communites}
#' @return \item{SES}{Standardized effect size of importance of terms (= (res.obs - res.rand.mean) / res.rand.sd)}
#' @return \item{res.obs.rank}{Rank of observed importance of terms vs. null communites}
#' @return \item{runs}{Number of randomizations}
#' @references Dobson, A. J. (1990) An Introduction to Generalized Linear Models. London: Chapman and Hall.
#' @references Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.
#' @references{Nakamura, A., C. J. Burwell, C. L. Lambkin, M. Katabuchi, A. McDougall, R. J. Raven, and V. J. Neldner. (2015) The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach. Journal of Biogeography 42:1406-1417.}
#' @seealso\code{\link{maglm}}, \code{\link{mamglm}}, \code{\link{ses.mamglm}}
#' @examples
#' library(mvabund)
#' #load species composition and environmental data
#' data(capcay)
#' adj.sr <- capcay$adj.sr
#' #use a subset of data in this example to reduce run time
#' env_sp <- capcay$env_sp[, 1:5]
#'
#' #to execute calculations on a single core:
#' ses.maglm(data = env_sp, y = "adj.sr", par = FALSE, 
#'          family = "gaussian", runs = 4)
#'
#' \dontrun{
#' #to execute parallel calculations:
#' sfInit(parallel = TRUE, cpus = 4)
#' sfExportAll()
#' ses.maglm(data = env_sp, y = "adj.sr", par = TRUE,
#'          family = "gaussian", runs = 4)
#' }
#'

ses.maglm <- function(data, y, family, scale = TRUE, AIC.restricted = TRUE,
                      par = FALSE, runs = 999){
  if (scale) data <- as.data.frame(scale(data))
  res.obs <- maglm(data, y = y, family, AIC.restricted = AIC.restricted)$importance
  null.env.list <- list()
  for (i in 1:runs){
    data.temp <- data
    data.temp$temp <- rnorm(nrow(data.temp))
    data.temp <- data.temp[order(data.temp$temp), ]
    null.env.list[[i]] <- data.temp[, -ncol(data.temp)]
  }
  if (par == FALSE) {
    res.rand0 <- sapply(null.env.list,
                        function(x){mamglm(x, y = y, family,
                                   AIC.restricted = AIC.restricted)$importance})
  } else {
    res.rand0 <- sfSapply(null.env.list,
                          function(x){maglm(x, y = y,
                          family, AIC.restricted = AIC.restricted)$importance})}
  res.rand <- t(res.rand0) #tranpose (row <-> column)
  res.rand.mean <- apply(res.rand, 2, mean, na.rm = T)
  res.rand.sd <- apply(res.rand, 2, sd, na.rm = T)
  SES <- (res.obs - res.rand.mean) / res.rand.sd
  res.obs.rank <- apply(rbind(res.obs, res.rand), 2, rank)[1, ]
  data.frame(res.obs, res.rand.mean, res.rand.sd, SES, res.obs.rank, runs)
}
