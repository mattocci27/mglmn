#' mglmn: Model Averaging for Multivariate Generalized Linear Models
#'
#' This package provide tools for univariate and multivariate generalized linear models with model averaging and null model technique (Nakamura et al. 2015).
#'
#' \describe{
#'  The package provides functions to estimate the relative importance of predictor variables in univariate and multivariate generalized linear models. The relative importance predictor variables are calculated by summing the Akaike weights of all models in which that predictor variable is included (Burnham & Anderson, 2002). The sum of the Akaike weights indicates the importance of a variable in explaining variation in a given dataset, relative to other predictor variables included in the analysis. The significance of each predictor variable can be assessed by the null model approach.
#'
#' \item{\code{\link{maglm}}}{
#' Model averaging for GLM based on information theory.
#' }
#' \item{\code{\link{mamglm}}}{
#' Model averaging for multivariate GLM based on information theory.
#' }
#' \item{\code{\link{ses.maglm}}}{Standardized effect size of relative importance values for model averaging GLM.
#' }
#' \item{\code{\link{ses.mamglm}}}{Standardized effect size of relative importance values for model averaging mutlivariate GLM.}}
#'
#' @author Masatoshi Katabuchi <mattocci27@gmail.com> and Akihiro Nakamura
#'
#' @references Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.
#' @references Nakamura, A., C. J. Burwell, C. L. Lambkin, M. Katabuchi, A. McDougall, R. J. Raven, and V. J. Neldner. (2015) The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach. Journal of Biogeography 42:1406-1417.
#'
#' @keywords internal
"_PACKAGE"

#' @import mvabund
#' @import snowfall
#' @importFrom stats glm logLik rnorm sd
#' @importFrom utils combn
NULL
