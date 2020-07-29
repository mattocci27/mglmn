#' Best variables
#'
#' Returns variables for the best model based on AIC
#'
#' @export
#' @param x A list of results of `maglm` and `mamglmg`
#' @return A vector of terms of the best model.
#' @seealso \code{\link{maglm}}, \code{\link{mamglm}}
#' @examples
#' #load species composition and environmental data
#' data(capcay)
#' adj.sr <- capcay$adj.sr
#' env_sp <- capcay$env_sp
#' 
#' #to fit a poisson regression model:
#' res <- maglm(data = env_sp, y = "adj.sr", family = "gaussian")
#' 
#' best.vars(res)

best.vars <- function(x){
  temp <- ifelse(x[[1]][1, -1:-5] == 1, colnames(x[[1]][, -1:-5]), NA)
  temp2 <- NULL
  j <- 1
  for (i in 1:length(temp)){
  if (is.na(temp[i]) == F){
    temp2[j] <- temp[i]
    j <- j + 1
    }
  }
  temp2
}
