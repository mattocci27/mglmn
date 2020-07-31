#' Best variables
#'
#' Returns variables for the best model based on AIC
#'
#' @export
#' @param x A list of results of `mamglm` and `mamgllvm`
#' @return A vector of terms of the best model.
#' @seealso \code{\link{mamglm}}, \code{\link{mamgllvm}}
#' @examples
#' #load species composition and environmental data
#' data(capcay)
#' #use a subset of data in this example to reduce run time
#' env_assem <- capcay$env_assem[, 1:5]
#' freq.abs <- as.matrix(log(capcay$abund + 1))
#'
#' #to fit a gaussian regression model to frequency data:
#' res <- mamglm(data = env_assem, y = "freq.abs", family = "gaussian")
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
