\name{mamglm}
\alias{mamglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model averaging for multivariate generalized linear models
}
\description{
Model averaging for multivariate GLM based on information theory.
}
\usage{
mamglm(data, y, family, scale = TRUE, AIC.restricted = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
}
  \item{y}{
Name of 'mvabund' object (character)}
  \item{family}{
the 'family' object used.
}
  \item{scale}{
Whether to scale independent variables (default = TRUE)
}
  \item{AIC.restricted}{
Wheter to use AICc (TRUE) or AIC (FALSE) (default = TRUE). 
}
}

\value{
A list of results

\item{res.table }{data frame with "AIC", AIC of the model, "log.L", log-likelihood of the model, "delta.aic", AIC difference to the best model, "wAIC", weighted AIC to the model, "n.vars", number of variables in the model, and each term.}

\item{importance }{vector of relative importance value of each term, caluclated as as um of the weighted AIC over all of the model in whith the term aperars.}

\item{family }{the 'family' object used.}


%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.

Wang, Y., Naumann, U., Wright, S.T. & Warton, D.I. (2012) mvabund- an R package for model-based analysis of multivariate abundance data. Methods in Ecology and Evolution, 3, 471-474.

Warton, D.I., Wright, S.T. & Wang, Y. (2012) Distance-based multivariate analyses confound location and dispersion effects. Methods in Ecology and Evolution, 3, 89-101.

Nakamura A., Burwell C.J., Lambkin C.L., Katabuchi M., McDougall A., Raven R.J. and Neldner V.J. (2015), The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach, Journal of Biogeography, DOI: 10.1111/jbi.12520
}

\author{
Masatoshi Katabuchi <mattocci27@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maglm}},
\code{\link{ses.maglm}},
\code{\link{ses.mamglm}}}
\examples{
#load species composition and environmental data
data(capcay)
#use a subset of data in this example to reduce run time
env_assem <- capcay$env_assem[,1:5]
freq.abs <- mvabund(log(capcay$abund+1))

#to fit a gaussian regression model to frequency data:
mamglm(data=env_assem,y="freq.abs",family="gaussian")

#to fit a binomial regression model to presence/absence data"
pre.abs0 <- capcay$abund
pre.abs0[pre.abs0>0] = 1
pre.abs <- mvabund(pre.abs0)

mamglm(data=env_assem,y="pre.abs",family="binomial")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line





