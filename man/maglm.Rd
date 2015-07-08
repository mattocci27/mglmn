\name{maglm}
\alias{maglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Model averaging for generalized linear models
}
\description{
Model averaging for GLM based on information theory.
}
\usage{
maglm(data, y, family, scale = TRUE, AIC.restricted = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
}
  \item{y}{
Vector of independent variables.
}
  \item{family}{
the 'family' object used.
}
  \item{scale}{
Whether to scale independent variables (default = TRUE)
}
  \item{AIC.restricted}{
Whether to use AICc (TRUE) or AIC (FALSE) (default = TRUE). 
}
}


\value{
A list of results

\item{res.table }{data frame with "AIC", AIC of the model, "log.L", log-likelihood of the model, "delta.aic", AIC difference to the best model, "wAIC", weighted AIC to the model, "n.vars", number of variables in the model, and each term.}

\item{importance }{vector of relative importance value of each term, caluclated as as um of the weighted AIC over all of the model in whith the term aperars.}

\item{family }{the 'family' object used.}

}
\references{
Dobson, A. J. (1990) An Introduction to Generalized Linear Models. London: Chapman and Hall.

Burnham, K.P. & Anderson, D.R. (2002) Model selection and multi-model inference: a practical information-theoretic approach. Springer Verlag, New York.

Nakamura A., Burwell C.J., Lambkin C.L., Katabuchi M., McDougall A., Raven R.J. and Neldner V.J. (2015), The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach, Journal of Biogeography, DOI: 10.1111/jbi.12520
}
\author{
Masatoshi Katabuchi <mattocci27@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mamglm}},
\code{\link{ses.maglm}},
\code{\link{ses.mamglm}}

}
\examples{
#load sspecies composition and environmental data
data(capcay)
adj.sr <- capcay$adj.sr
env_sp <- capcay$env_sp

#to fit a regression model:
maglm(data=env_sp, y="adj.sr", family="gaussian",AIC.restricted=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
