\name{best.vars}
\alias{best.vars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Best variables
}
\description{
Returns variables for the best model based on AIC
}
\usage{
best.vars(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A list of results of maglm and mamglm.
}
}

\value{
A vector of terms for the best model.
}

\author{
Masatoshi Katabuchi <mattocci27@gmail.com>
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{maglm}}, 
\code{\link{mamglm}} 

}
\examples{
#load species composition and environmental data
data(capcay)
adj.sr <- capcay$adj.sr
env_sp <- capcay$env_sp

#to fit a poisson regression model:
res <- maglm(data=env_sp, y="adj.sr", family="gaussian")

best.vars(res)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
