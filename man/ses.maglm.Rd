\name{ses.maglm}
\alias{ses.maglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardized effect size of relative importance values for mamglm
}
\description{
Standardized effect size of relative importance values for model averaging mutlivariate GLM.
}
\usage{
ses.maglm(data, y, family,scale=TRUE, AIC.restricted = TRUE, par=FALSE, runs = 999)
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
Wheter to use AICc (TRUE) or AIC (FALSE) (default = TRUE). 
}

  \item{par}{
Wheter to use parallel computing (default = FALSE)
}

  \item{runs}{
Number of randomizations.
}
}
\details{
The currently implemented null model shuffles the set of environmental variables across sites, while maintains species composition. Note that the function would take considerable time to execute.
}
\value{
A data frame of resluts for each term
\item{res.obs }{Observed importance of terms}
\item{res.rand.mean }{Mean importance of terms in null communites}
\item{res.rand.sd }{Standard deviation of importance of terms in null communites}
\item{SES }{Standardized effect size of importance of terms (= (res.obs - res.rand.mean) / res.rand.sd)}
\item{res.obs.rank }{Rank of observed importance of terms vs. null communites}
\item{runs }{Number of randomizations}

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
\code{\link{maglm}},
\code{\link{mamglm}},
\code{\link{ses.mamglm}}
}
\examples{
#load species composition and environmental data
data(capcay)
adj.sr <- capcay$adj.sr
#use a subset of data in this example to reduce run time
env_sp <- capcay$env_sp[,1:5]

#to execute calculations on a single core:
ses.maglm(data=env_sp, y="adj.sr", par=FALSE,family="gaussian", runs=4)

\dontrun{
#to execute parallel calculations:
sfInit(parallel=TRUE, cpus=4)
sfExportAll()
ses.maglm(data=env_sp, y="adj.sr", par=TRUE,family="gaussian", runs=4)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
