\name{ses.mamglm}
\alias{ses.mamglm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standardized effect size of relative importance values for mamglm
}
\description{
Standardized effect size of relative importance values for model averaging GLM.
}
\usage{
ses.mamglm(data, y, family, scale = TRUE,  AIC.restricted = TRUE, par=FALSE, runs = 999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame, typically of environmental variables. Rows for sites and colmuns for environmental variables.
}
  \item{y}{
Name of 'mvabund' object (character)
}

 \item{scale}{
Whether to scale independent variables (default = TRUE)
}

  \item{family}{
the 'family' object used.
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
%%  ~Describe the value returned
A data frame of resluts for each term
\item{res.obs }{Observed importance of terms}
\item{res.rand.mean }{Mean importance of terms in null communites}
\item{res.rand.sd }{Standard deviation of importance of terms in null communites}
\item{SES }{Standardized effect size of importance of terms (= (res.obs - res.rand.mean) / res.rand.sd)}
\item{res.obs.rank }{Rank of observed importance of terms vs. null communites}
\item{runs }{Number of randomizations}

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
\code{\link{mamglm}},
\code{\link{ses.maglm}}
}
\examples{
#load species composition and environmental data
data(capcay)
#use a subset of data in this example to reduce run time
env_assem <- capcay$env_assem[,1:5]
pre.abs0 <- capcay$abund
pre.abs0[pre.abs0>0] = 1
pre.abs <- mvabund(pre.abs0)

#to execute calculations on a single core:
ses.mamglm(data=env_assem,y="pre.abs", par=FALSE, family="binomial",AIC.restricted=FALSE,runs=4)


\dontrun{
#to execute parallel calculations:
sfInit(parallel=TRUE, cpus=4)
sfExportAll()
ses.mamglm(data=env_assem,y="pre.abs", par=TRUE, family="binomial",AIC.restricted=FALSE,runs=4)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
