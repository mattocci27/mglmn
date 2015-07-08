\name{make.formula}
\alias{make.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility function
}
\description{
Utility function for data manipulation, which is implemented in \link{maglm} and \link{mamglm}. 
}
\usage{
make.formula(lhs, var.list)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lhs}{
Numeric vector of dependent variables.
}
  \item{var.list}{
Character vector of independet variables.
}
}

\value{
an object of class '"formula"' %%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Masatoshi Katabuchi <mattocci27@gmail.com>}

\seealso{
\code{\link{maglm}},
\code{\link{mamglm}}}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
