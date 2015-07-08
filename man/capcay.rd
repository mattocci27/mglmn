\name{capcay}
\alias{capcay}
\docType{data}
\title{ Capcay data }
\description{
Species composition and environmental data from Capricornia Cays}
\usage{data(capcay)}
\format{
A list containing the elements
\describe{
\item{abund }{
A data frame with 14 observations of abundance of 13 ant species
}

\item{adj.sr }{
A vector of adjusted species richness of ants based on sample-based rarefaction curves to standardise sampling intensity across sites (see Nakamura et al. 2015 for more details).
}

\item{env_sp }{
A data frame of 10 environmental variables, which best explained the variation in the matrix of similarity values.}

\item{env_assem }{
A data frame of 10 environmental variables, which best explained the variation in the matrix of similarity values.}
}

The data frame \code{abund} has the following variables:
\describe{
\item{Camponotus.mackayensis }{(numeric) relative abundance of \emph{Camponotus mackayensis}}

\item{Cardiocondyla..nuda }{(numeric) relative abundance of \emph{Cardiocondyla nuda}}

\item{Hypoponera.sp..A }{(numeric) relative abundance of \emph{Hypoponera }spA}

\item{Hypoponera.sp..B }{(numeric) relative abundance of \emph{Hypoponera }spB}

\item{Iridomyrmex.sp..A }{(numeric) relative abundance of \emph{Iridomyrmex }spA}

\item{Monomorium.leave  }{(numeric) relative abundance of \emph{Monomorium leave}}

\item{Ochetellus.sp..A }{(numeric) relative abundance of \emph{Ochetellus }spA}

\item{Paratrechina.longicornis }{(numeric) relative abundance of \emph{Paratrechina longicornis}}

\item{Paratrechina.sp..A }{(numeric) relative abundance of \emph{Paratrechina }spA}

\item{Tapinoma.sp..A  }{(numeric) relative abundance of \emph{Tapinoma }spA}

\item{Tetramorium.bicarinatum }{(numeric) relative abundance of \emph{Tetramorium bicarinatum}}

}


The data frame \code{env_sp} has the following variables:
\describe{
\item{NativePlSp }{(numeric) native plant species richness}
\item{P.megaAbund }{(numeric) log-transformed relative abundance of \emph{Pheidole megacephala}}
\item{P.megaPA }{(numeric) presence/absence of \emph{Pheidole megacephala}}
\item{HumanVisit }{(numeric) presence/absence of frequent human visitiation}
\item{MaxTemp }{(numeric) mean daily maximum temp(degree celsius)}
\item{Rain4wk }{(numeric) total rainfall in the past 4 weeks (mm)}
\item{DistContinent }{(numeric) distance to the nearest continent (km)}
\item{DistNrIs }{(numeric) log-transformed distance to the nearest island (km)}
\item{Y }{(numeric) Y coordinate}
\item{XY }{(numeric) X coordinate * Y coordinate} 

	}


The data frame \code{env_assem} has the following variables:
\describe{
\item{IslandSize }{(numeric) log-transformed island size (ha)}

\item{ExoticPlSp }{(numeric) log-transformed exotic plant species richness}
\item{NativePlSp }{(numeric) native plant species richness}
\item{P.megaPA }{(numeric) presence/absence of \emph{Pheidole megacephala}}
\item{HumanVisit }{(numeric) presence/absence of frequent human visitiation}
\item{Rainsamp }{(numeric) log-transformed total rainfall during sampling (mm)}
\item{DistContinent }{(numeric) distance to the nearest continent (km)}
\item{DistNrIs }{(numeric) log-transformed distance to the nearest island (km)}
\item{Y }{(numeric) Y coordinate}
\item{XY }{(numeric) X coordinate * Y coordinate} 

	}
}

\references{
Nakamura A., Burwell C.J., Lambkin C.L., Katabuchi M., McDougall A., Raven R.J. and Neldner V.J. (2015), The role of human disturbance in island biogeography of arthropods and plants: an information theoretic approach, Journal of Biogeography, DOI: 10.1111/jbi.12520
}

\keyword{datasets}
