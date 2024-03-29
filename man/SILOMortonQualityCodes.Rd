% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SILOMorton.R
\name{SILOMortonQualityCodes}
\alias{SILOMortonQualityCodes}
\title{Plot the quality codes of the input data for Morton's Evap calculations}
\usage{
SILOMortonQualityCodes(SILO, filename = NULL)
}
\arguments{
\item{SILO}{a list of sites with SILO data, as created by SILOLoad()}

\item{filename}{optional, filename to write a plot of the rainfall quality codes to, including extension (e.g. png). Filename can include full path or sub folders.}
}
\value{
a ggplot geom_tile plot of the rainfall quality codes
}
\description{
Produces a tile plot displaying the quality codes for variables that are input to the 
calculation of Morton's evaporation equations, being maximum and minimum temperature, solar radiation and vapor pressure (derived from wet bulb temperature).
Evaporation is also plotted, if the site has pan observations.
}
\examples{
 
\dontrun{
X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
p<-SILOMortonQualityCodes(X)
}

}
