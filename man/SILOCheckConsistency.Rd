% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SILOCheckConsistency.R
\name{SILOCheckConsistency}
\alias{SILOCheckConsistency}
\title{Check for homogeneity between SILO rainfall station data}
\usage{
SILOCheckConsistency(X, folder = NA, pvallim = 0.05, changelim = 0.025)
}
\arguments{
\item{X}{A list of SILO station data, in the format created by \code{\link[SWTools]{SILOLoad}}}

\item{folder}{Path to folder to save resulting images to. Will be created if it doesn't exist}

\item{pvallim}{p value limit of the break point detection to display the double mass break point. Defaults to p=0.05}

\item{changelim}{significant slope limit display the double mass break point. Defaults to a slope change of 0.025}
}
\value{
If folder is not specified (or \code{NA}) the plots are shown in the R environment. If folder is specified, a figure for each station in \code{X} is saved to \code{folder}. There are 4 panels on the figure:
\itemize{
\item Annual rainfall for a given station, against the average across all stations in X (except the station presented).
\item Cumulative residuals of the annual rainfall from the straight line regression shown in the first panel. Assuming the residuals are are independent random variables, this figure include ellipses representing 80th and 95th percentile that the hypothesis that there is no change in slope can be rejected.
\item double mass curve, plotting the cumulative annual rainfall for the station against the station average. If a breakpoint is identified, this is displayed on the plot.The colours represent the median quality code for each year, with the same colour palette as \code{\link[SWTools]{SILOQualityCodes}}
\item Residuals of the cumulative rainfall from the straight line fitted to the double mass curve.
}
}
\description{
Compute tests on rainfall double mass curves and cumulative deviation in annual rainfall totals to test for consistency between a rainfall station and the average of another group of stations.
Non-homogeneity can occur for a number of reasons, such as interception from vegetation or buildings over time, moving of a station location, or due to interpolation of missing data or station closure
}
\details{
Two tests are calculated by \code{SILOCheckConsistency}, which are outlined in [Annex 4](https://www.fao.org/3/x0490e/x0490e0l.htm#annex%204.%20statistical%20analysis%20of%20weather%20data%20sets%201) of Allen et al. (1998).

The first considers the residual errors in annual rainfall at a station, compared to the straight line (intercept=0) regression with the average annual rainfall from the other sites in \code{X}. 
The residuals should follow a normal distribution with mean zero and standard deviation s_y,x. The annual rainfall data is plotted to visually assess the homoscedasticity requirement (constant variance).
Ellipses for 80\% and 95\% confidence in rejecting the homogeneity hypothesis are plotted on the cumulative residuals figure. 

The second test tests for a break point in the plot of cumulative annual rainfall, commonly referred to as a double-mass analysis. 
This analysis is outlined in Allen et al. (1998) and also Chang and Lee (1974). A bootstraped estimate of any breakpoint in the double-mass plot, indicating a change in the relationship between rainfall at the station and the average of all other stations,
is assessed using the method of Muggeo (2003), as provided in \link[segmented]{segmented}.
}
\examples{
\dontrun{
X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
SILOCheckConsistency(X,tempdir())
}
}
\references{
Chang, M., and Lee, R. (1974) Objective double-mass analysis, Water Resour. Res., 10( 6), 1123-1126, doi:10.1029/WR010i006p01123.

Allan, R., Pereira, L. and Smith, M. (1998) Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. 

Muggeo, V.M.R. (2003) Estimating regression models with unknown break-points. Statistics in Medicine 22, 3055-3071.
}
\seealso{
\link[SWTools]{SILOLoad}, \link[SWTools]{SILOSiteSummary}, \link[SWTools]{SILOQualityCodes},\link[SWTools]{SILOCorrectSite}
}
