#' SWTools: Helper Tools for Australian Hydrologists
#'
#' Functions to speed up workflow for hydrological analysis. 
#' Focused on Australian climate data (SILO climate data), hydrological models (eWater Source) and South Australian hydrological data 
#' (from \href{https://water.data.sa.gov.au}{Water Data SA}).
#' 
#' @section SILO functions:
#' 
#' SILO is a database of Australian climate data from 1889 to the present. It provides daily meteorological datasets for a range of climate variables 
#' in ready-to-use formats suitable for biophysical modelling, research and climate applications \href{https://www.longpaddock.qld.gov.au/silo/}{SILO Website}.
#' 
#' These functions allow SILO data to be downloaded from the \href{https://www.longpaddock.qld.gov.au/silo/}{SILO Website}, imported into R, 
#' calculate some basic statistics and undertake some quality assurance tests to easily visualise how much data has been interpolated, and to compare nearby sites to identify potential data issues.
#' \code{\link{SILODownload}},\code{\link{SILOLoad}} and \code{\link{SILOReport}} functions allow a vector of SILO sites to be downloaded and summarised in a Microsoft Word report.
#' 
#' @section Source and Veneer functions:
#' \href{https://ewater.org.au/products/ewater-source/}{eWater Source} is the Australia's national hydrological modelling platform, and is increasing in use around the world.
#' Functions are included to write SILO climate data to the format expected for Source \code{\link{SILOWriteforSource}},
#'  and reading in model outputs, \code{\link{read_res.csv}}.
#' 
#' \href{https://www.flowmatters.com.au/articles/introducing_veneer.html}{Veneer} is a RESTful API for interacting with Source models.
#' Functions are included that are wrappers for Veneer, to build URLs to get or set data in the Source model, and process the json object returned.
#' 
#' @section Aquarius functions:
#' South Australia's hydrological data is hosted on \href{https://water.data.sa.gov.au}{Water Data SA}. 
#' The \href{https://water.data.sa.gov.au/Data/Export}{Export link} creates URLs that enable multiple datasets to be downloaded.
#' \code{\link{AQWPDownload}} builds these URLs to download data in json format, and \code{\link{AQWPLoad}} loads this json file into the R interface.
#'
#' @docType package
#' @name SWTools
#'
#'@importFrom magrittr %>% 
#'@importFrom rlang .data
#' @aliases SWTools-package
NULL