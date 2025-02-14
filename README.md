
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SWTools: Helper Tools for Australian Hydrologists

<!-- badges: start -->

[![R-CMD-check](https://github.com/matt-s-gibbs/swtools/workflows/R-CMD-check/badge.svg)](https://github.com/matt-s-gibbs/swtools/actions)
<!-- badges: end -->

Functions to speed up workflow for hydrological analysis. Focused on
Australian climate data (SILO climate data), hydrological models (eWater
Source) and South Australian hydrological data (from [Water Data
SA](https://water.data.sa.gov.au)).

## SILO functions:

SILO is a database of Australian climate data from 1889 to the present.
It provides daily meteorological datasets for a range of climate
variables in ready-to-use formats suitable for biophysical modelling,
research and climate applications [SILO
Website](https://www.longpaddock.qld.gov.au/silo/).

These functions allow SILO data to be downloaded from the [SILO
Website](https://www.longpaddock.qld.gov.au/silo/), imported into R,
calculate some basic statistics and undertake some quality assurance
tests to easily visualise how much data has been interpolated, and to
compare nearby sites to identify potential data issues. *SILODownload*,
*SILOLoad* and *SILOReport* functions allow a vector of SILO sites to be
downloaded and summarised in a Microsoft Word report.

## Source and Veneer functions:

[eWater Source](https://ewater.org.au/ewater-solutions/tools/source/) is the
Australia’s national hydrological modelling platform, and is increasing
in use around the world. Functions are included to write SILO climate
data to the format expected for Source *SILOWriteforSource*, and reading
in model outputs, *read\_res.csv*.

[Veneer](https://www.flowmatters.com.au/articles/introducing_veneer.html)
is a RESTful API for interacting with Source models. Functions are
included that are wrappers for Veneer, to build URLs to get or set data
in the Source model, and process the json object returned.

## Aquarius functions:

South Australia’s hydrological data is hosted on [Water Data
SA](https://water.data.sa.gov.au), powered by an Aquatic Informatics
Aquarius database. The [Export
link](https://water.data.sa.gov.au/Data/Export) creates URLs that enable
multiple datasets to be downloaded. *AQWPDownload* builds these URLs to
download data in json format, and *AWQPLoad* loads this json file into
the R interface.

## Installation

You can install the released version of SWTools from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("SWTools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("matt-s-gibbs/swtools")
```

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- library(SWTools) -->
<!-- ## basic example code -->
<!-- ``` -->
