SWTStartupMessage <- function()
{
  # Startup message obtained as 
  # > figlet -f slant MCLUST
  msg <- c(paste0("SWTools version ", 
    utils::packageVersion("SWTools")),
    "\nIf you find SWTools useful please consider citing:\n
    Gibbs M.S, Alcorn M. & Vaze, J. (2023) The SWTools R package for SILO
    data acquisition, homogeneity testing and correction, 
    Australasian Journal of Water Resources, DOI: 10.1080/13241583.2023.2214989
    \nor type: citation(\"SWTools\")")
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # unlock .mclust variable allowing its modification
 # unlockBinding(".mclust", asNamespace("mclust")) 
  # startup message
  msg <- SWTStartupMessage()
  packageStartupMessage(msg)      
  invisible()
}