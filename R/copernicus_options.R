# https://cran.r-project.org/web/packages/settings/vignettes/settings.html Variable, global
# to package's namespace.  This function is not exported to user space and does not need to
# be documented.
MYPKGOPTIONS <- settings::options_manager(downloadPath = "zip",
    outPath = "tif", outProj = "+init=epsg:32662", pixelSize = "asIn",
    gdalPath = "C:/OSGeo4W64/bin/", server = "http://land.copernicus.vgt.vito.be/PDF///datapool/Vegetation/",
    user = "", password = "", ntry = 5, resamplingType = "near")

# User function that gets exported:

#' @title Set or get options for the \code{copernicus} package
#' @usage copernicus_options(...)
#' @description Set or get parameters used in \code{\link{download_copernicus}} and \code{\link{extract_copernicus}}
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'    \item{\code{downloadPath}} {Path where the data should be downloaded}
#'    \item{\code{outPath}} {Path where the output of \code{get_copernicus} should be stored}
#'    \item{\code{outProj}} {\code{character} string in the PROJ.4 format (see \code{?\link[sp]{CRS}}). Default to '+init=epsg:32662', corresponding to the Plate Carr\'ee WGS 84}
#'    \item{\code{pixelSize}} {\code{character} string in the PROJ.4}
#'    \item{\code{resamplingType}} {Default resampling method. Default is 'near'. See \code{\link[gdalUtils]{gdalwarp}} for other options}
#'    \item{\code{gdalPath}} {Default is 'C:/OSGeo4W64/bin/' (this is when the OSGEO suite has been installed)}
#'    \item{\code{server}} {Default is 'http://land.copernicus.vgt.vito.be/PDF///datapool/Vegetation/'}
#'    \item{\code{user}} {User name to access \href{http://www.copernicus.eu/main/data-access}{COPERNICUS data portal}. Parameter of \code{\link{download_copernicus}}}
#'    \item{\code{password}} {Password associate with user name to access COPERNICUS data portal. Parameter of \code{\link{download_copernicus}}}
#'    \item{\code{ntry}} {Number of tries after non-succes download response from the server (default = 5). Increasing the value increases chances to actually get the data. Parameter of \code{\link{download_copernicus}}}
#' }
#' @note \code{resamplingType} is not working yet. Use function argument instead.
#' @examples
#' copernicus_options('user' = 'nicolaus', password = 'copernicus')
#' copernicus_options()
#'
#' @export
copernicus_options <- function(...) {
    # protect against the use of reserved words.
    settings::stop_if_reserved(...)
    MYPKGOPTIONS(...)
}

#' Reset global options for pkg
#'
#' @export
reset_options <- function() settings::reset(MYPKGOPTIONS)
