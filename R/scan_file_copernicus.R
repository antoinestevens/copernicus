#' @title Scan COPERNICUS file(s) name
#' @description Extract COPERNICUS product characteristics from file names
#' @usage
#' scan_file_copernicus(fnames)
#' @param fnames \code{character} vector of file name(s)
#' @return
#' a \code{data.frame} with variables 'input' (input name(s)), 'product', 'date', 'tile', 'sensor' (VGT or PROBAV), 'version' (version of the algorithm)
#' @details
#' The geoland2-BioPar product follows the following naming  standard:
# g2_BIOPAR_<Acronym>_<YYYYMMDDHHMM>_<AREA>_<SENSOR>_V<Major.Minor>
#' @references
#' Baret et al. 2010. BioPar Product User Manual. Geoland2: Towards an Operational GMES Land Monitoring Core Service, 42 p.
#' @author Antoine Stevens
#' @examples
#' fnames <- c('g2_BIOPAR_NDVI_201102030000_H19V4_VGT_V1.3.zip',
#'             'g2_BIOPAR_NDVI_201410010000_H19V4_PROBAV_V2.1.zip')
#' scan_file_copernicus(fnames)
#'
#' @export
scan_file_copernicus <- function(fnames) {
    fnames <- basename(fnames)
    s <- stringr::str_match(basename(fnames), "g2_BIOPAR_([[:alnum:]]+)_([[:digit:]]{8})[[:digit:]]{4}_([[:alnum:]]+)_([[:alnum:]]+)_V(.\\..)")
    colnames(s) <- c("input", "product", "date", "tile", "sensor", "version")
    return(as.data.frame(s))
}
