#' @title
#' Check availability of a COPERNICUS tile
#' @description
#' Check whether the provided (H,V) pairs are available as COPERNICUS tiles
#' @usage
#' check_tile_copernicus(tileH,tileV)
#' @param tileH H index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param tileV V index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @details
#' \code{tileH} and \code{tileV} should have the same length and form pairs of (H,V) indices. Each tile corresponds to a file covering an area of 10 x 10
#' @return
#' a logical \code{vector} indicating which pair (H,V) is available
#' @references
#' Baret et al. 2010. BioPar Product User Manual. Geoland2: Towards an Operational GMES Land Monitoring Core Service, 42 p.
#' @author Antoine Stevens
#' @examples
#' check_tile_copernicus(19,4) # H19V4 is in Italy
#' check_tile_copernicus(0,4) # H0V4 is not available
#'
#' @export
check_tile_copernicus <- function(tileH, tileV) {

    if (length(tileH) != length(tileV))
        stop("tileH should have the same length as tileV")

    hv <- gen_tile_copernicus()
    hv$hv <- paste0("h", hv$h, "v", hv$v)
    HV <- paste0("h", tileH, "v", tileV)
    return(HV %in% (hv$hv))
}
