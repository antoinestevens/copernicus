#' @title
#' Get COPERNICUS tile(s) for a given extent
#' @description
#' Get COPERNICUS tile(s) position and name for a given extent or tile (H,V) pairs
#' @usage
#' get_tile_copernicus(extent,tileH,tileV,...)
#' @param extent an object of class \code{Raster*}, \code{Extent} or \code{SpatialPolygons*} giving the extent from which tiles info is retrieved
#' @param tileH H index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param tileV V index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param ... argument passed to \code{\link{gen_tile_copernicus}}
#' @return \code{data.frame} with tile(s) info: h, v, xmin, xmax, ymin, ymax
#' @details
#' One should choose to use either an extent or pairs of (H,V) values
#' If \code{extent} is of class \code{Extent}, then it is supposed to be in geographical coordinates.
#' If \code{extent} is of class \code{Raster*} or \code{SpatialPolygons*} they should have a coordinate system.
#' Part of the code has been grabed from the \code{MODIS} package v0.31
#' @author Antoine Stevens and Matteo Mattiuzzi (MODIS package)
#' @examples
#' library(raster)
#' # using an extent object
#' e <- extent(10,25,45,50) # xmin,xmax,ymin,ymax
#' suppressWarnings(get_tile_copernicus(extent = e))
#' # using (H,V) pairs
#' get_tile_copernicus(tileH = 19:20, tileV = 4:5)
#' @export
get_tile_copernicus <- function(extent, tileH, tileV, ...) {
    if ((missing(tileH) | missing(tileV)) & !missing(extent)) {

        # if Raster* object or Spatial*
        if (inherits(extent, "Raster") | inherits(extent, "SpatialPolygons") | inherits(extent,
            "Extent")) {
            if (inherits(extent, "Extent")) {
                # if min/max is inverted
                Txmax <- max(extent@xmin, extent@xmax)
                Txmin <- min(extent@xmin, extent@xmax)
                Tymax <- max(extent@ymin, extent@ymax)
                Tymin <- min(extent@ymin, extent@ymax)

                extent@ymin <- Tymin
                extent@ymax <- Tymax
                extent@xmin <- Txmin
                extent@xmax <- Txmax

                extent <- as(extent, "SpatialPolygons")  # convert extent to spatialPolygons
                warning("extent is of class 'Extent' and is supposed to be in geographical coordinates (LonLat)")

            } else {

                if (inherits(extent, "Raster")) {
                  if (is.na(projection(extent)))
                    stop("Provide a coordinate system to the 'extent' object")
                } else {
                  if (is.na(proj4string(extent)))
                    stop("Provide a coordinate system to the 'extent' object")
                }

                if (!isLonLat(extent)) {
                  if (inherits(extent, "Raster"))
                    extent <- projectExtent(extent, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +over") else extent <- spTransform(extent, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +over"))
                }
                # convert Raster extent to spatialPolygons
                if (inherits(extent, "Raster"))
                  extent <- as(extent(extent), "SpatialPolygons")
            }
        } else {
            stop("extent should be a Raster*, SpatialPolygons* or Extent object")
        }

        # COPERNICUS tile system, as a PolygonDataFrame
        tiles <- gen_tile_copernicus(poly = T, ...)
        if (is.na(proj4string(extent)))
            proj4string(extent) <- proj4string(tiles)

        result <- over(extent, tiles, returnList = T)[[1]]
    } else {

        if (missing(tileV))
            stop("Provide a numeric or character vector for 'tileV'")

        if (missing(tileH))
            stop("Provide a numeric or character vector for 'tileH'")

        if (length(tileH) != length(tileV))
            stop("tileH should have the same length as tileV")

        # COPERNICUS tile system, as a data.frame
        tiles <- gen_tile_copernicus(...)
        result <- merge(data.frame(h = tileH, v = tileV), tiles, by = c("h", "v"))
    }
    return(result)
}
