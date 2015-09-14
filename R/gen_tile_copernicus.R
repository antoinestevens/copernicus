#' @title Generate COPERNICUS tiling grid
#' @description Generates the COPERNICUS tiling system
#' @usage
#' gen_tile_copernicus(poly,offset,exclude = TRUE)
#' @param poly logical value indicating whether the function should return a . Default set to \code{FALSE}
#' @param offset numeric. Shift the tiling grid in the upper-left direction. Default to (1/112)/2.
#' The reference position is the centre of the pixel in COPERNICUS products, while the \code{raster} package, as well as other
#' GIS (eg QGIS) expect the reference of a grid to be at the upper-left corner. It means that the upper-left corner of the pixels is at  [(pixel_longitude - angular_resolution/2),(pixel_latitude + angular_resolution/2)]
#' @param exclude logical value indicating whether to return only tiles where COPERNICUS data are available or not. Default is \code{TRUE}
#' @details See also \code{\link[MODIS]{genTile}} in the MODIS package
#' @return a \code{data.frame} or \code{SpatialPolygonsDataFrame} with h,v pairs and bounding box. See \code{poly} parameter
#' @author Antoine Stevens
#' @details If \code{exclude} is \code{FALSE}, the logical variable \code{availability} is added to the returned object, indicating which tiles
#' have COPERNICUS data.
#' @references
#' Baret et al. 2010. BioPar Product User Manual. Geoland2: Towards an Operational GMES Land Monitoring Core Service, 42 p.
#' @examples
#' # Return a data.frame with tiling info
#' df <- gen_tile_copernicus()
#' head(df)
#' # Return a Spatial object
#' library(sp)
#' pol <- gen_tile_copernicus(poly = TRUE)
#' plot(pol)
#'
#' @export
gen_tile_copernicus <- function(poly = F, offset = (1/112)/2,exclude = TRUE) {

    if (!is.logical(poly) | length(poly) != 1)
        stop("poly should be a logical value of length 1")
    if (length(offset) != 1 | !is.numeric(offset)) {
        stop("offset should be a numeric value of length 1")
    }

    hv <- expand.grid(h = 0:35, v = 0:17)
    hv$xmin <- (hv$h * 10) - 180 - offset
    hv$xmax <- (hv$h * 10) - 170 - offset
    hv$ymin <- -(hv$v * 10) + 80 + offset
    hv$ymax <- -(hv$v * 10) + 90 + offset
    no_data <- rbind(expand.grid(0:35, c(0, 16, 17)), expand.grid(c(0:12, 14:35), 15), expand.grid(c(2:9,
        13, 15:34), 14), expand.grid(c(0:9, 12:31, 33), 13), expand.grid(c(0:8, 13:18, 22:28),
        12), expand.grid(c(0:3, 6, 8:9, 14, 16:18, 24:28), 11), expand.grid(c(5:9, 15, 16,
        18, 25, 26), 10), expand.grid(c(5:7, 15, 17, 24, 26), 9), expand.grid(c(0, 1, 3:7,
        14, 15, 24), 8), expand.grid(c(0, 1, 3:5, 13, 14, 24, 31, 33), 7), expand.grid(c(0,
        3:5, 11:15, 33:35), 6), expand.grid(c(0:4, 12:14, 33:35), 5), expand.grid(c(0:4, 13:16,
        34:35), 4), expand.grid(c(14, 15), 3), expand.grid(c(16, 18, 22), 1))
    colnames(no_data) <- c("h", "v")
    no_data$no <- T
    hv <- merge(hv, no_data, by = c("h", "v"), all.x = T)
    if(exclude)
      hv <- hv[is.na(hv$no), ]
    else
      hv$availability <- ifelse(is.na(hv$no),TRUE,FALSE)
    hv <- subset(hv, select = -no)
    if (poly) {
        pol <- list()
        for (i in 1:nrow(hv)) {
            coords <- matrix(c(hv$xmin[i], hv$ymin[i], hv$xmin[i], hv$ymax[i], hv$xmax[i],
                hv$ymax[i], hv$xmax[i], hv$ymin[i], hv$xmin[i], hv$ymin[i]), ncol = 2, byrow = T)

            pol[i] <- sp::Polygons(list(sp::Polygon(coords)), ID = i)
        }
        # the over argument is for lat values abobe 90 and long below - 180 (that might be due to
        # the offset)
        pol <- sp::SpatialPolygons(pol, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +over"))
        pol <- sp::SpatialPolygonsDataFrame(Sr = pol, data = hv, match.ID = F)
        return(pol)
    } else {
        return(hv)
    }
}
