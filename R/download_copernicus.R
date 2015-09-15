#' @title Download COPERNICUS products
#' @description
#' Download COPERNICUS products, for a given period of time and tile(s)
#' @usage
#' download_copernicus(product,begin,end,extent,tileH,tileV,outPath,user,password,...)
#' @param product One of the following: 'NDVI_V1' (Normalized Difference Vegetation Index - VGT instrument),'NDVI_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas)
#' @param begin begin of the time period. \code{Date} object, \code{numeric} or \code{character} of length 1 that can be transformed to a \code{Date} using \code{\link[lubridate]{ymd}}. See \code{?ymd} for more details. \code{ymj} format is also accepted (with \code{j} being the day of the year)
#' @param end end of the time period. Same format as \code{begin}
#' @param extent an object of class \code{Raster*}, \code{Extent} or \code{SpatialPolygons*} giving the extent from which tiles should be downloaded.
#' See \code{\link{get_tile_copernicus}} for details.
#' @param tileH H index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param tileV V index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param outPath Path where downloaded files should be stored. Default set via \code{copernicus_options('downloadPath')}
#' @param user user name to access COPERNICUS data portal. Default set via \code{copernicus_options('user')}
#' @param password password associated with user name to access COPERNICUS data portal. Default set via \code{copernicus_options('password')}
#' @param ... argument passed to \code{\link{get_url_copernicus}}, such as groupByDate
#' @details If target files are already present in the \code{outPath} directory, they are not downloaded.
#' One should choose to use either an extent or pairs of (H,V) values. If extent is provided, tileH and tileV are not used.
#' @return Return invisibly the list of downloaded files
#' @author Antoine Stevens
#' @examples
#' \dontrun{
#' # Don't forget to provide in copernicus_options() your user and password
#' # for COPERNICUS data portal before running this
#' # Let's download NDVI V1 prodct, for JUN 2009, and tile (h=19,v=4)
#' download_copernicus(product = 'NDVI_V1', begin = '2009-06-01', end = '2009-06-31',
#'                    tileH = 19, tileV = 4)
#' # one could also use an extent object, instead of (tileH,tileV) pairs, eg:
#' library(raster)
#' e <- extent(c(-1,2,49,51))
#' # this will download 12 files (4 tiles x 3 time periods)
#' download_copernicus(product = 'NDVI_V1', begin = '2009-06-01', end = '2009-06-31', extent = e)
#' }
#'
#' @export
download_copernicus <- function(product = c("NDVI_V1", "NDVI_V2", "LAI", "FCOVER", "FAPAR", "VCI",
    "VPI", "DMP", "BA"), begin, end, extent, tileH, tileV, outPath = copernicus_options("downloadPath"),
    user = copernicus_options("user"), password = copernicus_options("password"),...) {

    if (copernicus_options("user") == "" | copernicus_options("password") == "")
        stop("Set a user and password via 'copernicus_options()' to access COPERNICUS data portal")
    if(missing(extent)){
      if (missing(tileH))
          stop("Please provide a tileH argument")
      if (missing(tileV))
          stop("Please provide a tileV argument")
    } else {
      tileH = NULL;tileV = NULL
    }

    if (missing(begin))
        stop("Provide a begin date. Eg '20140101' or '2014001' or an object of class 'Date'")
    if (missing(end))
        stop("Provide an end date. Eg '20140101' or '2014001' or an object of class 'Date'")

    if (length(tileH) != length(tileV))
        stop("tileH should have the same length as tileV")

    # check the availability tiles
    if(!missing(extent))
      tiles <- get_tile_copernicus(extent = extent,exclude = FALSE) else tiles <- get_tile_copernicus(tileH = tileH, tileV = tileV, exclude = FALSE)

    if (all(!tiles$availability))
        stop("No Tile(s) is/are available")

    if (sum(!tiles$availability))
        warning(paste0("Tile(s): ", paste0("H", tiles$h[!tiles$availability], "V", tiles$v[!tiles$availability], collapse = " "),
            " is/are not available"))

    # keep only those that are available
    tiles <- tiles[tiles$availability,]

    # check the availability of the provided time period
    ch_time <- check_time_copernicus(product = product, begin = begin, end = end)

    # get file url's
    urls <- get_url_copernicus(product, ch_time$begin, ch_time$end, tileH = tiles$h, tileV = tiles$v, ...)

    # Set password and user name
    h <- curl::new_handle()
    curl::handle_setopt(h, username = user, password = password)

    # create dir if necessary
    outPath <- normalizePath(outPath)
    if (!dir.exists(outPath))
        dir.create(outPath, showWarnings = FALSE, recursive = TRUE)
    if(is.list(urls))
      destfiles <- lapply(urls, basename)
    else
      destfiles <- basename(urls)

    u <- unlist(urls)
    d <- unlist(destfiles)

    id <- !d %in% list.files(outPath)  # download only those that are not in the output directory

    print(paste0(sum(id), " files will be downloaded!"))

    u <- u[id]

    if (length(u)) {
        foreach(i = 1:length(u))%do%{
            print(paste("Downloading:", d[id][i]))
            # download data
            curl::curl_download(url = u[i], dest = paste0(outPath, "/", d[id][i]), handle = h)
        }
    }

    if(is.list(urls))
      return(invisible(lapply(destfiles,function(x)paste0(outPath,"/",x))))
    else
      return(invisible(paste0(outPath,"/",destfiles)))
}
