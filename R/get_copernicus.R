#' @title Download COPERNICUS products
#' @description
#' Download COPERNICUS products, for a given period of time and tile(s)
#' @usage
#' get_copernicus(product,begin,end,tileH,tileV,outPath,user,password)
#' @param product One of the following: 'NDVI_V1' (Normalized Difference Vegetation Index - VGT instrument),'NDVI_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas)
#' @param begin begin of the time period. \code{Date} object, \code{numeric} or \code{character} of length 1 that can be transformed to a \code{Date} using \code{\link[lubridate]{ymd}}. See \code{?ymd} for more details. \code{ymj} format is also accepted (with \code{j} being the day of the year)
#' @param end end of the time period. Same format as \code{begin}
#' @param tileH H index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param tileV V index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param outPath Path where downloaded files should be stored. Default set via \code{copernicus_options('downloadPath')}
#' @param user user name to access COPERNICUS data portal. Default set via \code{copernicus_options('user')}
#' @param password password associated with user name to access COPERNICUS data portal. Default set via \code{copernicus_options('password')}
#' @details If target files are already present in the \code{outPath} directory, they are not downloaded
#' @return Return invisibly the list of downloaded files
#' @author Antoine Stevens
#' @examples
#' \dontrun{
#' # Don't forget to provide in copernicus_options() your user and password
#' # for COPERNICUS data portal before running this
#' # this will download a lot of data!
#' get_copernicus(product = 'NDVI_V1', begin = '2009-06-01', end = '2014-05-31', tileH = 19, tileV = 4)
#' }
#'
#' @export
get_copernicus <- function(product = c("NDVI_V1", "NDVI_V2", "LAI", "FCOVER", "FAPAR", "VCI",
    "VPI", "DMP", "BA"), begin, end, tileH, tileV, outPath = copernicus_options("downloadPath"),
    user = copernicus_options("user"), password = copernicus_options("password")) {

    if (copernicus_options("user") == "" | copernicus_options("password") == "")
        stop("Set a user and password via 'copernicus_options()' to access COPERNICUS data portal")

    if (missing(tileH))
        stop("Please provide a tileH argument")
    if (missing(tileV))
        stop("Please provide a tileV argument")
    if (missing(begin))
        stop("Provide a begin date. Eg '20140101' or '2014001' or an object of class 'Date'")
    if (missing(end))
        stop("Provide an end date. Eg '20140101' or '2014001' or an object of class 'Date'")

    if (length(tileH) != length(tileV))
        stop("tileH should have the same length as tileV")

    # check the availability of tile
    ch_tile <- check_tile_copernicus(tileH, tileV)

    if (!any(ch_tile))
        stop("Tile(s) is/are not available")

    if (sum(!ch_tile)) {
        warning(paste0("Tile(s): ", paste0("H", tileH[!ch_tile], "V", tileV[!ch_tile], collapse = " "),
            " is/are not available"))
        tileH <- tileH[ch_tile]
        tileV <- tileV[ch_tile]
    }

    # check the availability of the provided time period
    ch_time <- check_time_copernicus(product, begin, end)

    # get file url's
    urls <- get_url_copernicus(product, ch_time$begin, ch_time$end, tileH, tileV)
    h <- curl::new_handle()
    curl::handle_setopt(h, username = user, password = password)

    # create dir if necessary
    if (!dir.exists(outPath))
        dir.create(outPath, showWarnings = FALSE, recursive = TRUE)

    destfiles <- stringr::str_match(urls, ".+/(.+\\.zip$)")[, 2]

    id <- !destfiles %in% list.files(outPath)  # download only those that are not in the output directory

    print(paste0(sum(id), " files will be downloaded!"))

    urls <- urls[id]
    destfiles = destfiles[id]

    if (length(urls)) {
        foreach(i = 1:length(urls)) %do% {
            print(paste("Downloading:", destfiles[i]))
            # download data
            curl::curl_download(url = urls[i], dest = paste0(outPath, "/", destfiles[i]), handle = h)
        }
    }
    return(invisible(destfiles))
}
