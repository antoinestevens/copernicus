#' @title Get url's of COPERNICUS products
#' @description
#' Get COPERNICUS url's for a given product, tile, and period of time
#' @usage
#' get_url_copernicus(product,begin,end,tileH,tileV,groupByDate,server,check_version = FALSE)
#' @param product one of the following: 'NDVI_1km_V1' (Normalized Difference Vegetation Index - VGT instrument),'NDVI_1km_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas)
#' @param begin begin of the time serie. \code{Date} object, \code{numeric} or \code{character} of length 1 that can be transformed to a \code{Date} using \code{\link[lubridate]{ymd}}. See \code{?ymd} for more details. \code{ymj} format is also accepted (with \code{j} being the day of the year)
#' @param end end of the time serie. Same format as \code{begin}
#' @param tileH H index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param tileV V index of the tile COPERNICUS system (\code{numeric} or \code{character})
#' @param groupByDate logical value indicating whether to return a list of vector with url's grouped by date, or a vector of url's. Default is \code{FALSE}.
#' @param server url of the COPERNICUS data server. Default to 'http://land.copernicus.vgt.vito.be/PDF///datapool/Vegetation/', see 'copernicus_options('server')'
#' @param check_version logical value indicating whether the algorithm version should be checked on the server data portal. Default to FALSE. See \code{\link{check_version_copernicus}}
#' @return
#' a character \code{vector} of url('s)
#' @author Antoine Stevens
#' @examples
#' # URL's for LAI product, for JAN 2014 and tiles (h=19,v=3) and (h=20,v=4)
#' get_url_copernicus(product = 'LAI',begin = '20140101', end = '20140131',
#'                    tileH = 19:20, tileV = 3:4)
#' get_url_copernicus(product = 'NDVI_1km_V2',begin = '20130101', end = '20130131',
#'                    tileH = 19:20, tileV = 3:4)
#' @export
get_url_copernicus <- function(product = c("NDVI_1km_V1", "NDVI_1km_V2", "LAI", "FCOVER", "FAPAR",
    "VCI", "VPI", "DMP", "BA"), begin, end, tileH, tileV, groupByDate = FALSE, server = copernicus_options("server"),
    check_version = FALSE) {

    product <- match.arg(product)
    collection <- c("Properties/LAI_V1/", "Properties/FCOVER_V1/", "Properties/FAPAR_V1/", "Indicators/NDVI_1km_V1/",
                    "Indicators/NDVI_1km_V2/", "Indicators/VCI_V1/", "Indicators/VPI_V1/", "Biomass/DMP_V1/", "Fire_Disturbance/BA_V1/")

    url <- paste0(server, stringr::str_subset(collection, product))

    if (!lubridate::is.Date(begin)){
        begin <- as.Date(lubridate::parse_date_time(begin, orders = c("ymd", "ymj"), quiet = TRUE))
        if (is.na(begin))
          stop("'begin' is not a valid date")
    }

    if (!lubridate::is.Date(end)){
        end <- as.Date(lubridate::parse_date_time(end, orders = c("ymd", "ymj"), quiet = TRUE))
        if (is.na(end))
          stop("'end' is not a valid date")
    }

    be <- begin %--% end # time interval

    # add xx days at the end and remove xx at the beginning because of a time lag for certain
    # products
    if (product %in% c("NDVI_1km_V1", "LAI", "FCOVER", "FAPAR")) {
        begin <- begin - 18
        end <- end - 18
    } else if (product == "BA") {
        begin <- begin - 9
        end <- end - 9
    }
    be_month <- seq(lubridate::floor_date(begin, "month"), lubridate::floor_date(end, "month"), by = "month")  # sequence per month

    # get the name of the folders (folder date in each month) + real acquistion date files are
    # looking like this: g2_BIOPAR_<Acronym>_<YYYYMMDDHHMM>_<AREA>_<SENSOR>_V<Major.Minor>
    ym <- rep(be_month, each = 3)  # there is 3 dates per month, so replicate 3 times
    y <- lubridate::year(ym)
    m <- lubridate::month(ym)
    if (product %in% c("NDVI_1km_V1", "LAI", "FCOVER", "FAPAR")) {
        dm <- c(3, 13, 23)  # days of the months
        d <- list(`28` = dm, `29` = dm + 1, `30` = dm + 2, `31` = dm + 3)  # the sequence is different for months with 28,29,30,31 days...
        d <- unlist(d[as.character(lubridate::days_in_month(be_month))])  # computes the number of days in each month and assign the day sequences
        lubridate::mday(ym) <- d  # assign day of the month
        sensor <- ifelse(ym < as.Date(lubridate::ymd("20140501")), "VGT", "PROBAV")  # change in the sensor since May 2014
        if (check_version)
            v <- check_version_copernicus(product) else v <- c("1.3", "1.4")
        version <- ifelse(ym < as.Date(lubridate::ymd("20140501")), v[1], v[2])
        ym <- ym + 18  # the real acquisition date is actually 18 days later (not sure why yet!)
        a <- format(ym, "%Y%m%d0000")  # format acquisition date
    } else {
        d <- c(1, 11, 21)
        lubridate::mday(ym) <- d
        d <- day(ym)
        if (product == "BA") {
            sensor <- ifelse(ym < as.Date(lubridate::ymd("20140401")), "VGT", "PROBAV")  # change in the sensor since April 2014 for BA
            if (check_version)
                v <- check_version_copernicus(product) else v <- c("1.3", "1.5")
            version <- ifelse(ym < as.Date(lubridate::ymd("20140401")), v[1], v[2])
            ym <- ym + 9  # there is a 9 days lag for BA products
        } else {
            sensor <- ifelse(ym < as.Date(lubridate::ymd("20140601")), "VGT", "PROBAV")  # change in the sensor since Jan 2014
            if (check_version) {
                version <- check_version_copernicus(product)[1]
                version <- rep(version,length(ym))
            } else {
                if (product == "NDVI_1km_V2"){
                  version <- ifelse(ym < as.Date(lubridate::ymd("20140101")), "2.0", "2.1")
                } else{
                  version <- "1.0"
                  version <- rep(version,length(ym))
                }
            }
        }
        a <- format(ym, "%Y%m%d0000")  # acquisition date
    }
    # keep only those that are between begin and end
    id <- (ym %within% be)
    a <- a[id]
    sensor <- sensor[id]
    version <- version[id]
    y <- y[id]
    m <- m[id]
    d <- d[id]

    # create product name
    folder_name <- paste0(stringr::str_replace(product, "_V[12]", ""), "_", a, "_", sensor, "_V",
                          stringr::str_extract(version, "[12]"))
    product_name <- paste0("g2_BIOPAR_", stringr::str_replace(product, "_V[12]", ""), "_", a, "_", "H",
        rep(tileH, each = length(a)), "V", rep(tileV, each = length(a)), "_", sensor, "_V",
        version, ".zip")

    # get final urls
    urls <- paste(url, y, m, d, folder_name, product_name, sep = "/")
    if(groupByDate)
      urls <- split(urls,lubridate::ymd(paste(y,m,d)))

    return(urls)
}
