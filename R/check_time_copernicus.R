#' @title
#' Begin and end date of COPERNICUS products
#' @description
#' Check whether a COPERNICUS product is availabe within a given time period
#' @usage
#' check_time_copernicus(product,begin,end)
#' @param product one of the following: 'NDVI_V1' (Normalized Difference Vegetation Index - VGT instrument),'NDVI_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas)
#' @param begin begin of the time period. \code{Date} object, \code{numeric} or \code{character} of length 1 that can be transformed to a \code{Date} using \code{\link[lubridate]{ymd}}. See \code{?lubridate::ymd} for more details. \code{ymj} format is also accepted (with \code{j} being the day of the year)
#' @param end end of the time period. Same format as \code{begin}
#' @return a \code{list} with components \code{begin} and \code{end} strip to the available time period
#' @author Antoine Stevens
#' @examples
#' # Check data availability for NDVI V1, between 01-01-2010 and 31-12-2014
#' check_time_copernicus('NDVI_V1',20100101,20141231)
#' # Check data availability for NDVI V2, between 01-01-2010 and 31-12-2014
#' # using different date formats
#' check_time_copernicus('NDVI_V2','2010001','2014365')
#' check_time_copernicus('NDVI_V2','2010-01-01','2014-12-31')
#'
#' @export
check_time_copernicus <- function(product = c("NDVI_V1", "NDVI_V2", "LAI", "FCOVER", "FAPAR",
    "VCI", "VPI", "DMP", "BA"), begin, end) {

    product <- match.arg(product)
    if (length(begin) != 1 | length(end) != 1)
        stop("'begin' and 'end' should be of length 1")

    if (!lubridate::is.Date(begin))
        begin <- as.Date(lubridate::parse_date_time(begin, orders = c("ymd", "yj")))

    if (!lubridate::is.Date(end))
        end <- as.Date(lubridate::parse_date_time(end, orders = c("ymd", "yj")))

    b <- c(NDVI_V1 = "12/1998", NDVI_V2 = "01/2013", LAI = "12/1998", FCOVER = "12/1998", FAPAR = "12/1998",
        VCI = "01/2013", VPI = "01/2013", DMP = "01/2013", BA = "04/1999")  # begin dates for each product
    e <- c(NDVI_V1 = "04/2014", NDVI_V2 = "08/2015", LAI = "07/2015", FCOVER = "07/2015", FAPAR = "07/2015",
        VCI = "08/2015", VPI = "08/2015", DMP = "01/2013", BA = "08/2015")  # end dates for each product

    b <- b[product]
    e <- e[product]

    b <- as.Date(lubridate::parse_date_time(b, "%m/%y"))
    e <- as.Date(lubridate::parse_date_time(e, "%m/%y"))

    if (begin < b) {
        begin <- b
        warning(paste0("begin date is before than the older available date for the product. It is changed to: ",
            begin))
    }

    if (end > e) {
        end <- e
        warning(paste0("end date is after than the latest available date for the product. It is changed to: ",
            end))
    }
    list(begin = begin, end = end)
}
