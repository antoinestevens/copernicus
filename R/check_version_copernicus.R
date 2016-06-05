#' @title Check algorithm version for a given COPERNICUS product
#' @description Retrieve version of the algorithm before and after May/June 2014
#' @usage
#' check_version_copernicus(product,server,user,password)
#' @param product one of the following: 'NDVI_1km_V1' (Normalized Difference Vegetation Index - VGT instrument),'NDVI_1km_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas)
#' @param server url of the COPERNICUS data server. Default to 'http://land.copernicus.vgt.vito.be/PDF///datapool/Vegetation/', see 'copernicus_options('server')'
#' @param user user name to access COPERNICUS data portal. Default set via \code{copernicus_options('user')}
#' @param password password associated with user name to access COPERNICUS data portal. Default set via \code{copernicus_options('password')}
#' @return
#' a \code{character} vector of algorithm version
#' @details Versions of the processing algorithm may be subject to change.
#' This function fetches the server data portal to retrieve up-to-date algorithm versions for each product,
#' taking into account changes in product version after May/June 2014
#' @author Antoine Stevens
#' @examples
#' \dontrun{
#' # user and password should be provided, for instance through global package options
#' # ?copernicus_options
#' # NDVI_1km_V1: there is only one version of the algorithm
#' check_version_copernicus('NDVI_1km_V1')
#' #  LAI
#' check_version_copernicus('LAI')
#' }
#' @export
check_version_copernicus <- function(product = c("NDVI_1km_V1", "NDVI_1km_V2", "LAI", "FCOVER", "FAPAR",
    "VCI", "VPI", "DMP", "BA"), server = copernicus_options("server"), user = copernicus_options("user"),
    password = copernicus_options("password")) {

    if (copernicus_options("user") == "" | copernicus_options("password") == "")
        stop("Set a user and password via 'copernicus_options()' to access COPERNICUS data portal")

    product <- match.arg(product)
    collection <- c("Dynamics/LAI_V1/", "Dynamics/FCOVER_V1/", "Dynamics/FAPAR_V1/", "Indices/NDVI_1km_V1/",
        "Indices/NDVI_1km_V2/", "Indices/VCI_V1/", "Indices/VPI_V1/", "BIOMASS/DMP_V1/", "Fire_Disturbance/BA_V1/")

    auth <- httr::authenticate(user = user, password = password)
    httr::set_config(auth)

    url <- paste0(server, stringr::str_subset(collection, product))

    if (product %in% c("NDVI_1km_V1", "LAI", "FCOVER", "FAPAR"))
        d <- c("2014/01/06", "2014/12/06") else d <- c("2014/01/01", "2014/12/01")

    url <- paste0(url, d, "/?cov=tile")
    # get the list of files
    h1 <- rvest::html(url[1]) %>% rvest::html_nodes("tr~ tr+ tr a") %>% rvest::html_attr("href") %>% rvest::html() %>%
          rvest::html_nodes("a") %>% rvest::html_attr("href") %>% stringr::str_subset("zip$") %>% magrittr::extract(1)

    if (product != "NDVI_1km_V1") {
        # get the list of files
        h2 <- rvest::html(url[2]) %>% rvest::html_nodes("tr~ tr+ tr a") %>% rvest::html_attr("href") %>% rvest::html() %>%
              rvest::html_nodes("a") %>% rvest::html_attr("href") %>% stringr::str_subset("zip$") %>% magrittr::extract(1)

        version <- c(scan_file_copernicus(h1)[, "version"], scan_file_copernicus(h2)[, "version"])
    } else {
        version <- scan_file_copernicus(h1)[, "version"]
    }
    httr::reset_config()
    return(version)
}
