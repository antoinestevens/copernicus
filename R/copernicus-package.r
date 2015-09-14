#' @title  Access and extract COPERNICUS imagery
#'
#' @section Introduction:
#' The copernicus package allows to download data from the COPERNICUS data portal
#' through the fast HTTP access (the so called 'data pool': http://land.copernicus.vgt.vito.be/PDF/datapool/).
#' The user should be registered to access data.
#'
#' @section Download:
#' \code{get_copernicus} wil download zip files on the data portal based on an extent or given tiles (h,v) pairs and a product name.
#' Only the following products are available:  'NDVI_V1' (Normalized Difference Vegetation Index - VGT instrument),
#' 'NDVI_V2' (Normalized Difference Vegetation Index - PROBAV instrument),'LAI' (Leaf Area Index),'FCOVER' (Fraction of Vegetation Green Cover),
#' 'FAPAR' (Fraction of Absorbed Photosynthetically Active Radiation),'VCI' (Vegetation Condition Index),'VPI' (Vegetation Productivity Index),
#' 'DMP' (Dry Matter Productivity),'BA' (Burnt Areas).
#'
#' The availability of the data between two dates can be checked with \code{\link{check_time_copernicus}} and at a given location with \code{\link{check_tile_copernicus}}.
#' One can get the URL's of images for a given time period, location and product name with \code{\link{get_url_copernicus}}.
#' Information included in the name of the files (e.g. aglorithm version, instrument) can be retrieved with \code{\link{scan_file_copernicus}}.
#' The COPERNICUS tiling system can be created with \code{\link{gen_tile_copernicus}} and fetched with \code{\link{get_tile_copernicus}}
#' which will retrieve tiles info (h,v,bounding box in LatLon) based on an extent or (h,v) pair.
#'
#' @section Extracting:
#' File conversion can be done with \code{\link{extract_copernicus}}, which will transfrom raw h5 files to tif, by converting DN to physical values
#' using the associated gain and offset of each product. The function can further subset data on a given extent, resample to a given resolution
#' and project to a given coordinate system. One can also choose which layers of the h5 files can be extracted. This function requires to
#' install GDAL.
#' Quality Flags can be extracted with \code{\link{convert_qf_copernicus}}.
#'
#' @section Package options:
#' Some of the package default options are set with \code{\link{copernicus_options}}, such as the download path, user name and password.
#'
#' @note
#' Pieces of code are taken/inspired from the MODIS package (v0.10-31): https://r-forge.r-project.org/R/?group_id=1252),
#' Function arguments follow the same naming as in MODIS, as much as possible.
#' @name copernicus
#' @docType package
#' @import foreach raster sp rgeos
#' @importFrom lubridate %--% %within%
#' @importFrom magrittr %>%
NULL
