#' @title Convert bit values from a QFLAG COPERNICUS product
#' @description Convert a Raster* object representing QFLAG bit values to a Raster* with two categories representing flagged and non-flagged pixels
#' @usage
#' convert_qf_copernicus(r,qf,filename=rasterTmpFile(),...)
#' @param r A \code{\link[raster]{Raster-class}} object
#' @param qf Quality Flag to be extracted. Can be more than one of these:
#' 'sea','snow','suspect','aero_status_mixed','aero_source_climato','input_invalid','lai_invalid','fapar_invalid','fcover_invalid','b2_saturation','b3_saturation','filtered','gap_filled','ndvi_invalid'
#' @param filename Character vector giving the name of the output raster to save. 
#'        Default is created through \code{\link[raster]{rasterTmpFile}}
#' @param ... arguments passed to \code{\link[raster]{writeRaster}}
#' @return A \code{\link[raster]{Raster-class}} object with two values (1,0), representing respectively pixels that are flagged by at least one of the give \code{qf},
#'        and pixels that are not flagged in any of the given \code{qf}
#' @references
#' Baret et al. 2010. BioPar Product User Manual. Geoland2: Towards an Operational GMES Land Monitoring Core Service, p. 27
#' @author Antoine Stevens
#' @examples
#' \dontrun{
#' # Don't forget to provide in copernicus_options() your user and password details
#' # for COPERNICUS data portal before running this
#' # First, get data: NDVI_V1, for JAN 2009
#' fn <- get_copernicus(product = 'NDVI_V1', begin = '2009-01-01', end = '2009-31-01',
#'                      tileH = 19, tileV = 4)
#' fn # downloaded file names
#' # extract layers 3 (QFLAG) of the downladed file. Store images in the 'H19V4' folder
#' f <- extract_copernicus(fnames = fn,job = 'H19V4',layers = 3)
#' # f is the h5 file name(s)
#' # for each layer, the function append its name to the saved file name
#' # so we can change the name of the file(s) accordingly
#' f <- sub('\\.h5','_NDVI-QFLAG.tif',f)
#' # Create a rasterStack with QF bit values
#' library(raster)
#' QF <- stack(f)
#' # Find pixels flagged as being covered by snow
#' snow <- convert_qf_copernicus(QF,'snow')
#' plot(snow)
#' # Find pixels flagged as 'sea'
#' sea <- convert_qf_copernicus(QF,'sea')
#' plot(sea)
#' # flags can be combined
#' # Find pixels flagged as being suspect and with aerosol status mixed
#' bad <- convert_qf_copernicus(QF,c('suspect','aero_status_mixed'))
#' plot(bad)
#' }
#' @export
convert_qf_copernicus <- function(r, qf, filename=rasterTmpFile(),...) {

    if (!inherits(r, "Raster"))
      stop("r should be a Raster* object")

    q <- stringr::str_detect(paste(qf, collapse = "|"), c("sea", "snow", "suspect", "aero_status_mixed",
        "aero_source_climato", "input_invalid", "lai_invalid", "fapar_invalid", "fcover_invalid",
        "b2_saturation", "b3_saturation", "filtered", "gap_filled", "ndvi_invalid"))
    if (!sum(q))
        stop("qf should match at least one (or possibly more) of these : c('sea','snow','suspect','aero_status_mixed','aero_source_climato','input_invalid','lai_invalid','fapar_invalid','fcover_invalid','b2_saturation','b3_saturation','filtered','gap_filled','ndvi_invalid')")
    
    b <- list()
    b[[1]] <- brick(r,nl=nlayers(r), values=FALSE)
    b[[1]] <- writeStart(b[[1]], filename = filename,...)
    
    tr <- blockSize(x)
    
    for ( i in seq_along(tr$row))
      b[[1]] <- writeValues(b[[1]], .convert_qf(i = i, row = tr$row, nrows = tr$nrow,qf = q), tr$row[i])
    
    for (a in seq_along(b))
      b[[a]] <- writeStop(b[[a]])
    
    b <- brick(filename)
    b
}

.convert_qf <- function(i,row,nrows,qf){
  
  # Bit 1: Land/Sea Land Sea Bit 2: Snow status Clear Snow Bit 3: Suspect No suspect Suspect
  # Bit 4: Aerosol status Pure Mixed Bit 5: Aerosol source Modis Climato Bit 6: Input status
  # OK Out of range or invalid Bit 7: LAI status OK Out of range or invalid Bit 8: FAPAR
  # status OK Out of range or invalid Bit 9: FCover status OK Out of range or invalid Bit 10:
  # B2 saturation status OK Saturated Bit 11: B3 saturation status OK Saturated Bit 12:
  # Filtering status Not filtered Filtered Bit 13: Gap filling status Not filled Filled Bit
  # 14: NDVI status Ok invalid
  val  <-  raster::getValues(r, row=row[i], nrows=nrows[i])
  lev <- sort(unique(as.vector(val)))
  
  # there is 65535 possible values for Uint16
  bits <- sapply(lev, function(x) as.integer(intToBits(x)[1:14]))
  rownames(bits) <- c("sea", "snow", "suspect", "aero_status_mixed", "aero_source_climato",
                      "input_invalid", "lai_invalid", "fapar_invalid", "fcover_invalid", "b2_saturation",
                      "b3_saturation", "filtered", "gap_filled", "ndvi_invalid")
  colnames(bits) <- lev
  # bits <- bits[,-ncol(bits)] # remove the last column corresponding to 65535
  ids <- lev[as.logical(colSums(bits[qf, , drop = F]))]
  nas <- which(val == 65535 ) # replace by NA
  ids <- val %in% ids
  val[ids] <- 1
  val[!ids] <- 0
  if(length(nas))
    val[nas] <- NA
  val
}
