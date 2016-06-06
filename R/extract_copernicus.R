#' @title Extract and process COPERNICUS h5 files
#' @description Un-zip, extract the given layers of COPERNICUS h5 files to tif files. The function can also crop, project and apply gain and offset values
#' @usage
#' extract_copernicus(fnames,extent,extend,convertDN = TRUE,outProj,pixelSize,resamplingType,
#'                    outPath,job,gdalPath,zip = TRUE,layers = 1, allowParallel = FALSE,...)
#' @param fnames \code{character} vector of the file names to transform (with a zip or h5 extension).
#' If \code{fnames} is a list of vector of file names, then files are automatically mosaicked for each element of the list
#' @param extent A \code{\link[raster]{Raster-class}}, \code{\link[raster]{Extent}} or \code{\link[sp]{SpatialPolygons-class}} giving the subwindow to crop the image to.
#'        If this is an \code{\link[raster]{Extent}} object, it is assumed to be in geographical coordinates.
#'        Else, the projection info of the object is used to project the image to the same projection.
#' @param extend numeric vector of length 1 or 2 (nrow,ncol). Extend the spatial extent of the subwindows (extent argument) by a given number of (rows,cols)
#' @param convertDN logical value indicating whether DN values should be converted to physical values (eg vegetation index). Default to TRUE
#' @param outProj \code{character} string giving the coordinate projection system of the output in the PROJ.4 format. Can also be a \code{\link[sp]{CRS-class}} object.
#'        Default is '+init=epsg:32662' (Plate Carree, WGS84), i.e. no re-projection is done.
#'        See \code{copernicus_options('outProj')} to change default value.
#'        if the \code{extent} argument is set, with an object of class \code{\link[raster]{Raster-class}} or \code{\link[sp]{SpatialPolygons-class}}, their projection info is used instead, unless their projection is 'NA'.
#' @param pixelSize output pixel size (c(xres,yres)). Default is 'asIn' (same as the input image). Can be set via \code{copernicus_options('pixelSize')}. If a \code{\link[raster]{Raster-class}} object is provided as \code{extent} argument, the resolution of the object override this argument.
#' @param resamplingType Resampling method in case of reprojection or change in pixel resolution. Should be one of the following: 'near','bilinear','cubic','cubicspline'
#' @param outPath Path where processed files should be stored. Default set via \code{copernicus_options('outPath')}
#' @param job Path to append to \code{outPath}
#' @param gdalPath Path to the gdal binaries. Default set via \code{copernicus_options('gdalPath')}
#' @param zip logical value indicating whether file(s) should be un-zipped before h5 extraction
#' @param layers \code{numeric} vector indicating the index/indices of the layer(s) to extract from the h5 file(s). Default set to 1
#'        See eg http://land.copernicus.eu/global/products/ndvi for more details.
#' @param allowParallel Logical. If a \code{foreach} parallel backend is loaded and available, should the function use it? Default is \code{FALSE}.
#' @param ... arguments passed to \code{\link[raster]{writeRaster}}
#' @return Return invisibly the list of extracted h5 files. If files are mosaicked, tiles numbers are dropped from the names
#' @author Antoine Stevens
#' @examples
#' \dontrun{
#' # Don't forget to provide in copernicus_options() your user and password details
#' # for COPERNICUS data portal before running this
#' # e.g. : copernicus_options(user = "Smith", password = "hello")
#' # First, get data: NDVI_1km_V1, for JAN 2009
#' fn <- download_copernicus(product = 'NDVI_1km_V1', begin = '2009-01-01', end = '2009-01-31',
#'                      tileH = 19, tileV = 4)
#' fn # downloaded file names
#' # extract layers 1,2,3 of the downladed file. Store images in the 'H19V4' folder
#' extract_copernicus(fnames = fn,job = 'H19V4',layers = c(1:3))
#' # Now, crop the tile to a given extent (extent argument),
#' # and add 1 row and col at each side of the data (extend argument)
#' # This is useful to add a buffer zone, in case of spatial smoothing
#' # tile location is:
#' get_tile_copernicus(tileH = 19, tileV = 4)
#' # create extent object
#' library(raster)
#' # extent corresponding to the Delta of the Po
#' e <- extent(c(10.5,12.5,44,46)) # xmin,xmax,ymin,ymax
#' f <- extract_copernicus(fnames = fn,extent = e,job = 'H19V4',extend = 1, layers = 2)
#' f # name of the h5 file(s)
#' # for each layer, the function append its name to the saved file name
#' # so we can change the name of the file(s) accordingly
#' f <- sub('\\.h5','_NDVI.tif',f)
#' # this is the resulting raster stack
#' s <- stack(f);s
#' plot(s)
#' # extent argument could also be a Raster* object,
#' # its projection, and resolution are then used to crop/project
#' # first let's create a raster object using the extent object
#' r <- raster(e)
#' projection(r) <- '+init=epsg:4326' # LatLon WGS84
#' # project to UTM 32N
#' r <- projectExtent(r,CRS('+init=epsg:32632'))
#' res(r) <- 1000 # set resolution to 1 km
#' # now extract COPERNICUS data and match these topology
#' f <- extract_copernicus(fnames = fn,extent = r,job = 'H19V4',extend = 1, layers = 2)
#' s <- stack(sub('\\.h5','_NDVI.tif',f));s
#' plot(s)
#' # This works with a SpatialPolygons* object too
#' # resolution can be set through pixelSize (in target geometry)
#' sPol <- as(e,'SpatialPolygons')
#' proj4string(sPol) <- CRS('+init=epsg:4326')
#' sPol <- spTransform(sPol,CRS('+init=epsg:32632')) # project to UTM 32N
#' f <- extract_copernicus(fnames = fn,extent = sPol,pixelSize = c(2000,2000),
#'                         job = 'H19V4',extend = 1, layers = 2)
#' s <- stack(sub('\\.h5','_NDVI.tif',f));s
#' plot(s)
#' # One can also simply reproject using the outProj argument
#' # and change resolution with the pixelSize argument
#' f <- extract_copernicus(fnames = fn,outProj = '+init=epsg:32632', pixelSize = c(1000,1000),
#'                         job = 'H19V4',layers = 2)
#' s <- stack(sub('\\.h5','_NDVI.tif',f));s
#' plot(s)
#' # extract_copernicus allows also to mosaic files, by grouping file names within a list
#' # first, download data spanning neighbouring tiles
#' e <- extent(c(-1,1,49,51)) # the English Channel
#' # the groupByDate argument allows to return a list of files, grouped by date
#' fn <- download_copernicus(product, begin = '2009-01-01', end = '2009-01-31',
#'                          extent = e,groupByDate = TRUE)
#' # now extract
#' f <- extract_copernicus(fnames = fn,extent = e, layers = 2)
#' f <- sub('\\.h5','_NDVI.tif',f)
#' s <-stack(f);s
#' plot(s,1)
#' # Let's add the COPERNICUS tiling system, projected to Plate Carree
#' sPol <- spTransform(gen_tile_copernicus(poly = T),CRS("+init=epsg:32662"))
#' plot(sPol,add=T)
#' # mosaicking works also with projection
#' f <- extract_copernicus(fnames = fn,extent = e, outProj = '+init=epsg:32632', layers = 2,
#'                        resamplingType = "bilinear")
#' f <- sub('\\.h5','_NDVI.tif',f)
#' s <-stack(f);s
#' plot(s)
#' }
#'
#' @export
extract_copernicus <- function(fnames, extent, extend, convertDN = TRUE, outProj = copernicus_options("outProj"),
    pixelSize = copernicus_options("pixelSize"), resamplingType = c("near", "bilinear", "cubic",
        "cubicspline"), outPath = copernicus_options("outPath"), job = "", gdalPath = copernicus_options("gdalPath"),
    zip = TRUE, layers = 1, allowParallel = FALSE, ...) {

    gdalUtils::gdal_setInstallation(search_path = gdalPath)  # set gdal path for gdalUtils

    if(!is.logical(allowParallel))
      stop("allowParallel should be a logical")

    if (missing(extent))
        extent <- NULL

    if (missing(extend))
      extend <- NULL

    if(class(outProj)=="CRS")
      outProj <- rgdal::CRSargs(outProj)

    if (!identical(CRS(outProj), CRS(copernicus_options("outProj"))))
        t_srs <- outProj else t_srs <- NULL

    if (pixelSize[1] != "asIn") {
        if (is.character(pixelSize) | (is.numeric(pixelSize) & length(pixelSize) != 2))
            stop("pixelSize should be either 'asIn' or a numeric vector of length 2")
        tr <- pixelSize
    } else {
        tr <- NULL
    }

    if (inherits(extent, "Raster")) {
        # extent is a raster, then reproject to the same coord system and given pixel size
        if (!is.na(projection(extent))) {
            t_srs <- projection(extent)
            warning("extent Raster projection will override outProj argument")
        }
        tr <- res(extent)  # resolution
        if (pixelSize[1] != "asIn")
            warning("resolution of extent Raster object will override pixelSize argument")
        resamplingType <- match.arg(resamplingType)
    } else if (inherits(extent, "SpatialPolygons")) {
        # extent is a SpatialPolygons*, then reproject to the same coord system
        if (!is.na(projection(extent))) {
            t_srs <- projection(extent)
            warning("extent SpatialPolygons projection will override argument")
        }
        resamplingType <- match.arg(resamplingType)
    } else if (class(extent)=="Extent"){
          extent <- raster(extent)
          projection(extent) <- "+init=epsg:4326"
    } else if (!is.null(extent)){
        stop("extent should be of class Raster*, SpatialPolygons* or Extent")
    }

    # append job folder to outPath
    if (job != "") {
        if (stringr::str_detect(outPath, "/$"))
            outPath <- paste0(outPath, job) else outPath <- paste0(outPath, "/", job)
    }
    outPath <- normalizePath(outPath,mustWork = FALSE)
    cat("Output Directory = ", outPath, "\n")

    if(allowParallel){
      `%mydo%` <- foreach::`%dopar%`
    } else {
      `%mydo%` <- foreach::`%do%`
    }

    # fnames could be a list with file names, grouped by years,
    # so we iterate over the list and for each element of the list, once again
    f_h5 <- foreach(fgroup = iterators::iter(fnames), .combine = c, .packages = c("iterators","gdalUtils","stringr","rgdal","rhdf5","raster"))%mydo%{

      # the layer loop is outside, to allow mosaiking
      # this is however not efficient, since the h5 is read several times
      foreach(layer = iterators::iter(layers),i = iterators::icount())%do%{

            src_dataset <- foreach(f = iterators::iter(fgroup),.combine = c)%do%{

                finfo <- scan_file_copernicus(f)
                if (zip) {
                    folder <- finfo[, "date"]
                    f_h5 <- extension(f, ".h5")
                    if(i==1)
                    utils::unzip(f, files = paste0(folder, "/", basename(f_h5)),
                          exdir = sub("/$|\\\\$", "", outPath),junkpaths = T)
                } else {
                    f_h5 <- f
                }

                f_h5 <- paste0(outPath, "/", basename(f_h5))

                h5info <- rhdf5::h5ls(f_h5, all = T)
                if(layer>nrow(h5info))
                  stop(paste0("Layer: ", layer, " does not exist in the h5 file"))

                ginfo <- gdalUtils::gdalinfo(f_h5)
                LAT <- stringr::str_subset(ginfo, "LAT") %>% stringr::str_replace(".+=", "") %>% as.numeric
                LONG <- stringr::str_subset(ginfo, "LONG") %>% stringr::str_replace(".+=", "") %>% as.numeric
                instrument <- stringr::str_subset(ginfo, "INSTRUMENT_ID") %>% stringr::str_replace(".+=", "")
                satellite <- stringr::str_subset(ginfo, "SATELLITE") %>% stringr::str_replace(".+=", "")
                # HV <- sub('.+=','',stringr::str_subset(ginfo,'REGION_NAME'))
                # d <- as.Date(ymd(sub('.+=','',stringr::str_subset(ginfo,'TEMPORAL_START'))))

                # bounding coordinates
                e_tile <- extent(c(LONG, LONG + 10, LAT - 10, LAT))
                # http://land.copernicus.eu/global/article/125sq-kilometers-burnt-south-australia 'Please
                # note that the coordinates may be given to you for pixel centre, whereas GDAL and QGIS
                # will expect them as pixel (outer) corners, so you may have to extend the coordinates with
                # 1/2 pixel size in all directions. '
                e_tile <- e_tile + (1/112)  # same as:  e_tile <- extend(e_tile,(1/112)/2)
                # project geographical coordinates in the plate carree system (no datum transformation)
                # the +over argument allows longitude output outside -180 to 180 range (disables wrapping)
                # (because one can have potentially -180 - (1/112)/2 )
                # https://trac.osgeo.org/proj/wiki/GenParm
                # e_tile_proj <- rgdal::project(as.matrix(e_tile), "+init=epsg:32662 +over") # does not work consistently
                e_tile_proj <- raster(e_tile)
                projection(e_tile_proj) <- "+init=epsg:4326"
                e_tile_proj <- as.matrix(extent(projectExtent(e_tile_proj,"+init=epsg:32662 +over")))

                if (!is.null(extent)) {

                    # project to geographical coordinates if necessary
                    if (!is.null(t_srs)){
                      if(!isLonLat(CRS(t_srs)))
                        e <- extent(projectExtent(extent, "+init=epsg:4326"))
                      else
                      e <- extent(extent)
                    } else {
                      e <- extent(extent)
                    }
                    # now, add row/col (or pix) at each side if requested by the user
                    if (!is.null(extend)) {

                        if (length(extend) == 1)
                          extend <- rep(extend, 2)

                        e <- extend(e, extend/112)  # add extend*(1/112) at each side (row/col)
                    }
                    # create tile raster
                    tile <- raster(e_tile,resolution = 1/112)

                    # crop the extent to the extent of the tile
                    e <- extent(crop(tile,e))

                    # project the subwindow
                    # e_proj <- rgdal::project(as.matrix(e), "+init=epsg:32662 +over") # does not work consistently
                    e_proj <- raster(e)
                    projection(e_proj) <- "+init=epsg:4326"
                    e_proj <- as.matrix(extent(projectExtent(e_proj,"+init=epsg:32662 +over")))

                    # compute crop subwindow
                    # translate extent to the coord system of the tile (upper-left --> pixel centre)
                    srcwin <- round(c(e@xmin - LONG + (1/112/2), LAT  - e@ymax + (1/112/2), e@xmax - e@xmin, e@ymax - e@ymin) * 112)
                } else {
                    e_proj <- e_tile_proj
                    srcwin <- NULL
                }

                if(nrow(h5info) == 1){
                  sd_index <- NULL # remove sd_index when there is only one layer, else gdal_translate gives an error
                } else {
                  sd_index <- layer
                  h5info <- h5info[layer, ]  # extract the selected layers only
                }

                att <- rhdf5::h5readAttributes(f_h5, h5info$name)
                rhdf5::H5close()
                if(length(fgroup)>1){
                    cat(paste0("Extracting: ", basename(f_h5),"\n"))
                } else {
                  if(i==1)
                    cat(paste0("Extracting: ", basename(f_h5),"\n"))
                }
                cat(paste0("- ", h5info$name, "\n"))

                dst_dataset <- extension(rasterTmpFile(),"tif")
                # http://land.copernicus.eu/global/faq/how-convert-swi-hdf5-data-geotiff

                args_to_gdal <- list(src_dataset = f_h5, dst_dataset = dst_dataset, sd_index = sd_index,
                                     srcwin = srcwin, a_ullr = e_proj[c(1, 4, 3, 2)], a_srs = "+init=epsg:32662",
                                     a_nodate = as.numeric(att$MISSING_VALUE))

                args_to_gdal <- args_to_gdal[!sapply(args_to_gdal,is.null)] # remove unnecessary arguments

                # convert
                do.call(gdalUtils::gdal_translate,args_to_gdal)

                # cleaning
                if (zip&i==length(layers))
                  unlink(f_h5)

                dst_dataset
            }

            # change the name of f_h5 if mosaiking
            # removing the tile info
            if(length(src_dataset)>1){
              ftmp <- scan_file_copernicus(fgroup)[1,]
              f_h5 <- paste0(outPath,"/g2_BIOPAR_",ftmp$product,"_",ftmp$date,"0000_",ftmp$sensor,"_V",ftmp$version,".h5")
            }

            # Projection/ mosaicking
            if (!is.null(t_srs) | !is.null(tr)) {
              # projection
              args_to_gdal <- list(srcfile = src_dataset, dstfile = sub("\\.grd$", ".tif", rasterTmpFile()),
                                   s_srs = "+init=epsg:32662", t_srs = t_srs, tr = tr, r = resamplingType, output_Raster = T,
                                   overwrite = TRUE)
              args_to_gdal <- args_to_gdal[!sapply(args_to_gdal,is.null)] # remove unnecessary arguments

              r <- do.call(gdalUtils::gdalwarp,args_to_gdal)
            } else if (length(src_dataset)>1) {
              # mosaiking
              r <- gdalUtils::gdalwarp(srcfile = src_dataset, dstfile = sub("\\.grd$", ".tif", rasterTmpFile()),
                                       output_Raster = T,
                                       overwrite = TRUE)
            } else {
              r <- raster(src_dataset[[1]])
            }

            layer_name <- att$PRODUCT
            # set name
            names(r) <- layer_name
            # set projection
            projection(r) <- outProj
            if (layer_name %in% c("NDVI", "LAI", "FAPAR", "FCOVER", "VPI", "VCI", "DMP")) {
              #######################
              # Specific values:
              # NDVI V2, VPI, VCI, : 252 for cloud/shadow pixels, 253 for snow/ice,
              # 254 for sea pixels and 251/255 for invalids or data errors.  NDVI V1, LAI, FAPAR 255 for
              # invalid pixels, 253 for pixels with out of range value superior to the max physical
              # value, 254 for pixels with out of range value inferior to the minimum physical value.
              # DMP: -1 Background ; -2 Sea; -3 Snow/Ice; -4 Cloud; -5 Missing data; -257 NDVI < 0; -300
              # Missing Meteo data create a new layer with these values
              ######################
              r2 <- r

              if ((layer_name == "NDVI" & instrument == "VGT3") | layer_name %in% c("VPI","VCI")) {
                # remove values outside range
                r <- clamp(r, upper = 250, useValues = F)
                r2 <- clamp(r2, lower = 251, useValues = F)
                r2 <- r2 - 251  # convert to zero-base index
                r2_lev <- list(code = c("invalid", "cloud/shadow", "snow/ice", "sea", "invalid"))
              } else if (layer_name == "DMP") {
                r <- clamp(r, upper = 0, useValues = F)
                r2 <- clamp(r2, lower = 1, useValues = F)
                r2 <- subs(r2, data.frame(from = c(-300, -257, -5:-1), to = 0:6))
                r2_lev <- list(code = c("missing_meteo", "NDVI<0", "missing_data", "cloud",
                                        "snow/ice", "sea", "background"))
              } else {
                r <- clamp(r, upper = 250, useValues = F)
                r2 <- clamp(r2, lower = 251, useValues = F)
                r2 <- r2 - 253
                r2_lev <- list(code = c("out_max", "out_min", "invalid"))
              }
              names(r2) <- paste0(names(r), "_flag")
              # write to disk, with attribute table
              rgdal::writeGDAL(as(r2, "SpatialGridDataFrame"), fname = sub("\\.h5$", paste0("_",
                        layer_name, "_flag.tif"), f_h5), catNames = r2_lev, type = "Byte", mvFlag = 255)
            }

            # convert DN to biophysical values see GIO-GL1_PUM_NDV1V1_I1.00.pdf pg 26 and copernicus
            # website: eg: http://land.copernicus.eu/global/products/ndvi?qt-ndvi_characteristics=5 The
            # physical values (PV) can be derived from the digital number (DN) using the relation: PV =
            # Scaling * DN + Offset biophy_range <- data_frame(product =
            # c('LAI','FAPAR','FCOVER','NDVI_1km_V1','NDVI_1km_V2','VPI','VCI','DMP'), min =
            # c(0,0,0,-.1,-.08,0,-.125,0), max = c(7, .94, 1, .9,.92,100,1.125,327.67), maxDN =
            # c(210,235,250,250,250,210,250,32767), scaling =
            # c(1/30,1/250,1/250,1/250,1/250,1/2,0.005,0.01), offset = c(0,0,0,25,-.08,-5,0.125,0))
            if (convertDN)
              r <- (r - as.numeric(att$OFFSET))/as.numeric(att$SCALING_FACTOR)

            # write to disk
             writeRaster(r, filename = sub("\\.h5$", paste0("_", layer_name,".tif"), f_h5), ...)
        }
        f_h5
    }
    return(invisible(f_h5))
}
