#' @title Compare Temporal Consistency of two Rasters
#' @description Analysis of the temporal consistency (agreement) of two \code{\link[raster]{Raster-class}} objects with dimensions
#' x (lon), y (lat), z (time). The analysis is performed pixel by pixel. The temporal profile (z dimension) of each
#' pixel at a given (x,y) is extracted in the two raster and compared using a set of statistics. This results in a raster map for
#' each computed statistic, depicting the spatial variability of the temporal coherence (agreement) between the two datasets.
#' @usage compare_raster_time(x,y,stats,FUN = NULL,filename=rasterTmpFile(),cores,...)
#' @param x A \code{\link[raster]{Raster-class}} object. Better be a \code{RasterBrick}, for faster computing
#' @param y Another \code{\link[raster]{Raster-class}} object to compare with the first one
#' @param stats A character \code{vector} with one or more of the following: 'missing_x','missing_y', 'cor',
#' 'ax', 'ay', 'bx', 'by', 'ac','acu', 'acs', 'mbe', 'rmsd', 'rmspd',  'rmpdu', 'rmpds','mpdpu','mpdps'. See details
#' @param FUN An optional function to be applied on the \code{z} slot of the input \code{x} and \code{y} (can be set with
#' \code{\link[raster]{setZ}}, to group statistics according to the resulting classes. If provided, only one
#' \code{stats} is allowed. See Examples section.
#' @param cores number of cores used when computing statistics. Default is 1.
#' @param filename name of the file where the output raster should be saved. Default is created through \code{\link[raster]{rasterTmpFile}}
#' @param ... arguments passed to \code{\link[raster]{writeRaster}}
#' @details
#' The \code{stats} arguments can take one or more of the following values (see Ji and Gallo, 2006):
#' \itemize{
#'    \item{\code{missing_x}}{Number of missing values in the \code{x} raster}
#'    \item{\code{missing_y}}{Number of missing valuse in the \code{y} raster}
#'    \item{\code{cor}}{Correlation coefficient}
#'    \item{\code{ax}}{Intercept of the Geometric Mean Functional Relationship (GMFR) model x = a + by}
#'    \item{\code{ay}}{Intercept of the GMFR model y = a + bx}
#'    \item{\code{bx}}{Coefficient of the GMFR model x = a + by}
#'    \item{\code{by}}{Coefficient of the GMFR model y = a + bx}
#'    \item{\code{ac}}{Agreement Coefficient}
#'    \item{\code{acu}}{Unsystematic Agreement Coefficient}
#'    \item{\code{acs}}{Systematic Agreement Coefficient}
#'    \item{\code{mbe}}{Mean Bias Error}
#'    \item{\code{rmsd}}{Root Mean Square Deviation}
#'    \item{\code{rmspd}}{Root Mean Square Deviation in percentage of mean y}
#'    \item{\code{rmpdu}}{Unsystematic Square Root of Mean Product-difference }
#'    \item{\code{rmpds}}{Systematic Square Root of Mean Product-difference}
#'    \item{\code{mpdpu}}{Proportion of the unsystematic differences over the total difference}
#'    \item{\code{mpdps}}{Proportion of the systematic differences over the total difference}
#' }
#' @return A \code{\link[raster]{Raster-class}} object with layers correponding to \code{stats}. If \code{FUN} argument
#' is provided, layers correponds to the groups resulting from applying \code{FUN} to the \code{z} slot of input rasters \code{x}
#' and \code{y}.
#' @author Antoine Stevens
#' @references
#' Ji, L., and Gallo, K. (2006). An Agreement Coefficient for Image Comparison. Photogrammetric Engineering & Remote Sensing 72, 823-833.
#' Meroni, M., Atzberger, C., Vancutsem, C., Gobron, N., Baret, F., Lacaze, R., Eerens, H., and Leo, O. (2013). Evaluation of Agreement Between Space Remote Sensing SPOT-VEGETATION fAPAR Time Series. IEEE Transactions on Geoscience and Remote Sensing 51, 1951-1962.
#' Meroni M., Fasbender D., Balaghi et al. (2015). Testing VGT data continuity between SPOT and PROBA-V missions for operational yield forecasting in North African countries. JRC Technical Report, 28 p.
#' @examples
#' \dontrun{
#' # Let's compare VGT and PROBA-V instruments during their overlaping period
#' # (October 2013 - March 2014)
#' # Don't forget to provide in copernicus_options() your user and password details
#' # for COPERNICUS data portal before running this
#' # e.g. : copernicus_options(user = "Smith", password = "hello")
#' # First, get data for NDVI_V1
#' fn_SPOT <- download_copernicus(product = 'NDVI_V1', begin = '2013-10-01', end = '2014-03-31',
#'                      tileH = 19, tileV = 4)
#' # and NDVI_V2 ...
#' fn_PROBA <- download_copernicus(product = 'NDVI_V2', begin = '2013-10-01', end = '2014-03-31',
#'                      tileH = 19, tileV = 4)
#' # Extract NDVI, export to tif
#' f_SPOT <-extract_copernicus(fn_SPOT,job = "product_comparison",layers = 2)
#' f_PROBA <-extract_copernicus(fn_PROBA,job = "product_comparison",layers = 1)
#'
#' # Convert to  rasterBrick
#' f_SPOT <- sub('\\.h5','_NDVI.tif',f_SPOT)
#' f_PROBA <- sub('\\.h5','_NDVI.tif',f_PROBA)
#' SPOT  <- writeRaster(stack(f_SPOT),filename = rasterTmpFile())
#' PROBA  <- writeRaster(stack(f_PROBA),filename = rasterTmpFile())
#'
#' # Compare temporal consistency
#' # Use the 'cores' to speed up processing!
#' ct <- compare_raster_time(SPOT,PROBA)
#' # plot
#' brks <- c(0,.05,.1,.2,.3,.7)
#' nb <- length(brks)-1
#' cols <- rev(heat.colors(nb))
#' plot(ct,"rmsd",col = cols,breaks = brks) # rmsd
#' # focus on a small area
#' # extent corresponding to the Delta of the Po
#' e <- extent(c(10.5,12.5,44,46)) # xmin,xmax,ymin,ymax
#' # convert geo coordinates to EPSG 32662 (Platte Carree)
#' e <- extent(projectExtent(raster(e,crs = crs("+init=epsg:4326")),crs("+init=epsg:32662")))
#' plot(ct,"ac",ext = e,zlim=c(0,1))
#'
#' # Let's see if there are different patterns
#' # according to seasons
#' # First, define a z slot
#' d <- scan_file_copernicus(names(SPOT))$Date # get dates
#' SPOT <- setZ(SPOT,d)
#' PROBA <- setZ(PROBA,d)
#' # Compare, grouping by Quarter
#' # create a special function, appending 'Q' to the quarter number
#' quarter <- function(x) paste0("Q",lubridate::quarter(x))
#' quarter(d)
#' ct_Quarter <- compare_raster_time(SPOT,PROBA,stats = "cor",FUN = quarter)
#' plot(ct_Quarter)
#' }
#'
#' @export
compare_raster_time <- function(x,y,
                                stats = c("missing_x","missing_y", "cor", "ax", "ay", "bx", "by", "ac","acu", "acs", "mbe", "rmsd", "rmspd",  "rmpdu", "rmpds","mpdpu","mpdps"),
                                FUN = NULL,filename=rasterTmpFile(),cores,...){

  stats <- match.arg(stats,several.ok = TRUE)

  if(!inherits(x,"Raster"))
    x <- stack(x, quick=TRUE)

  if(!inherits(y,"Raster"))
    y <- stack(y, quick=TRUE)

  # compare dimensions
  if(!compareRaster(x,y))
    stop("x and y rasters have different geometries")
  if(nlayers(x) != nlayers(y))
    stop("x and y rasters have a different number of bands")

  if(!is.null(FUN)){
    z1 <- getZ(x)
    z2 <- getZ(y)
    if(is.null(z1)|is.null(z2))
      stop("Set z values via 'setZ'")
    if(!identical(z1,z2))
      stop("Z values are not identical")
    if(length(stats)>1){
      warning("More than one 'stats' is provided. Only the first one is kept")
      stats <- stats[1]
    }
    f <- FUN(z1)
    nl <- nlevels(factor(f))
    fn <- levels(factor(f))
  } else {
    f <- NULL
    nl <- length(stats)
    fn <- stats
  }
  b <- list()
  b[[1]] <- brick(x,nl=nl, values=FALSE)
  b[[1]] <- writeStart(b[[1]], filename = filename,...)
  if (missing(cores)){
    tr <- blockSize(x,n = nlayers(x)*2)
    for ( i in seq_along(tr$row))
      b[[1]] <- writeValues(b[[1]], .compare_time(i = i, x = x, y = y, row = tr$row, nrows = tr$nrow, stats = stats, f = f), tr$row[i])
  } else {

    beginCluster(cores)
    cl <- getCluster()
    on.exit(endCluster())

    # send expr and data to cluster nodes
    parallel::clusterEvalQ(cl,library(matrixStats))
    parallel::clusterExport(cl,".compare_xy")

    # number of blocks
    tr <- blockSize(x, minblocks=cores)

    for (i in 1:cores)
      parallel:::sendCall(cl[[i]],.compare_time,list(i = i, x = x, y = y, row = tr$row, nrows = tr$nrow, stats = stats, f = f),tag=i)

    for (i in 1:tr$n) {
      d <- parallel:::recvOneData(cl);
      if (!d$value$success)
        stop("Cluster error in Row: ", tr$row[d$value$tag],"\n")

      b[[1]] <- writeValues(b[[1]], d$value$value, tr$row[d$value$tag])
      ni <- cores + i
      if (ni <= tr$n)
        parallel:::sendCall(cl[[d$node]],.compare_time,list(i = ni, x = x, y = y, row = tr$row, nrows = tr$nrow, stats = stats, f = f),tag=ni)
    }
  }

  for (a in seq_along(b))
    b[[a]] <- writeStop(b[[a]])

  b <- brick(filename)
  names(b) <-  fn
  b
}

.compare_time <- function(i,x,y,row,nrows, stats, f){
  x <- getValues(x,row=row[i], nrows=nrows[i])
  y <- getValues(y,row=row[i], nrows=nrows[i])
  if(is.null(f)){
    .compare_xy(x,y,stats)
  } else {
    res <- matrix(NA,nrow = nrow(x),ncol = nlevels(factor(f)))
    for(k in 1:nlevels(factor(f))){
      id <-  factor(f) == levels(factor(f))[k]
      print(id)
      xk <- x[,id]
      yk <- y[,id]
      res[,k] <- .compare_xy(xk,yk,stats)
    }
    res
  }
}


#' @title Compare Spatial Consistency of Two Rasters
#' @description Analysis of the spatial consistency (agreement) of two \code{\link[raster]{Raster-class}} objects with dimensions
#' x (lon), y (lat), z (time). The analysis is performed layer by layer. At a given z, an entire layer is extracted in
#' in the two rasters and the two layers are compared using a set of statistics. This results in a time serie for
#' each computed statistic, depicting the temporal profile (seasonality) of the spatial coherence (agreement) between the two datasets.
#' @usage compare_raster_space(x,y,lc,stats)
#' @param x A \code{\link[raster]{Raster-class}} object. Better be a \code{RasterBrick}, for faster computing
#' @param y Another \code{\link[raster]{Raster-class}} object to compare with the first one
#' @param lc An optional \code{\link[raster]{raster}} object giving classes for which separate statistics are retrieved
#' @param stats A character \code{vector} with one or more of the following: 'missing_x','missing_y', 'cor',
#' 'ax', 'ay', 'bx', 'by', 'ac','acu', 'acs', 'mbe', 'rmsd', 'rmspd',  'rmpdu', 'rmpds','mpdpu','mpdps'. See details
#' The \code{stats} arguments can take one or more of the following values (see Ji and Gallo, 2006):
#' \itemize{
#'    \item{\code{missing_x}}{Number of missing values in the \code{x} raster}
#'    \item{\code{missing_y}}{Number of missing valuse in the \code{y} raster}
#'    \item{\code{cor}}{Correlation coefficient}
#'    \item{\code{ax}}{Intercept of the Geometric Mean Functional Relationship (GMFR) model x = a + by}
#'    \item{\code{ay}}{Intercept of the GMFR model y = a + bx}
#'    \item{\code{bx}}{Coefficient of the GMFR model x = a + by}
#'    \item{\code{by}}{Coefficient of the GMFR model y = a + bx}
#'    \item{\code{ac}}{Agreement Coefficient}
#'    \item{\code{acu}}{Unsystematic Agreement Coefficient}
#'    \item{\code{acs}}{Systematic Agreement Coefficient}
#'    \item{\code{mbe}}{Mean Bias Error}
#'    \item{\code{rmsd}}{Root Mean Square Deviation}
#'    \item{\code{rmspd}}{Root Mean Square Deviation in percentage of mean y}
#'    \item{\code{rmpdu}}{Unsystematic Square Root of Mean Product-difference }
#'    \item{\code{rmpds}}{Systematic Square Root of Mean Product-difference}
#'    \item{\code{mpdpu}}{Proportion of the unsystematic differences over the total difference}
#'    \item{\code{mpdps}}{Proportion of the systematic differences over the total difference}
#' }
#' @return an \code{array} of statistics with 1st dimension corresponding to layers in input rasters, 2nd dimension to
#' \code{stats} and optional 3rd dimension to classes defined by \code{lc}
#' @author Antoine Stevens
#' @references
#' Ji, L., and Gallo, K. (2006). An Agreement Coefficient for Image Comparison. Photogrammetric Engineering & Remote Sensing 72, 823-833.
#' Meroni, M., Atzberger, C., Vancutsem, C., Gobron, N., Baret, F., Lacaze, R., Eerens, H., and Leo, O. (2013). Evaluation of Agreement Between Space Remote Sensing SPOT-VEGETATION fAPAR Time Series. IEEE Transactions on Geoscience and Remote Sensing 51, 1951-1962.
#' Meroni M., Fasbender D., Balaghi et al. (2015). Testing VGT data continuity between SPOT and PROBA-V missions for operational yield forecasting in North African countries. JRC Technical Report, 28 p.
#' @examples
#' \dontrun{
#' # Let's compare VGT and PROBA-V instruments during their overlaping period
#' # (October 2013 - March 2014)
#' # Don't forget to provide in copernicus_options() your user and password details
#' # for COPERNICUS data portal before running this
#' # e.g. : copernicus_options(user = "Smith", password = "hello")
#' # First, get data for NDVI_V1
#' fn_SPOT <- download_copernicus(product = 'NDVI_V1', begin = '2013-10-01', end = '2014-03-31',
#'                      tileH = 19, tileV = 4)
#' # and NDVI_V2 ...
#' fn_PROBA <- download_copernicus(product = 'NDVI_V2', begin = '2013-10-01', end = '2014-03-31',
#'                      tileH = 19, tileV = 4)
#' # Extract NDVI, export to tif
#' f_SPOT <-extract_copernicus(fn_SPOT,job = "product_comparison",layers = 2)
#' f_PROBA <-extract_copernicus(fn_PROBA,job = "product_comparison",layers = 1)
#'
#' # Convert to  rasterBrick
#' f_SPOT <- sub('\\.h5','_NDVI.tif',f_SPOT)
#' f_PROBA <- sub('\\.h5','_NDVI.tif',f_PROBA)
#' SPOT  <- writeRaster(stack(f_SPOT),filename = rasterTmpFile())
#' PROBA  <- writeRaster(stack(f_PROBA),filename = rasterTmpFile())
#' # Compare their spatial consistency
#' # by land cover class
#' # Create a fake LC map
#' lc <- raster(SPOT)
#' values(lc) <- as.numeric(cut(1:ncell(lc), 3))
#' cs <- compare_raster_space(SPOT,PROBA,lc)
#' str(cs)
#' # plot the three classes
#' # get correlation coeff
#' r <- cs[,"cor",]
#' # get acquisition dates
#' d <- scan_file_copernicus(names(SPOT))$Date
#' plot(d,r[,1],type = "l",xlab = "", ylab = "Correlation coefficient",ylim=c(0,1))
#' lines(d,r[,2],col = "red")
#' lines(d,r[,3],col = "blue")
#' legend("bottomright",lty = 1, legend = paste0("class ", 1:3), col = c("black","red","blue"))
#' }
#'
#' @export
compare_raster_space <- function(x,y,lc,
                                 stats= c("missing_x","missing_y", "cor", "ax", "ay", "bx", "by", "ac", "acu", "acs", "mbe" ,"rmsd", "rmspd", "rmpdu", "rmpds","mpdpu","mpdps")){
  stats <- match.arg(stats,several.ok = TRUE)

  if(!inherits(x,"Raster"))
    x <- stack(x, quick=TRUE)

  if(!inherits(y,"Raster"))
    y <- stack(y, quick=TRUE)

  if(!missing(lc)){
    if(!inherits(lc,"Raster"))
      lc <- stack(lc, quick=TRUE)
    z <- factor(getValues(lc))
    res <- array(NA,dim = c(nlayers(x),length(stats),nlevels(z)))
    dimnames(res) <- list(names(x),stats,levels(z))
  } else {
    z <- NULL
    res <- matrix(NA,nrow = nlayers(x),ncol = length(stats))
    rownames(res) <- names(x)
    colnames(res) <- stats
  }

  # compare dimensions
  if(!compareRaster(x,y))
    stop("x, y rasters have different geometries")
  if(nlayers(x) != nlayers(y))
    stop("x and y rasters have a different number of bands")

  if(!missing(lc))
    if(!compareRaster(x,lc))
      stop("x, y and lc rasters have different geometries")

  tr <- blockSize(x)
  chunks <- as.numeric(cut(1:nlayers(x),tr$n)) # transpose chunks to layer index
  for (i in 1:tr$n){

    ch <- which(chunks == i)
    xx <- getValuesBlock(x,row = 1, nrows = nrow(x),col = 1, ncols = ncol(x),lyrs = ch)
    yy <- getValuesBlock(y,row = 1, nrows = nrow(y),col = 1, ncols = ncol(y),lyrs = ch)

    if(is.null(z)){
      res[ch,] <- .compare_xy(xx,yy,stats,byrow = FALSE)
    } else {
      for(k in 1:nlevels(z)){
        id <-  z == levels(z)[k]
        res[ch,,k] <- .compare_xy(xx[id,],yy[id,],stats,byrow = FALSE)
      }
    }
  }
  res
}

.compare_xy <- function(x,y,stats,byrow = TRUE){
  # See Ji and Gallo (2006) An Agreement Coefficient for Image Comparison
  # pg 826 ex:
  # x = c(6, 8, 9, 10, 11, 14); y = c(2, 3, 5, 5, 6, 8)
  if(!byrow){
    x <- t(x)
    y <- t(y)
  }

  py <- ncol(y);ny <- nrow(y) # dims
  res <- matrix(nrow = ny, ncol = length(stats))
  colnames(res) <- stats

  if("missing_x" %in% stats){
    mix <- matrixStats::rowCounts(x,value = NA) # much faster
    res[,"missing_x"] <- mix
  }
  if("missing_y" %in% stats){
    miy <- matrixStats::rowCounts(y,value = NA)
    res[,"missing_y"] <- miy
  }

  # keep only complete cases
  x[is.na(y)] <- NA
  y[is.na(x)] <- NA

  n <- py - matrixStats::rowCounts(y,value = NA) # n complete cases

  mx <- .rowMeans(x,ny,py,na.rm = TRUE) # mean x
  my <- .rowMeans(y,ny,py,na.rm = TRUE) # mean x

  xmx <- x - mx # deviation to the mean x
  ymy <- y - my # deviation to the mean y

  sdx <- (.rowSums(xmx^2,ny,py,na.rm=T)/(n-1))^.5 # standard deviation x
  sdy <- (.rowSums(ymy^2,ny,py,na.rm=T)/(n-1))^.5 # standard deviation y

  if(any(stats %in% c("cor","ax","bx","by","ay","acu","acs","rmpds","rmpdu","mpdpu","mpdps"))){

    covxy <- .rowSums(xmx*ymy,ny,py,na.rm=T)/(n-1) # covariance
    r <- covxy/(sdx*sdy) # correlation
    if("cor" %in% stats)
      res[,"cor"] <- r

    if(any(stats %in% c("ax","bx","acu","acs","rmpds","rmpdu","mpdps","mpdpu"))){
      bx <- sdx/sdy * sign(r)  # coefficient of the GMFR x = a + by
      ax <- mx - (bx*my)  # intercept of the GMFR x = a + by
      xhat <- ax + (bx*y)
      if("ax" %in% stats)
        res[,"ax"] <- ax
      if("bx" %in% stats)
        res[,"bx"] <- bx
    }

    if(any(stats %in% c("ay","by","acu","acs","rmpds","rmpdu","mpdps","mpdpu"))){
      by <- sdy/sdx * sign(r) # coefficient of the GMFR y = a + bx
      ay <- my - (by*mx) # intercept of the GMFR y = a + bx
      yhat <- ay + (by*x)
      if("ay" %in% stats)
        res[,"ay"] <- ay
      if("by" %in% stats)
        res[,"by"] <- by
    }
  }

  if(any(stats %in% c("mbe","ac","acs","acu"))){
    mbe <- (mx-my) # mbe
    if("mbe" %in% stats)
      res[,"mbe"] <- mbe
  }

  if(any(stats %in% c("rmsd","rmspd","ac","acs","acu","mbe","rmpds","mpdpu","mpdps"))){

    ssd <- .rowSums((x-y)^2,ny,py,na.rm=T) # sum of squared differences

    if(any(stats %in% c("ac","acs","acu"))){
      mbe <- abs(mbe)
      spod <- .rowSums((mbe + abs(x - mx))*(mbe + abs(y - my)),ny,py,na.rm=T) # sum of potential differences
      if("ac" %in% stats){
        ac <- 1 - (ssd/spod) # agreement coefficient
        res[,"ac"] <- ac
      }
    }

    if(any(stats %in% c("rmsd","rmspd","mpdpu","mpdps"))){
      msd <- ssd / n # mean squared difference
      if(any(stats %in% c("rmsd","rmspd"))){
        rmsd <- msd^.5
        if("rmsd" %in% stats)
          res[,"rmsd"] <- rmsd
        if("rmspd" %in% stats){
          rmspd <- rmsd/my
          res[,"rmspd"] <- rmspd
        }
      }
    }
  }

  if(any(stats %in% c("acu","acs","rmpdu","rmpds","mpdps","mpdpu"))){

    spdu <- .rowSums(abs(x - xhat)*abs(y - yhat),ny,py,na.rm=T) # unsystematic sum of product-difference

    if(any(stats %in% c("rmpdu","mpdpu","mpdpu"))){
      mpdu <- spdu/n
      if("rmpdu" %in% stats){
        rmpdu <- (mpdu)^.5
        res[,"rmpdu"] <- rmpdu
      }
      if("mpdpu" %in% stats){
        mpdpu <- mpdu/msd
        res[,"mpdpu"] <- mpdpu
      }
    }

    if("acu" %in% stats){
      acu <- 1 - (spdu/spod)
      res[,"acu"] <- acu
    }

    if(any(stats %in% c("acs","rmpds","mpdps"))){
      spds <- ssd - spdu # systematic sum of product-difference
      if (any(stats %in% c("rmpds","mpdps"))){
        mpds <- spds/n
        if("rmpds" %in% stats){
          rmpds <- (mpds)^.5
          res[,"rmpds"] <- rmpds
        }
        if("mpdps" %in% stats){
          mpdps <- mpds/msd
          res[,"mpdps"] <- mpdps
        }

      }
      if("acs" %in% stats){
        acs <- 1 - (spds/spod)
        res[,"acs"] <- acs
      }
    }
  }
  res[is.nan(res)] <- NA
  res[,stats,drop=FALSE]
}

