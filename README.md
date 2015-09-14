# Access and process COPERNICUS Global Land Vegetation products

The aim of `copernicus` is to access to the [COPERNICUS Global Land Vegetation data portal](http://land.copernicus.eu/global/). 
The package provides functions to easily download, extract, convert, subset and project COPERNICUS data.
__The user should be registered to access data !__ The package is at its early development stage. Use with care!  
Please report bugs to <antoinexstevens@gmail.com>

## To install copernicus, use:

```r
devtools::install_github("antoinestevens/copernicus")
```

One of the dependencies of `copernicus` is the `rhdf5` package. 
It can be installed from [bioconductor](https://www.bioconductor.org/packages/release/bioc/html/rhdf5.html)

```r
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

File extraction and image projection is done via [GDAL](http://www.gdal.org/). GDAL can be otbtained through for instance the
[OSGeo4W](https://trac.osgeo.org/osgeo4w/) project (windows).

## Download

`get_copernicus` wil download zip files on the data portal based on an extent or given tiles (h,v) pairs and a product name.
Only the following products are available:  `NDVI_V1` (Normalized Difference Vegetation Index - VGT instrument),
`NDVI_V2` (Normalized Difference Vegetation Index - PROBAV instrument),`LAI` (Leaf Area Index),`FCOVER` (Fraction of Vegetation Green Cover),
`FAPAR` (Fraction of Absorbed Photosynthetically Active Radiation),`VCI` (Vegetation Condition Index),`VPI` (Vegetation Productivity Index),
`DMP` (Dry Matter Productivity),`BA` (Burnt Areas).
The availability of the data between two dates can be checked with `check_time_copernicus` and at a given location with `check_tile_copernicus`.
One can get the URL's of images for a given time period, location and product name with `get_url_copernicus`.
Information included in the name of the files (e.g. aglorithm version, instrument) can be retrieved with `scan_file_copernicus`.
The COPERNICUS tiling system can be created with `gen_tile_copernicus` and fetched with `get_tile_copernicus`
which will retrieve tiles info (h,v,bounding box in LatLon) based on an extent or (h,v) pair.

## Extracting images

File conversion can be done with `extract_copernicus`, which will transfrom raw h5 files to tif, by converting DN to physical values
using the associated gain and offset of each product. The function can further subset data on a given extent, resample to a given resolution
and project to a given coordinate system. One can also choose which layers of the h5 files can be extracted.
Quality Flags can be extracted with `convert_qf_copernicus`.

## Package options

Some of the package default options are set with `copernicus_options`, such as the download path, user name and password.

TODO: mosaiking
