#!/usr/bin/env Rscript
# Date: 27/04/2015
# Author: lihui luo (luolh@lzb.ac.cn)
# This script mask netcdf file with time dimension through shape file

library(raster)
library(ncdf)
library(rgdal)

rm(list = ls())     # clear objects  
graphics.off()      # close graphics windows   

# Set parameters
Args <- commandArgs(trailingOnly=TRUE);
if(length(Args) != 1) {
  message("netcdf_mask.R requires file_prefix file_Date as input. Terminating"); quit()
}

# Reading the shapefile
buffer30km_shp <- readOGR("QTP_railway_highway_buffer30km.shp", layer=basename("QTP_railway_highway_buffer30km"))

# Getting the spatial extent of the shapefile
e <- extent(buffer30km_shp)

# Reading the raster you want to crop with time dimension
china_raster <- brick(paste(Args[1],Args[2],".nc",sep=""), values=TRUE)

# Cropping the raster to the shapefile spatial extent
china_raster.crop <- crop(china_raster, e, snap="out")

# Dummy raster with a spatial extension equal to the cropped raster,
# but full of NA values
buffer30km_crop <- setValues(china_raster.crop, NA)

# Rasterize the catchment boundaries, with NA outside the catchment boundaries
buffer30km_shp.r <- rasterize(buffer30km_shp, buffer30km_crop)

# Putting NA values in all the raster cells outside the shapefile boundaries
china_raster.masked <- mask(x=china_raster.crop, mask=buffer30km_shp.r)

# Create new netcdf file with masking
writeRaster(china_raster.masked, paste(Args[1],,Args[2],"-masking.nc",sep=""), "CDF", zname="time", zunit=", varname=substr(Args[1],1,4)", varname=substr(Args[1],1,4), overwrite=TRUE)

