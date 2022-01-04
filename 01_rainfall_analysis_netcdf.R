##TJR Sed Flux - Rainfall Analysis
#read in NetCDF historical rainfall data from 1950-2021 - monthly total (https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form)


######################################################################
###install packages
# install.packages("RNetCDF")
# install.packages("ncdf4")
# install.packages("fields")
# install.packages ("chron")
library("remotes")
# remotes::install_github("RS-eco/processNC", force=TRUE)
remotes::install_github("USGS-R/smwrBase")
library(smwrBase)
library(processNC)

#Load packages to read netCDF files
library(RNetCDF)
library(ncdf4)

#Load Plotting package
library(fields)
library(maptools)

#Load other packages
library(chron)
library(lattice)
library(RColorBrewer)
library(raster)
library(sf)


######################################################################
#Change to your working directory
#setwd("C:/Users/KristineT/Downloads")
setwd("C:/Users/KristineT.SCCWRP2K/SCCWRP/OPC Sediment Flux to Coast - TJR Sediment Flux/Data/TJR_data/rainfall/")

#directory with watershed boundaries
shape.dir <- "C:/Users/KristineT.SCCWRP2K/Documents/Git/TJR_sedimentflux/data/"

## read in data 
# set filename
fname<-"adaptor.mars.internal-1639773972.1690288-26673-11-f861055c-c093-407d-88b1-d86683cad185.nc"
dname <- "tp"  # note: tp means total precip (not temporary)

#read in netcdf file as a stacked raster
raster.all <- stack(fname, varname="tp")

#read in netcdf to get units
nc.data <- open.nc(fname)
print(nc.data)
dat<-read.nc(nc.data)

######################################################################
###Clip the raster to watershed boundaries

#read in subbasin polygon shapefiles and transform to WGS84 projection to match raster
#entire TJR watershed
TJR.all <- st_read(paste0(shape.dir, "contrib_wtshd.shp"), quiet = T) %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs ")
#active TJR watershed downstream of dams
TJR.active <- st_read(paste0(shape.dir, "active_wtshd.shp"), quiet = T) %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs ")

#check extent of shapefiles
e.all <- extent(TJR.all)
e.active <- extent(TJR.active)

#check coordinate system
crs(TJR.active)
crs(TJR.all)

#crop stack raster to TJR watershed extent 
raster.crop <- crop(raster.all, e.all, snap="out") 
extent(raster.crop)
#crop raster to active TJR watershed area
raster.crop.active <- crop(raster.all, e.active, snap="out") 
extent(raster.crop.active)

#mask clip to polygon layer (crop to polygon shape, instead of extent rectangle), plot to check
raster.crop.all <- mask(raster.crop, TJR.all)
plot(raster.crop.all)
raster.crop.active <- mask(raster.crop.active, TJR.active)
plot(raster.crop.active)

######################################################################
##stack algebra - sum each cell based on water year total monthly precip, get a raster with stacks for each WY totals, summarize by mean annual WY total precip

#find the stack names for each raster
stack.names <- names(raster.crop.all)
stack.names <- gsub("X", "", stack.names)
stack.names <- gsub("[.]", "-", stack.names)

#format as date, find water year associated with each stack, water year 1950 is partial, start with WY 1951
date <- as.POSIXct(stack.names, format = "%Y-%m-%d")
water.year <- waterYear(date)

#stack algebra - sum each cell based on water year total precip
wy.total.p.all <- stackApply(raster.crop.all, water.year, fun=sum)
wy.total.p.active <- stackApply(raster.crop.active, water.year, fun=sum)
#set the level names to WY_ instead of level_
names(wy.total.p.all) <- gsub("level_", "WY_", names(wy.total.p.all))
names(wy.total.p.active) <- gsub("level_", "WY_", names(wy.total.p.active))

#summarize the mean total annual precip for each watershed all and active, convert m to mm
mean.wy.totalP.all.mm <- cellStats(wy.total.p.all, stat='mean', na.rm=TRUE)*1000
mean.wy.totalP.active.mm <- cellStats(wy.total.p.active, stat='mean', na.rm=TRUE)*1000
#summarize the sum, total annual precip for each watershed all and active, convert m to mm
sum.wy.totalP.all.mm <- cellStats(wy.total.p.all, stat='sum', na.rm=TRUE)*1000
sum.wy.totalP.active.mm <- cellStats(wy.total.p.active, stat='sum', na.rm=TRUE)*1000


#normalized mean total annual precip
#areas
#all.area.km2 <- 4364.6229
#active.area.km2 <- 1138 
#normal.mean.wy.totalP.all.mm <- cellStats(wy.total.p.all, stat='mean', na.rm=TRUE)*1000/11.1*all.area.km2
#normal.mean.wy.totalP.active.mm <- cellStats(wy.total.p.active, stat='mean', na.rm=TRUE)*1000/11.1*active.area.km2


#create vector of unique water years
Water.Year <- as.numeric(as.character(unique(water.year)))

#create dataframe with active mean total P and all mean total P
output <- data.frame(cbind(Water.Year, mean.wy.totalP.all.mm, mean.wy.totalP.active.mm, sum.wy.totalP.all.mm, sum.wy.totalP.active.mm))
#remove the first row of 1950 because it was partial year
output <- output[2:length(output$Water.Year),]

#write csv
write.csv(output, file="C:/Users/KristineT.SCCWRP2K/Documents/Git/TJR_sedimentflux/output_data/mean_WY_totalP_TJR_all_active_wtshd.csv", row.names=FALSE)




