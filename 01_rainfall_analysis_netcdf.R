##TJR Sed Flux - Rainfall Analysis
#read in NetCDF historical rainfall data from 1950-2021 - monthly total (https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=form)


######################################################################
###install packages
#install.packages("RNetCDF")
#install.packages("ncdf4")
#install.packages("fields")
install.packages ("chron")
library("remotes")
remotes::install_github("RS-eco/processNC", force=TRUE)
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
setwd("C:/Users/KristineT/Downloads")

#Open data file (TRMM 3-hourly )
fname<-"adaptor.mars.internal-1639773972.1690288-26673-11-f861055c-c093-407d-88b1-d86683cad185.nc"

## read in data 
# set working directory and filename
setwd("C:/Users/KristineT/Downloads")
fname<-"adaptor.mars.internal-1639773972.1690288-26673-11-f861055c-c093-407d-88b1-d86683cad185.nc"
dname <- "tp"  # note: tp means total precip (not temporary)

#read in netcdf file as a stacked raster
raster.all <- stack(fname, varname="tp")
#raster.clip <- crop(raster.all, extent(-117.15, -115.87, 32.10, 32.94))

###Clip the raster to watershed boundaries

#read in subbasin polygon shapefiles and transform to WGS84 projection to match raster
TJR.all <- st_read("C:/Users/KristineT/Documents/Git/TJR_sedimentflux/data/contrib_wtshd.shp", quiet = T) %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs ")
TJR.active <- st_read("C:/Users/KristineT/Documents/Git/TJR_sedimentflux/data/active_wtshd.shp", quiet = T) %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs ")

#check extent of shapefiles
e.all <- extent(TJR.all)
e.active <- extent(TJR.active)

#check coordinate system
crs(raster.clip)
crs(TJR.all)

#crop stack raster to TJR watershed extent 
raster.crop <- crop(raster.all, e.all, snap="out") 
extent(raster.crop)
#crop raster to active TJR watershed area
raster.crop.active <- crop(raster.all, e.active, snap="out") 
extent(raster.crop.active)

#mask to clip to polygon layer
raster.crop.all <- mask(raster.crop, TJR.all)
plot(raster.crop.all)
raster.crop.active <- mask(raster.crop.active, TJR.active)
plot(raster.crop.active)

#



## Example SpatialPolygonsDataFrame
data(wrld_simpl)
SPDF <- subset(wrld_simpl, NAME=="Brazil")

## Example RasterLayer
r <- raster(nrow=1e3, ncol=1e3, crs=proj4string(SPDF))
r[] <- 1:length(r)

## crop and mask
r2 <- crop(r, extent(SPDF))
r3 <- mask(r2, SPDF)

## Check that it worked
plot(r3)
plot(SPDF, add=TRUE, lwd=2)
plot(r)





TJR.all.masked[TJR.all.masked == -32767]
for(i in 1:nlayers(TJR.all.masked)) {
  TJR.all.masked[[i]][TJR.all.masked[[i]] %in% -32767] <- NA
  }
TJR.all.masked[TJR.all.masked %in% c(-32767)] <- NA

# get longitude and latitude
lon <- ncvar_get(ncin,"longitude")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin,"latitude")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

# get precip
tp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")
#close netCDF file
nc_close(ncin)
#check workspace
ls()

######################
### clean up variables

## convert time variable

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

# replace netCDF fill values with NA's
tp_array[tp_array==fillvalue$value] <- NA
length(na.omit(as.vector(tp_array[,,1])))


# example of single slice
# get a single slice or layer (January)
m <- 100
tp_slice <- tp_array[,,m]
# quick map
image(lon,lat,tp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(seq(0, 0.05, .001))
levelplot(tp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))



data(wrld_simpl)

#Define min/max values to plot and colors for plotting

zmin=0.

zmax=20.

clevs<-c(0,2,4,6,8,10,12,14,16,18,20,50)

ccols<-c("#5D00FF", "#002EFF","#00B9FF","#00FFB9" ,"#00FF2E","#5DFF00","#E8FF00", "#FF8B00","red", "#FF008B","#E800FF")

palette(ccols)

#Plot image  (see result in Figure 3)

par(mfrow=c(1,1))

image.plot(xlon,ylat,precip, zlim=c(zmin,zmax), breaks=clevs, col=palette(ccols))

plot(wrld_simpl,add=TRUE)

     