# cyclone data

# make list of all raster files
rfiles = list.files(path = "/Users/tadzio/Documents/UQProject/data/historical_MP/", pattern = 'hdr.adf', recursive = T, full.names = T)

# init df for extent for every raster
len = length(rfiles)
ext = data.frame(xmin = rep(NA, len),
                 xmax = rep(NA, len),
                 ymin = rep(NA, len),
                 ymax = rep(NA, len),
                 xres = rep(NA, len),
                 yres = rep(NA, len))

# determine extent necessary to create stack
for(i in 1:len){
  # load raster
  rast = raster(rfiles[i])
  # reproject raster
  rast = projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))
  # fill ext df
  ext$xmin[i] = extent(rast)@xmin
  ext$xmax[i] = extent(rast)@xmax
  ext$ymin[i] = extent(rast)@ymin
  ext$ymax[i] = extent(rast)@ymax
  ext$xres[i] = res(rast)[1]
  ext$yres[i] = res(rast)[2]
}

# create extent object that encompasses all rasters
# pass vector to extent() in this order: xmin, xmax, ymin, ymax
area = extent(c(min(ext$xmin),
                max(ext$xmax),
                min(ext$ymin),
                max(ext$ymax)))
# init stack
cyclStack = stack()
# init projection raster
r.proj = raster(area, crs = crs(rast))
# set resolution
res(r.proj) = res(rast)

# iteratively load raster files into a stack
for(i in 1:len){
  #load raster files again
  rast = raster(rfiles[i])
  # reproject again
  rast = projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))

  # reproject raster on extended raster file using nearest neigbor interpolation
  rast = resample(rast, r.proj, method = "ngb")
  # add layer to stack
  cyclStack = addLayer(cyclStack, rast)
}