# cyclone data

# make list of all raster files
rfiles = list.files(path = "/Users/tadzio/Documents/UQProject/data/historical_MP/", pattern = 'hdr.adf', recursive = T, full.names = T)

# init df for extent for every raster
len = length(rfiles)
ext = data.frame(xmin = rep(NA, len),
                 xmax = rep(NA, len),
                 ymin = rep(NA, len),
                 ymax = rep(NA, len))

# determine extent necessary to create stack
for(i in 1:len){
  # load raster
  rast = raster(rfiles[i])
  projectRaster(rast, crs = CRS("+proj=longlat +datum=WGS84"))
  # fill ext df
  ext$xmin[i] = extent(rast)@xmin
  ext$xmax[i] = extent(rast)@xmax
  ext$ymin[i] = extent(rast)@ymin
  ext$ymax[i] = extent(rast)@ymax
}

# create extent object that encompasses all rasters
# pass vector to extent() in this order: xmin, xmax, ymin, ymax
area = extent(c(min(ext$xmin),
                max(ext$xmax),
                min(ext$ymin),
                max(ext$ymax)))
# init stack
cyclStack = stack()

# iteratively load raster files into a stack
for(i in 1:len){
  browser()
  rast = raster(rfiles[i])
  # extend raster files
  rast = extend(rast, area, value = NA)
  # add layer to stack
  cyclStack = addLayer(cyclStack, rast)
}