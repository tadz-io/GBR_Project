library(sp)
library(rgdal)
library(data.table)
library(dplyr)

# load function to calculate euclidean distance between consequtive images
source("/Users/tadzio/Documents/UQProject/scripts/eudist.R")

#split dataset per transect
sdata = split(cdata, as.factor(cdata$trans))

# max distance interval within cluster
d = 70


clust.seq = lapply(sdata, int = d, function(x, int){
  
  # assign input of lapply to new var
  xy = x
  
  # project coordinates to UTM 
  coordinates(xy) = c("lon", "lat")
  proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
  xy = spTransform(xy, CRS("+proj=utm +zone=55 +datum=WGS84"))
  
  # sequential clustering code
  #--------------------------------------------
  
  # calculate cumulative distance
  # x is transformed to UTMs within eudist function
  cumdist = cumsum(eudist(x)[,2])
 
  # cut transect with specified intervals
  sc = cut(cumdist, seq(0, max(cumdist), d))
  # assign first image to cluster 1
  # cl only contains factors for 2nd image up to the last since distance is calculated between consequetive images
  sc = c(1, sc@.Data)
    
  # split transect according to cluster (=sc)
  subtrans = split(xy, as.factor(sc))
  
  # calcualte values per cluster
  subtrans.dat = lapply(subtrans, function(y){
    
    # calculate max distance between images and number of images + quads per cluster
    dmax = max(dist(y@coords))
    no.img = length(y$image)
    no.quad = sum(y$no.quad)
    
    # define output
    list(images = y$image,
         no.img = no.img,
         no.quad = no.quad,
         dmax = dmax)
  })
  
})

clust.hrc = lapply(sdata, int =d, function(x, int){

  # assign input of lapply to new var
  xy = x
  
  # project coordinates to UTM 
  coordinates(xy) = c("lon", "lat")
  proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
  xy = spTransform(xy, CRS("+proj=utm +zone=55 +datum=WGS84"))
  
  # hierarchical clustering code
  # -----------------------------------------
  
  # use complete-linkage hierarchical clustering
  hc = hclust(dist(coordinates(xy)), method = "complete")
  # cut tree at specified distance 
  hc.d = cutree(hc, h=d)
  
  # split transect according to hierarchical clusters (=hc.d)
  subtrans2 = split(xy, as.factor(hc.d))
  
  subtrans2.dat = lapply(subtrans2, function(w){
    # calculate max distance between images and number of images + quads per cluster
    dmax = max(dist(w@coords))
    no.img = length(w$image)
    no.quad = sum(w$no.quad)
    
    # define output
    list(images = w$image,
         no.img = no.img,
         no.quad = no.quad,
         dmax = dmax)
    
  })

})

# get cluster-id names
n = names(unlist(clust.seq, recursive = FALSE))
n2 = names(unlist(clust.hrc, recursive = FALSE))

# unlist and create list of dataframe
# for sequential clustering
clust.seq = lapply(unlist(clust.seq, recursive = FALSE), function(z){
  as.data.frame(z)
})
# for hierarchical clustering
clust.hrc = lapply(unlist(clust.hrc, recursive = FALSE), function(z){
  as.data.frame(z)
})

# bind list with id (=n, n2)
clust.seq = rbindlist(Map(cbind, id = as.factor(n), clust.seq))
clust.hrc = rbindlist(Map(cbind, id = as.factor(n2), clust.hrc))

# aggregate data
ag.seq = aggregate(cbind(no.quad, no.img, dmax) ~ id, data = clust.seq, mean, na.rm = TRUE)
ag.hrc = aggregate(cbind(no.quad, no.img, dmax) ~ id, data = clust.hrc, mean, na.rm = TRUE)

# show histograms
par(mfrow=c(2,2))
hist(ag.seq$no.quad, xlim = c(0,100), ylim = c(0,700), breaks=seq(0,500,5))
hist(ag.seq$dmax, ylim = c(0,1000))
hist(ag.hrc$no.quad, xlim = c(0,100), ylim = c(0,700), breaks=seq(0,500,5))
hist(ag.hrc$dmax, ylim = c(0,1000))


# filter
# specify the maximum distance between two points in the cluster, below which clusters should be removed from analysis
dmin = 35
# specify minimum number of quadrants within cluster
qmin = 30

# select images of clusters that meet requirements
clust.sel = clust.seq[clust.seq$no.quad > qmin & clust.seq$dmax > dmin,]
# match and bind cluster-id to cdata
cldata = cbind(clust.id = clust.sel$id,
               cdata[cdata$image %in% clust.sel$image,])

rownames(cldata) = NULL

# aggregate data within cluster

cldata = aggregate(. ~ clust.id, data = cldata, FUN = mean)[,c(1,7:27)]

# redundant
# --------------------------------------------------------
# fetch method from id name and create boolean
sc.id = 1:dim(cl.data)[1] %in% grep("sc", cl.data$id)
hc.id = 1:dim(cl.data)[1] %in% grep("hc", cl.data$id)

# assign method to column
cl.data$clust[sc.id] = "sc"
cl.data$clust[hc.id] = "hc"


