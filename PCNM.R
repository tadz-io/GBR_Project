library("vegan")
xy.d1 = dist(mite.xy)
# create spanning tree (spantree finds minimum distance between points and connects these points)
spanning = spantree(xy.d1)
dmin = max(spanning$dist)

# truncate distance matrix
# set values after trunction point to 4*dmin (see Borcard&Legendre 2002 and Dray et al 2006)
xy.d1[xy.d1 > dmin] = 4*dmin
# PCoA of truncated distance matrix (figure out difference with PCA!)
xy.PCoA = cmdscale(xy.d1, k=nrow(mite.xy)-1, eig=TRUE)
