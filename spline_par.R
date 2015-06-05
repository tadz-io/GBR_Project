# SPLINE CORRELOGRAM
#  - parrell processing for spline correlogram analysis on GBR dataset 
#  - trendsurf function detrends the data (following Bocard, Gillet & Legendre 2011; Numerical Ecology with R; Chapter 7)
#  - trendsurf function computes spline correlogram of detrended data
#  
# ************************************************************************************************************************
trendsurf = function(dat, nsamp, npoint)
{
  # libraries
  require("sp")
  require("ade4")
  require("vegan")
  require("ncf")
  
  
  # extract coordinates and project to UTM coordinate system
  xy = dat[,3:4]
  coordinates(xy) = c("lon","lat")
  proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
  xy = spTransform(xy, CRS("+proj=utm +zone=55 +datum=WGS84"))
  xy = xy@coords
  xy = data.frame(xy)
  
  # create seperate dataframe for species (excluding catagories Unc (=unclear) and WATE (=water)); MANUEL, YOU AGREE??  
  sp = dat[,5:25]
  
  # center coordinates
  xy.c = scale(xy, scale=FALSE)
  # calculate 3rd degree non-orthogonal polynomial on centered coordinates
  xy.poly = poly(as.matrix(xy.c), degree=3, raw=TRUE)
  colnames(xy.poly) = c("X", "X2", "X3", "Y", "XY", "X2Y", "Y2", "XY2", "Y3")
  
  # hellinger transformation of cover data
  sp.h = decostand(sp, "hellinger")
  assign("sp.h", sp.h, env=globalenv()) 
  # RDA with all polynomials
  sp.trend.rda = rda(sp.h ~ ., data=as.data.frame(xy.poly))
  # output adjusted R^2
  (R2adj.poly = RsquareAdj(sp.trend.rda)$adj.r.squared)
  
  # model selection
  mod0 = rda(sp.h ~ 1, data=as.data.frame(xy.poly))
  mod.sel = ordiR2step(mod0, sp.trend.rda, perm.max=200)
  mod.sel$anova #summary anova table -> all terms contribute significantly except X3. Influence of X3 seems large in RDA though??
  # select significant terms and pass to RDA
  poly.sel = attributes(mod.sel$terms)$term.labels
  sp.trend.rda2 = rda(sp.h ~ ., data=as.data.frame(xy.poly)[,poly.sel])
  
  # test for significance of RDA axis
  anova.cca(sp.trend.rda2, step=1000, by="axis")
  
  # plot first three (significant) axes
  #--------------------------------------------------------------------------------------------------------------------------------------------
  # note to self: 
  # LC scores = linear contraints: linear combinations of the independent variables. LC scores are in environmental space,
  # each axis formed by linear combinations of environmental variables.
  # WA scores = weighted averages (sums) of the species scores that are as simimlar to LC scores as possible. WA scores are in species space.
  # LC scores show were the site should be; the WA scores show where the site is.
  #--------------------------------------------------------------------------------------------------------------------------------------------
  sp.trend.fit = scores(sp.trend.rda2, choices=c(1,2,3), display="lc", scaling=1)
  par(mfrow=c(1,3))
  s.value(xy.c, sp.trend.fit[,1])
  s.value(xy.c, sp.trend.fit[,2])
  s.value(xy.c, sp.trend.fit[,3])
  
  # data detrending
  sp.h.det = resid(lm(as.matrix(sp.h) ~ ., data=as.data.frame(xy.poly)[,poly.sel])) # or only selecting lc scores of significant RDA axis???
  
  #remove sp.h from global environment
  rm(sp.h)
  
  # produce spline correlogram
  spline = spline.correlog(xy$lon, xy$lat, sp.h.det, type="boot", resamp = nsamp, npoints = npoint)
  
  # output of lowest x intercept of spline function, mean + standard dev of lowest x intercept of bootstrap results per transect
  output = c(dat$trans[i], spline$real$x.intercept, mean(spline$boot$boot.summary$x.intercept), sd(spline$boot$boot.summary$x.intercept))
  
  return(output)
}

# load casted GBR dataset
#*************************
load("/Users/tadzio/Documents/UQ\ Project/Repository/GBR_Project/cdata.Rda")

# run trendsurf script in parallel
# ********************************

library(doSNOW)
# specify threads and register clusters
threads = 2
cl = makeCluster(threads)
registerDoSNOW(cl)

# specify tasks to be run in parallel
N = unique(cdata$trans)

# run trendsurf in parallel
result = foreach(i=1:2, .combine=rbind) %dopar%
{
  trendsurf(subset(cdata, trans==N[i]), 2, 300)
}
 
# clear clusters
stopCluster(cl)

# convert matrix to dataframe
result = data.frame(result)
# save result to directory
save(result, file="result.Rda")