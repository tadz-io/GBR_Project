# SPLINE CORRELOGRAM
#  - parrell processing for spline correlogram analysis on GBR dataset 
#  - trendsurf function detrends the data (following Bocard, Gillet & Legendre 2011; Numerical Ecology with R; Chapter 7)
#  - trendsurf function computes spline correlogram of detrended data
#  
# ************************************************************************************************************************

trendsurf = function(dat, nsamp, npoint)
{
  # surpress output to terminal
  # sink("log.txt")
  # sink("/dev/null") 
  
  # libraries
  require("sp")
  require("ade4")
  require("vegan")
  require("ncf")
  
  # extract coordinates and project to UTM coordinate system
  xy = dat[,c("lat", "lon")]
  coordinates(xy) = c("lon","lat")
  proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
  xy = spTransform(xy, CRS("+proj=utm +zone=55 +datum=WGS84"))
  xy = xy@coords
  xy = data.frame(xy)
  
  # create seperate dataframe for species (excluding catagories WATE (=water))  
  group =   c("ACR-BRA", "ACR-HIP", "ACR-OTH", "ACR-PE",
              "ACR-TCD", "ALC-SF", "CCA", "DSUB",
              "FAV-MUS", "GORG", "MALG", "OTH-HC",
              "OTH-SF", "OTH-SINV", "POCI",
              "POR-BRA", "POR-ENC", "POR-MASS",
              "Sand", "Turf", "Turfsa")
  
  sp = dat[,group]
  
  # warning
  # cat("cdata has changed (no.quad included as column); columns do not match anymore")
  # readline(prompt="Press [enter] to continue")
  
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
  # anova.cca(sp.trend.rda2, step=1000, by="axis")
  
  # plot first three (significant) axes
  #--------------------------------------------------------------------------------------------------------------------------------------------
  # note to self: 
  # LC scores = linear contraints: linear combinations of the independent variables. LC scores are in environmental space,
  # each axis formed by linear combinations of environmental variables.
  # WA scores = weighted averages (sums) of the species scores that are as simimlar to LC scores as possible. WA scores are in species space.
  # LC scores show were the site should be; the WA scores show where the site is.
  #--------------------------------------------------------------------------------------------------------------------------------------------
  # sp.trend.fit = scores(sp.trend.rda2, choices=c(1,2,3), display="lc", scaling=1)
  # par(mfrow=c(1,3))
  # s.value(xy.c, sp.trend.fit[,1])
  # s.value(xy.c, sp.trend.fit[,2])
  # s.value(xy.c, sp.trend.fit[,3])
  
  # data detrending
  sp.h.det = resid(lm(as.matrix(sp.h) ~ ., data=as.data.frame(xy.poly)[,poly.sel])) 
  
  #remove sp.h from global environment
  rm(sp.h)
  
  # produce spline correlogram
  spline = spline.correlog(xy$lon, xy$lat, sp.h.det,
                           type = "boot",
                           filter = FALSE,
                           save = TRUE,
                           xmax = 500,
                           resamp = nsamp, npoints = npoint)
  
  # output of lowest x intercept of spline function, mean + standard dev of lowest x intercept of bootstrap results per transect
  output = list(trans       = dat$trans[1],
                x.intercept = spline$real$x.intercept,
                bootint     = spline$boot$boot.summary$x.intercept)
  
  # undo surpression of output
  # sink()
  
  # output transect number
  cat("Finished transect:", dat$trans[1],"\n")
  # return output
  return(output)
}

# load casted GBR dataset
#*************************
load("cdata.Rda")

# run trendsurf script in parallel
# to do: use mclapply function and split dataset per transect:
# http://lcolladotor.github.io/2013/11/14/Reducing-memory-overhead-when-using-mclapply/#.VXGb9UIkJUQ
# ***************************************************************************************************

library(parallel)
library(plyr)

# specify threads 
threads = 1

# split data
sdata = split(cdata, as.factor(cdata$trans))
# apply trendsurf in parallel
res =  mclapply(sdata,
       trendsurf, nsamp = 1000, npoint = 300,
       mc.cores = threads,
       mc.preschedule = FALSE,
       mc.set.seed = TRUE)


# save result to directory as R object
# to load use readRDS()
saveRDS(res, file="result.rds")