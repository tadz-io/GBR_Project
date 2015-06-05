# IMPORT OF GBR DATASET
# script imports GBR dataset (.csv) and casts by ID first
# not all transects included: see comments below
#**********************************************************
library("reshape")

#import data file
data = read.csv(file.choose(),head=FALSE)
colnames(data) = c("id","image","trans","lat","lon","label","func. group","descr.","cover")

#cast dataset
cdata = cast(id+image+trans+lat+lon~label,data=data,mean)

# remove datapoints from transect 14012 where image # > 2529 (these images don't belong to transect)
# remove transects 10004, 10005, 10007, 10009, 14008, 15011 (see comments logbook)
cdata =cdata[!(cdata$trans==14012 & cdata$image>140122529),]
cdata = subset(cdata, !cdata$trans %in% c(10004,10005,10007,10009,14008,15011))
rownames(cdata) = NULL

# aggregate data by unique images
cdata = aggregate(.~ image, data=cdata, mean)
cdata$id = NULL

# add catagory Unc (=unclear) to DSUB (=death substrate)
# machine often classifies death substrate as unclear
cdata$DSUB = cdata$DSUB + cdata$Unc
# remove Unc and WATE (=water) catagory
cdata$Unc = NULL
cdata$WATE = NULL