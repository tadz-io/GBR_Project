#import data file
data = read.csv(file.choose(),head=FALSE)
colnames(data) = c("id","image","trans","lat","lon","label","func. group","descr.","cover")

#cast dataset
cdata = cast(id+image+trans+lat+lon~label,data=data,mean)

# remove datapoints from transect 14012 where image # > 2529 (these images don't belong to transect)
# transects 10004, 10005, 10007, 15011, 14008, 10009 are excluded in im_distr.R script
cdata =cdata[!(cdata$trans==14012 & cdata$image>140122529),]
rownames(cdata) = NULL