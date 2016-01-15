# nMDS
library("vegan")
# need sp.clust from trans_cluster.R
# TURFSA + DSUB = TURF
sp.clust$Turf = sp.clust$Turf + sp.clust$Turfsa + sp.clust$DSUB
sp.clust$Turfsa = NULL
sp.clust$DSUB = NULL

group = c("ACR.BRA","ACR.HIP","ACR.OTH","ACR.PE","ACR.TCD",
          "ALC.SF","CCA","FAV.MUS","GORG","MALG","OTH.HC",
          "OTH.SF","OTH.SINV","POCI","POR.BRA","POR.ENC",
          "POR.MASS","Sand","Turf")
sp.matrix = sp.clust[,group]
sp.matrix = cbind(id = seq(1,dim(sp.matrix)[1]), sp.matrix)

sp.melt = melt(sp.matrix, id.vars = "id")
sp.melt$func = func_cat$func_group[match(sp.melt$variable, func_cat$label)]
#  !!!!!!!!!
sp.matrix = cast(sp.melt, id~func, sum)[,-1]
# bind node IDs to sp.matrix
# need output of ctree
sp.matrix = cbind(where(ct.final), sp.matrix)
colnames(sp.matrix)[1] = "node"
# aggregate groups

# specify which nodes to use for nMDS
node.id = c(3,4)
# to select those rows:
# sp.matrix[(sp.matrix[,"node"] %in% node.id),-1]
# do transformations here
nMDS = metaMDS(sp.matrix[(sp.matrix[,"node"] %in% node.id),-1], k=2, trymax = 10)

# create ggplot of nMDS
library("devtools")
source_url("https://raw.github.com/JoFrhwld/FAAV/master/r/stat-ellipse.R")
nMDS.df = data.frame(MDS1 = nMDS$points[,1], MDS2 = nMDS$points[,2])
nMDS.df$class = as.factor(sp.matrix[(sp.matrix[,"node"] %in% node.id),1])
# make species df
sp.cords = as.data.frame(nMDS$species)
# ggplot
ggplot() +
  geom_point(data = nMDS.df, aes(MDS1,MDS2, color=class, shape = class)) +
  stat_ellipse(data = nMDS.df, level = 0.68, geom="polygon", alpha = 0.2, aes(MDS1,MDS2,fill = class, color = class)) +
  geom_point(data = sp.cords, aes(MDS1,MDS2)) +
  geom_text(data = sp.cords, aes(MDS1,MDS2, label=row.names(sp.cords)))
# or qplot
qplot(data = nMDS.df, x = MDS1, y = MDS2, colour = class) + stat_ellipse()

View(sp.matrix$'ACR.BRA')
# try nMDS on node responses
# first reshape pNode.s to wide format
pNode.wide = reshape(pNode.s[,c("NodeID","variable","cover")],
                     idvar = "NodeID",
                     timevar = "variable",
                     direction = "wide")
rownames(pNode.wide) = NULL
# not working because k must be n-1
nMDS.nodes = metaMDS(as.matrix(pNode.wide[,-1]), k=1, trymax = 10)

# try PCA
pca.nodes = prcomp(pNode.wide[,-1],
                   center = T,
                   scale = T)

# try PCA with ggbiplot github repo
install_github("vqv/ggbiplot")
library("devtools")
library("ggbiplot")
# create factor list for sampling points
class = where(ct.final)
class[class==2] = "low"
class[class==3] = "high"
class = as.factor(class)
# TURFSA + DSUB = TURF
sp.clust$Turf = sp.clust$Turf + sp.clust$Turfsa + sp.clust$DSUB
sp.clust$Turfsa = NULL
sp.clust$DSUB = NULL
# do PCA
pca.nodes = prcomp(sp.clust[,c(-1,-2,-3)], scale = T, center = T)
# make plot
ggbiplot(pca.nodes, obs.scale = 1, var.scale = 1,
         groups = class, ellipse = TRUE, circle = FALSE, alpha = 0.7) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
# doesn't seem to be much, try grouping by func. or morph