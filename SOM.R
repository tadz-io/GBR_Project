# Kohonen SOM
library("kohonen")
library("reshape")
library("ggplot2")

# create training set
# ---------------------------------------------------------------------
# use clustered dataset (cldata): trans_cluster.R
# normalizing density data per functional group (see Park et al. 2004)
train.n = apply(cldata[,2:22], 2, function(x){
  return((x - min(x))/(max(x) - min(x)))
  })

# initialize SOM grid
som.grid = somgrid(xdim = 4, ydim = 4, topo = "hexagonal")



# start training of SOM
# ----------------------
mod = som(train.n,
          grid = som.grid,
          rlen = 100,
          alpha = c(.05, .01),
          keep.data = TRUE,
          n.hood = "circular"
          )

# summary of SOM
summary(mod)

# plot SOM
par(mfrow=c(1,2))
plot(mod, type = "changes")
plot(mod, type = "counts")

# plot composition of SOM nodes
ncomp = melt(t(mod$codes))
colnames(ncomp) = c("fgroup", "node", "cover")
ggplot(ncomp, aes(x = node, y = cover, fill=fgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_hue()

# see Giraudel & Lek 2001 for adaption of learning rule for abundance data
