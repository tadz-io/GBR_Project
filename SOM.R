# Kohonen SOM
library("kohonen")

# create training set
# use clustered dataset (cldata): trans_cluster.R
train = as.matrix(cldata[,2:22])
# check if scaling is still required (see Park et al. 2004)

# initialize SOM grid
som.grid = somgrid(xdim = 4, ydim = 4, topo = "hexagonal")

# start training of SOM
mod = som(train,
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
library("reshape")
library("ggplot2")
library("RColorBrewer")

ncomp = melt(t(mod$codes))
colnames(ncomp) = c("fgroup", "node", "cover")
ggplot(ncomp, aes(x = node, y = cover, fill=fgroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_hue()

# see Giraudel & Lek 2001 for adaption of learning rule for abundance data
