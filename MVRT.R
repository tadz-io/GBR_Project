# MULTIVARIATE REGRESSION TREE (MVRT)

# party
#---------------
library("party")

set.seed(290875)

# subset data
sel = sample(seq(1,1523), size = 1000, replace = F)
# # subset species matrix
# sp.sub = sp.clust[sel, 2:22]
# rownames(sp.sub) = NULL
# # subset environmental matrix
# env.sub = env.clust[sel,3:21]
# rownames(env.sub) = NULL

m = cbind(sp.clust[,-1], env.clust[,c(-1,-2)])
m$morph = as.factor(m$morph)
# create tree
# first set formula
formula = as.formula(paste(paste(colnames(sp.clust[-1]), collapse = '+'),
                           paste(colnames(env.clust)[c(-1,-2)], collapse='+'), sep = '~'))

# TO DO: include coordinates as explanatory vars
# TO DO: read main.pdf in overview-ctrees (github)
rss = integer(30)
for(i in 1:30){

  ct = ctree(formula, data = m, controls = ctree_control(testtype = "Bonferroni",
                                                         maxsurrogate = 0,
                                                         maxdepth = i))
  #plot(ct, terminal_panel = node_barplot(ct,beside = T))
  
  # get predicted vaules
  p = predict(ct)
  p = matrix(unlist(p), nrow=length(p), byrow = T)
  # calculate residuals
  # CHECK: is test_trafo original data? and what is the difference with predict_trafo??
  resid = p-ct@responses@test_trafo
  # histogram residuals
  #hist(resid, nclass = 100)
  # deviance or residual sum of squares; see:
  # http://stats.stackexchange.com/questions/6581/what-is-deviance-specifically-in-cart-rpart
  rss[i] = mean(resid^2)
  cat(i)
}

# do k-fold cross-validation
# to do: 
# TURFSA + DSUB = TURF

# plot terminal nodes
# ---------------------------------------------------------------------------------
# create tree
ct.final = ctree(formula, data = m, controls = ctree_control(testtype = "Bonferroni",
                                                             maxsurrogate = 0,
                                                             maxdepth = 3))

# get id terminal nodes
tNode = sort(unique(where(ct.final)))
# init matrix for node predictions
pNode = as.data.frame(matrix(nrow=length(tNode), ncol=length(ct.final@responses@variables)+1))
colnames(pNode)[-1] = colnames(ct.final@responses@variables)
colnames(pNode)[1] = "NodeID"
# get predictions for every node
for(i in 1:length(tNode)){
  # browser()
  pNode[i,1] = tNode[i]
  pNode[i,-1] = nodePredict.ctree(ct.final, nodeID = tNode[i])
}

#now plot output
# set colors first
library(RColorBrewer)
cols = rev(colorRampPalette(brewer.pal(8, "Set1"))(21))

# set graph. parameters
par(mfrow = c(2,length(tNode)/2))

for(i in 1:length(tNode)){
  barplot(t(as.matrix(pNode[i,-1])), 
          beside = T,
          col = cols, 
          names.arg= colnames(pNode)[-1],
          las = 2,
          ylim = c(0,0.15),
          cex.names=0.8,
          main = pNode[i,1])
}
