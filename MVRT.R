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


# giving mob function a try
# ----------------------------
mob1 = mob(CCA + ACR.BRA ~ cots_mean + cots_probMin| dhw_time , data = m, na.action = na.omit, model = linearModel, 
           control = ctrl)

# mvpart
# --------------
library("mvpart")
ct2 = mvpart(data.matrix(m[,1:21])~cots_mean + cots_probMax + cots_probMin + cots_time + cycl_mean + 
               cycl_probMax + cycl_probMin + cycl_time + dhw_mean + 
               dhw_probMax + dhw_probMin + dhw_time + sst_mean + sst_std + 
               we_log + slope + aspect.sin + aspect.cos + morph,
             data = m, xv = "lse", xval = 10, legend = T, bars = T, size = 6)
plot(ct2)
text(ct2, splits = T)
