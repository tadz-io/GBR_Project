# K-FOLD CROSS-VALIDATION FOR CONDITIONAL INFERENCE TREES
# CV for ctree (party package)

CV.ctree = function(x,k,formula,maxsize){
  require("party")
  # set seed
  set.seed(1)
  # get response var names
  var = unlist(strsplit(as.character(formula)[2], split = ".[+]."))
  # init df for MSE
  cv.mse = data.frame(tree.size = rep(NA,maxsize*k),
                      mse = rep(NA,maxsize*k),
                      r2 = rep(NA,maxsize*k))
  browser()
  # size of dataset
  n = dim(x)[1]
  # create subset indices
  set = sample(rep(1:k, length.out = n))

  # CV for every size of tree
  for(i in 1:maxsize){
   # for every set
    for(j in 1:k){
      # train model on k-1 sets
      tree.train = ctree(formula, x[set!=j,], controls = ctree_control(testtype = "Bonferroni",
                                                                      maxsurrogate = 0,
                                                                      maxdepth = i))
      # test model on remaining set
      predict = predict(tree.train, newdata = x[set==j,])
      # convert list to matrix
      predict = matrix(unlist(predict), nrow = length(predict), byrow = T)
      # observed values of test set
      obs = m[which(set==j), var]
      # calculate MSE
      mse = mean((predict-obs)^2)
      # calculate total sum of squares (tss) and residual sum of squares (rss)
      tss = sum((obs-mean(as.matrix(obs)))^2)
      rss = sum((predict-obs)^2)
      # calculate R2
      r2 = 1-rss/tss
      # fill cv.mse df
      cv.mse$tree.size[(i-1)*k+j] = i
      cv.mse$mse[(i-1)*k+j] = mse
      cv.mse$r2[(i-1)*k+j] = r2
      }
   cat("iteration: ", i, "\n")   
  }
  return(cv.mse)
}

# if using split to subset data: 
# use rbindlist(split_data[1:n]) to merge into df