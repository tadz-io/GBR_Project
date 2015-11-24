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
                      mse = rep(NA,maxsize*k))

  # browser()
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
      # calculate MSE
      mse = mean((predict-m[which(set==j), var])^2)
      # fill cv.mse df
      cv.mse$tree.size[(i-1)*k+j] = i
      cv.mse$mse[(i-1)*k+j] = mse
    
    
   }
   cat("iteration: ", i, "\n")
   
  }
  return(cv.mse)
}

# if using split to subset data: 
# use rbindlist(split_data[1:n]) to merge into df