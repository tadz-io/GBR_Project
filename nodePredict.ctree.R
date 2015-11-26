# EXTRACT PREDICTION VALUES FOR NODES OF OBJECT CTREE {party}

nodePredict.ctree = function(x, nodeID){
  
  require("party")
  # get predicted value of node #x
  p = nodes(x, nodeID)[[1]]$prediction
  # transpose "p" and coerce to df
  p = as.data.frame(t(p))
  # get variable names
  varnames = colnames(x@responses@variables)
  colnames(p) = varnames
  return(p)
}