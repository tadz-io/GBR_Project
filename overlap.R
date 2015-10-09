# overlap function
#***************************************************************************
# function takes two 2x2 bounding box matrices as inputs and returns boolean
# TRUE if bounding boxes overlap or share edge
# FALSE if bounding boxes do not overlap
#***************************************************************************  
overlap = function(b1,b2){
  
  trmA1 = abs(b1[1,1]+b1[1,2]-b2[1,1]-b2[1,2])
  trmA2 = b1[1,2]-b1[1,1]+b2[1,2]-b2[1,1]
  
  trmB1 = abs(b1[2,1]+b1[2,2]-b2[2,1]-b2[2,2])
  trmB2 = b1[2,2]-b1[2,1]+b2[2,2]-b2[2,1]
  
  if(trmA1 <= trmA2 & trmB1 <= trmB2 ){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

 


