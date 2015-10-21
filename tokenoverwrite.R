token.overwrite <- function(x){
  x = "Calculate <V1,1,10> + <V2,5,20>"
  #NOTE:  Must add y1[1] = NULL for current format of other token. functions
  for (i in 1:length(y1)){
    x = sub("<.*?>", y1[i], x)}
  
  
  return(x)}