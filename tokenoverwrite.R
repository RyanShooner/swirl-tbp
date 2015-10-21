token.overwrite <- function(x, y){
  #For testing, uncomment the following two lines, or comment them for usage with other functions:
  #x = "Calculate <V1,1,10> + <V2,5,20>"
  #y = list(X = NULL, V1 = 5, V2 = 3)
  #NOTE:  Must add y[1] = NULL for current format of other token. functions.  Will be changed later.
  y[1] = NULL
  for (i in 1:length(y)){
    x = sub("<.*?>", y[i], x)}
  
  
  return(x)}