token.replace <- function(x,y){
  
  names(y) = paste0("<", names(y), ">")
  
  y[1] <- NULL
  
  for (i in 1:length(y)){
    x = gsub(names(y)[i], (y)[[i]], x)
    paste0("<",y[i],">")
  }
  
  return(x)
}