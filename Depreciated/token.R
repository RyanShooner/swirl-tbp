token = function(X){
  #Currently Not Yet Ready
  
  token.find <- function(x){
    x = "In its simplest form, R can be used as an interactive calculator. Type <V1,1,10> + <V2,1,10> and press Enter."
    #x.answer = "<V1> + <V2>"
    s = strsplit(x, "<")
    s = s[[1]]
    s = s[-1]
    s = strsplit(s, ">")
    
    get.first = function(x){
      return(x[1])
    }
    
    make.random = function(x,y){
      return(c(sample(x:y, 1)))}
    
    token = sapply(s, get.first) #creates list of tokens
    token = strsplit(token, ",") #seperates values
    
    ans = NULL
    
    ans.list = list(NULL)
    
    for (i in 1:length(token)) {
      tmp = make.random((token[[i]][2]), (token[[i]][3]))
      n = c(token[[i]][1])
      #names(tmp)=n
      ans = c(ans, tmp)
      ans.list[[n]] = tmp  #to create list instead of vector
      
    }
    
    return(ans.list)
  }
  
  token.replace <- function(x,y){
    
    names(y) = paste0("<", names(y), ">")
    
    y[1] <- NULL
    
    for (i in 1:length(y)){
      x = gsub(names(y)[i], (y)[[i]], x)
      paste0("<",y[i],">")
    }
    
    return(x)
  }
  
  #y = tokenfind(x)
  #a = tokenreplace(x,y)
  
  token.overwrite <- function(x){
  x = "Calculate <V1,1,10> + <V2,5,20>"
  for (i in 1:length(y1)){
    x = sub("<.*?>", y1[i], x)}
  
  
  return(x)}
}