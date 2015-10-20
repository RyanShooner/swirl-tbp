tokenreplace = function(x, y){ #Assuming x is the CorrectAnswer input ("<V1> + <V2>"), and Y is the output from TokenFind (List of three: Null, V1, and V2)
  s = strsplit(x, "<")
  s = s[[1]]
  s = s[-1]
  s = strsplit(s, ">")
  
  get.first = function(x){
    return(x[1])
  }
x = sapply(s, get.first)  
y[[1]] <- NULL  
for (i in 1:length(x)){
x = gsub(x[i], y[i], x)
paste0("<",y[i],">")
}

return(x)

}