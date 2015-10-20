tokenreplace2 <- function(x,y){
  
  names(y) = paste0("<", names(y), ">")
  
  y[1] <- NULL
  
  for (i in 1:length(y)){
    x = gsub(names(y)[i], (y)[[i]], x)
    paste0("<",y[i],">")
  }
  
  return(x)
}

x1 = "<V1> + <V2>"
y1 = list(X = NULL, V1 = 5, V2 = 3)
tokenreplace2(x1,y1)

x2 = "x = <V1>:<V2>"
y2 = list(X=NULL,V1 = 1, V2 = 100)
tokenreplace2(x2,y2)

#####################################################################
# write a function that takes x and y, and replaces each token of the 
# form <V1*> with <V1> and then replaces the token with the correct 
# value, where * denotes the wildcard character (.* using regular
# expressions)
#####################################################################

############################### 
# Regular expression2
############################### 

 ?regex # if you need help

 x = "Calculate <V1,1,10> + <V2,5,20>"
 
# replace the token <V1 with a -
token1 = "<V1"
gsub(token1, "-", x)

# replace l with a -
gsub("l", "-",x)

# The dot (.) matches any character.
# replace l and the next character with a -
gsub("l.", "-",x)

# A * matches the preceeding item 0 or more times
# Replace any string beginning and ending with an 'l'
# with a -
gsub("l.*l", "-", x)
