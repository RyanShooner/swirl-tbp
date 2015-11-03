########################################################
# Assignment:
# (1) Complete the create.tokens function which takes a 
# token string, evaluates the R code, and returns a list 
# of tokens with names and values.
# (2) Use the create.tokens function and the token.find
# function to update the output and answer given below
########################################################

## return a list of tokens, similar to what token.find did previously
token.replace <- function(y,x){
  
  for (i in 1:length(y)){
    x = sub("<.*?>", y[[i]][2], x)} #Not ideal.  Need to change y[[i]][2] to account for all possible codes, maybe?
  return(x)
}

create.tokens <- function(token.str, output.to) { #token.str will be code to input, output.to will be the value to apply token.replace to
  eval(parse(text = token.str))   #pulls the code out of the character string and executes it using magic
  a = ls.str(pattern = "V")       # creates a vector of tokens in the function environment
  x = list()                      #creates blank list
  for (i in 1:length(a)){
    x[i] = list(c(a[i], get(a[i]))) #This took forever to figure out.  Had to add a list into the list?
  }
  z = token.replace(x, output.to) #takes the output of create.tokens and applys it to the desired output value
  return(z)
}


##########################################################
# Test case, use create.tokens and token.replace to
# update the output and answer strings
##########################################################
tokens = "V1 = sample(1:10,1); V2 = V1 + sample(3:20,1)"
output = "create a vector x that contains <V1> through <V2>"
answer = "x = <V1>:<V2>"




####################################################################
# Environment code is below (this is no longer needed)
####################################################################

#token.env = new.env()
#ls() # listing of global environment
#ls(envir = token.env) # listing of token environment

#eval(parse(text = tokens), envir = token.env)
#ls(envir = token.env) # listing of token environment

# get vector of token names
#ls(envir = token.env) # listing of token environment
#get("V1", envir = token.env)

