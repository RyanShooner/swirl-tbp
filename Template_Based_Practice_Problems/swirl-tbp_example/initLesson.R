# will be called at the start of lesson
# can use to specify token functions
gen.var <-function() {
  vars = c("x", "y", "v", "num", "X", "Y")
  sample(vars, 1)
}

#################################################
# Generate tailored hints based on user response 
#################################################
createVectorHint <- function(){
  e <- get("e", parent.frame())
  ans = c(e$token.list$num1, e$token.list$num2)
 
  # if user response makes no sense, return NA and give default hint 
  if (!is.numeric(e$val)) return(NA)

  #Check that user enters correct number of values
  if (length(ans) != length(e$val)) {
	return ("The vector should contain 2 numbers only.")
  }

  # if the user and correct answer vectors are identical, only 
  # error would be that the vector name is not correct
  if (all(e$val== ans)) {
	return ("Make sure to use the correct vector name.")	
  } 

  return (NA)

}
