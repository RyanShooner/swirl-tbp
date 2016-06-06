################################################################
# load and update swirl functions for use in swirl-tbp 
################################################################

swirl_cat <-function(...) {
 #cat(...)
}

library(swirl)
source("parse_content.R")
source("resume.R")
source("tokens.R")
source("testResponse.R")
source("instructionSet.R")
source("hints.R")
source("info.R")

e=environment(getFromNamespace("parse_content.yaml", "swirl"))
environment(parse_content.yaml) = e
environment(resume.default) = e
environment(testResponse.default) = e
environment(rpt) = e
environment(swirl) = e
environment(waitUser.default) = e

assignInNamespace("parse_content.yaml", parse_content.yaml, "swirl") 
assignInNamespace("resume.default", resume.default, "swirl") 
assignInNamespace("testResponse.default", testResponse.default, "swirl") 
assignInNamespace("waitUser.default", waitUser.default, "swirl") 
assignInNamespace("swirl", swirl, "swirl") 

# Returns TRUE if the variable 'var' has the value 'val' with an error
# tolerance of epsilon. Error tolerance is needed to check some calculations. For example
# identical(X,Y) returns FALSE for X = 1-pnorm(3) and Y = pnorm(3, lower.tail = FALSE)
# since a user could use either, an error tolerance (e.g., eps = 1e-10) is needed
# Note that the 'var' should be the name of a variable in quotes
# do we want to add this to answerTests2.R of swirl package?
var_has_value <- function(var, val, eps = 0) {
  var <- str_trim(var)
  if(exists(var, globalenv())){
    var <- get(var, globalenv())
    return (identical(abs(val-var)<=eps, TRUE))
  } else {
    swirl_out(paste0("Error: ", var, " does not exist. Make sure to store your answer in ", var, "."))
    return(FALSE)
  }
}

environment(var_has_value) = e

## add rpt
info <- function(){
  swirl_out("When you are at the R prompt (>):")
  swirl_out("-- Typing skip() allows you to skip the current question.", skip_before=FALSE)
  swirl_out("-- Typing rpt() allows you to repeat the previous (possibly similar) question.", skip_before=FALSE)
  swirl_out("-- Typing play() lets you experiment with R on your own; swirl will ignore what you do...", skip_before=FALSE)
  swirl_out("-- UNTIL you type nxt() which will regain swirl's attention.", skip_before=FALSE)
  swirl_out("-- Typing bye() causes swirl to exit. Your progress will be saved.", skip_before=FALSE)
  swirl_out("-- Typing main() returns you to swirl's main menu.", skip_before=FALSE)
  swirl_out("-- Typing info() displays these options again.", skip_before=FALSE, skip_after=TRUE)
  invisible()
}

environment(info) = e
assignInNamespace("info", info, "swirl") 


