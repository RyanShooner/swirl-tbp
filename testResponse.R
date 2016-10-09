var_is_a <- function(class, var_name) {
  e <- get("e", parent.frame())
  class <-  str_trim(class)
  var_name <- str_trim(var_name)
  if(exists(var_name, globalenv())){
    val <- get(var_name, globalenv())
    label <- val
    results <- expectThat(val, is_a(class), label=label)
    if(is(e,"dev") && !results$passed)swirl_out(results$message)
    return(results$passed)
  } else {
    if(is(e,"dev"))swirl_out(paste(var_name, "does not exist."))
    return(FALSE)
  }
}

# original in instructionSet.R
# GD: modified to allow for repeating

# Only the question classes enter a testing loop. Testing is the
# same in both cases. If the response is correct they indicate
# instruction should progress. If incorrect, they publish a hint
# and return to the previous step.


testResponse.default <- function(current.row, e){
  swirl_cat("in testResponse...\n")

  # Increment attempts counter
  e$attempts <- 1 + e$attempts
  # Get answer tests
  tests <- current.row[,"AnswerTests"]

  swirl_cat("Answer Tests = ", tests, "\n")

  if(is.na(tests) || tests == ""){
    results <- is(e, "dev")
    if(!results){
      stop("BUG: There are no tests for this question!")
    }
  } else {
    tests <- str_trim(unlist(strsplit(tests,";")))
    swirl_cat("after trim, tests = ", tests, "\n")
    results <- lapply(tests, function(keyphrase){testMe(keyphrase,e)})
  }
  correct <- !(FALSE %in% unlist(results))
  if(correct){
    swirl_out(praise())
    e$iptr <- 1

    #update TimesRepeated for this question and move to next row if we are done
    num = as.integer(e$current.row$TimesRepeated)
    num = num + 1
    e$les[e$row,]$TimesRepeated = num
    swirl_cat("upate num to ", num, "\n")
    if (num >= e$current.row$NumTimes) {
      e$row <- 1 + e$row
    }
    # Reset attempts counter, since correct
    e$attempts <- 1
  } else {
    # Restore the previous global environment from the official
    # in case the user has garbled it, e.g., has typed x <- 3*x
    # instead of x <- 2*x by mistake. The hint might say to type
    # x <- 2*x, which would result in 6 times the original value
    # of x unless the original value is restored.
    if(length(e$snapshot)>0)xfer(as.environment(e$snapshot), globalenv())
    mes <- tryAgain()
    if(is(current.row, "cmd_question")) {
      mes <- paste(mes, "Or, type info() for more options.")
    }
    swirl_out(mes)
  
    # get hint, possibly using hint function 
    temp <- current.row[,"Hint"]
    if (!is.na(e$current.row$HintFunction)) {
    	 if (!is.na(temp)) {
      	     swirl_out(temp, skip_after=!is_mult)
    	 }  
         hf = get(e$current.row$HintFunction)
	 temp <- hf()
    }
    
    # Suppress extra space if multiple choice
    is_mult <- is(e$current.row, "mult_question")
    # If hint is specified, print it. Otherwise, just skip a line.
    if (!is.na(temp)) {
      swirl_out(temp, skip_after=!is_mult)
    } else {
      message()
    }
    e$iptr <- e$iptr - 1
  }
}

testMe <- function(keyphrase, e){
  # patch to accommodate old-style tests

  swirl_cat("atesting: ", keyphrase, "\n")
  oldcourse <- attr(e$les, "course_name") %in%
    c("Data Analysis", "Mathematical Biostatistics Boot Camp",
      "Open Intro")

  if(oldcourse){
	swirl_cat("old course...\n")
    # Use old test syntax
    # Add a new class attribute to the keyphrase using
    # the substring left of its first "=".
    attr(keyphrase, "class") <- c(class(keyphrase),
                                  strsplit(keyphrase, "=")[[1]][1])
    return(runTest(keyphrase, e))
  } else {
    swirl_cat("eval...\n")
    # Use new test syntax
    return(eval(parse(text=keyphrase)))
  }
}

# CUSTOM TEST SUPPORT. An environment for custom tests is inserted
# "between" function testMe and the swirl namespace. That is,
# an environment, customTests, is created with parent swirl
# and child testMe. Code evaluated within testMe will thus search
# for functions first in customTests, and then in the swirl namespace.
#
# Custom tests must be defined in a file named "customTests.R" in the
# lesson directory. Tests in such files are loaded into environment
# customTests when a lesson is first loaded or progress is restored.
# The environment is cleared between lessons.

# An environment with parent swirl to hold custom tests.
customTests <- new.env(parent=environment(testMe))
# Make customTests the parent of testMe.
environment(testMe) <- customTests

# Function to load custom tests from a source file.
loadCustomTests <- function(lespath){
  customTests$AUTO_DETECT_NEWVAR <- TRUE
  cfile <- file.path(lespath,"customTests.R")
  if(file.exists(cfile)){
    source(cfile, local=customTests)
  }
  return(TRUE) # legacy
}

# Function to remove everything from environment customTests
clearCustomTests <- function(){
  remove(list=ls(customTests), envir=customTests)
}
