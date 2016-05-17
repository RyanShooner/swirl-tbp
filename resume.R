
# allows user to repeat the previous question
rpt <- function(){
  invisible()
}

# Default method resume implements a finite state (or virtual) machine. 
# It runs a fixed "program" consisting of three "instructions" which in 
# turn present information, capture a user's response, and test and retry 
# if necessary. The three instructions are themselves S3 methods which 
# depend on the class of the active row of the course lesson. The 
# instruction set is thus extensible. It can be found in R/instructionSet.R. 

# modified from swirl_2.2.21:
# GD: added this line before executing current instruction 
#     e$current.row = token.generate(e$current.row) 

resume.default <- function(e, ...){

  cat("\n\n==== resuming ====\n\n")
  # Check that if running in test mode, all necessary args are specified
  if(is(e, "test")) {
    # Capture ... args
    targs <- list(...)
    # Check if appropriately named args exist
    if(is.null(targs$test_course) || is.null(targs$test_lesson)) {
      stop("Must specify 'test_course' and 'test_lesson' to run in 'test' mode!")
    } else {
      # Make available for use in menu functions
      e$test_lesson <- targs$test_lesson
      e$test_course <- targs$test_course
    }
    # Check that 'from' is less than 'to' if they are both provided
    if(!is.null(targs$from) && !is.null(targs$to)) {
      if(targs$from >= targs$to) {
        stop("Argument 'to' must be strictly greater than argument 'from'!")
      }
    }
    if(is.null(targs$from)) {
      e$test_from <- 1
    } else {
      e$test_from <- targs$from
    }
    if(is.null(targs$to)) {
      e$test_to <- 999 # Lesson will end naturally before this
    } else {
      e$test_to <- targs$to
    }
  }
  
  esc_flag <- TRUE
  on.exit(if(esc_flag)swirl_out("Leaving swirl now. Type swirl() to resume.", skip_after=TRUE))
  # Trap special functions
  if(uses_func("info")(e$expr)[[1]]){
    esc_flag <- FALSE
    return(TRUE)
  }
  if(uses_func("nxt")(e$expr)[[1]]){    
    ## Using the stored list of "official" swirl variables and values,
    #  assign variables of the same names in the global environment
    #  their "official" values, in case the user has changed them
    #  while playing.
    if(length(e$snapshot)>0)xfer(as.environment(e$snapshot), globalenv())
    swirl_out("Resuming lesson...")
    e$playing <- FALSE
    e$iptr <- 1
  }
  
  # The user wants to reset their script to the original
  if(uses_func("reset")(e$expr)[[1]]) {
    e$playing <- FALSE
    e$reset <- TRUE
    e$iptr <- 2
    swirl_out("I just reset the script to its original state. If it doesn't refresh immediately, you may need to click on it.", 
              skip_after = TRUE)
  }
 
  # GD: The user repeat the previous question
  if (uses_func("rpt")(e$expr)[[1]]) {
     e$playing <- FALSE
     e$iptr <- 1

     # if current question has not been repeated yet, then go to previous question 
     # this is not the case
     num = as.integer(e$les[e$row,]$TimesRepeated)
     if (num == 0) { 
     	e$row <- e$row - 1
     }
	
     # update TimesRepeated counter in all cases 
     num = as.integer(e$les[e$row,]$TimesRepeated)
     num = num - 1
     e$les[e$row,]$TimesRepeated = num
     swirl_out("Repeating the previous question.", skip_after = TRUE)
  } 
 
  # The user wants to submit their R script
  if(uses_func("submit")(e$expr)[[1]]){
    e$playing <- FALSE
    # Get contents from user's submitted script
    e$script_contents <- readLines(e$script_temp_path, warn = FALSE)
    # Save expr to e
    e$expr <- try(parse(text = e$script_contents), silent = TRUE)
    swirl_out("Sourcing your script...", skip_after = TRUE)
    try(source(e$script_temp_path))
  }
  
  if(uses_func("play")(e$expr)[[1]]){
    swirl_out("Entering play mode. Experiment as you please, then type nxt() when you are ready to resume the lesson.", skip_after=TRUE)
    e$playing <- TRUE
  }
  
  # If the user wants to skip the current question, do the bookkeeping.
  if(uses_func("skip")(e$expr)[[1]]){
    # Increment a skip count kept in e.
    if(!exists("skips", e)) e$skips <- 0
    e$skips <- 1 + e$skips
    # Enter the correct answer for the user
    # by simulating what the user should have done
    correctAns <- e$current.row[,"CorrectAnswer"]
    
    # If we are on a script question, the correct answer should
    # simply source the correct script
    if(is(e$current.row, "script") && is.na(correctAns)) {
      correct_script_path <- e$correct_script_temp_path
      if(file.exists(correct_script_path)) {
        # Get contents of the correct script
        e$script_contents <- readLines(correct_script_path, warn = FALSE)
        # Save expr to e
        e$expr <- try(parse(text = e$script_contents), silent = TRUE)
        # Source the correct script
        try(source(correct_script_path))
        # Inform the user and open the correct script
        swirl_out("I just sourced the following script, which demonstrates one possible solution.",
                  skip_after=TRUE)
        file.edit(correct_script_path)
        readline("Press Enter when you are ready to continue...")
      }
      
    # If this is not a script question...
    } else {
      # In case correctAns refers to newVar, add it
      # to the official list AND the global environment
      if(exists("newVarName",e)) {
        correctAns <- gsub("newVar", e$newVarName, correctAns)
      }
      e$expr <- parse(text=correctAns)[[1]]
      ce <- cleanEnv(e$snapshot)
      e$val <- suppressMessages(suppressWarnings(eval(e$expr, ce)))
      xfer(ce, globalenv())
      ce <- as.list(ce)
      
      # Inform the user and expose the correct answer
      swirl_out("Entering the following correct answer for you...",
                skip_after=TRUE)
      message("> ", e$current.row[, "CorrectAnswer"])
      
    }
    
    # Make sure playing flag is off since user skipped
    e$playing <- FALSE
    
  # If the user is not trying to skip and is playing, 
  # ignore console input, but remain in operation.
  } else if(exists("playing", envir=e, inherits=FALSE) && e$playing) {
    esc_flag <- FALSE
    return(TRUE)
  }    
  
  # If the user want to return to the main menu, do the bookkeeping
  if(uses_func("main")(e$expr)[[1]]){
    swirl_out("Returning to the main menu...")
    # Remove the current lesson. Progress has been saved already.
    if(exists("les", e, inherits=FALSE)){
      rm("les", envir=e, inherits=FALSE)
    }
  }
  
  # If user is looking up a help file, ignore their input
  # unless the correct answer involves do so
  if(uses_func("help")(e$expr)[[1]] || 
       uses_func("`?`")(e$expr)[[1]]){
    # Get current correct answer
    corrans <- e$current.row[, "CorrectAnswer"]
    # Parse the correct answer
    corrans_parsed <- parse(text = corrans)
    # See if it contains ? or help
    uses_help <- uses_func("help")(corrans_parsed)[[1]] ||
      uses_func("`?`")(corrans_parsed)[[1]]
    if(!uses_help) {
      esc_flag <- FALSE
      return(TRUE)
    }
  }
  
  # Method menu initializes or reinitializes e if necessary.
  temp <- mainMenu(e)
  # If menu returns FALSE, the user wants to exit.
  if(is.logical(temp) && !isTRUE(temp)){
    swirl_out("Leaving swirl now. Type swirl() to resume.", skip_after=TRUE)
    esc_flag <- FALSE # To supress double notification
    return(FALSE)
  }
  
  # if e$expr is NOT swirl() or nxt(), the user has just responded to
  # a question at the command line. Simulate evaluation of the
  # user's expression and save any variables changed or created
  # in e$delta. 
  # TODO: Eventually make auto-detection of new variables an option.
  # Currently it can be set in customTests.R
  if(!uses_func("swirl")(e$expr)[[1]] &&
       !uses_func("swirlify")(e$expr)[[1]] &&
       !uses_func("testit")(e$expr)[[1]] &&
       !uses_func("nxt")(e$expr)[[1]] &&
       isTRUE(customTests$AUTO_DETECT_NEWVAR)) {
    e$delta <- mergeLists(safeEval(e$expr, e), e$delta)
  }
  # Execute instructions until a return to the prompt is necessary
  #GD: add token.list
  while(!e$prompt){
    # If the lesson is complete, save progress, remove the current
    # lesson from e, and invoke the top level menu method.
    # Below, min() ignores e$test_to if it is NULL (i.e. not in 'test' mode)
    if(e$row > min(nrow(e$les), e$test_to)) {
      # If in test mode, we don't want to run another lesson
      if(is(e, "test")) {
        swirl_out("Lesson complete! Exiting swirl now...",
                  skip_after=TRUE)
        esc_flag <- FALSE # to supress double notification
        return(FALSE)
      }
      saveProgress(e)
      # form a new path for the progress file
      # which indicates completion and doesn't
      # fit the regex pattern "[.]rda$" i.e.
      # doesn't end in .rda, hence won't be
      # recognized as an active progress file.
      new_path <- paste(e$progress,".done", sep="")
      # rename the progress file to indicate completion
      if(file.exists(new_path))file.remove(new_path)
      file.rename(e$progress, new_path)
      # Coursera check
      courseraCheck(e)
      # remove the current lesson and any custom tests
      if(exists("les", e, inherits=FALSE)){
        rm("les", envir=e, inherits=FALSE)
      }
      # Reset skip count if it exists
      if(exists("skips", e)) e$skips <- 0
      clearCustomTests()
      # Let user know lesson is complete
      swirl_out("You've reached the end of this lesson! Returning to the main menu...")
      # let the user select another course lesson
      temp <- mainMenu(e)
      # if menu returns FALSE, user wants to quit.
      if(is.logical(temp) && !isTRUE(temp)){
        swirl_out("Leaving swirl now. Type swirl() to resume.", skip_after=TRUE)
        esc_flag <- FALSE # to supress double notification
        return(FALSE)
      }
    }

    # If we are ready for a new row, prepare it
    if(e$iptr == 1){      
      # Increment progress bar
      cat("\n")
      setTxtProgressBar(e$pbar, e$pbar_seq[e$row])
      
      #  Any variables changed or created during the previous
      #  question must have been correct or we would not be about
      #  to advance to a new row. Incorporate these in the list
      #  of swirl's "official" names and values.
      if (!is.null(e$delta)){
        e$snapshot <- mergeLists(e$delta,e$snapshot)
      }
      e$delta <- list()
      saveProgress(e)

      cat("START ROW #: ", e$row, "\n")

      e$current.row <- e$les[e$row,]
  
      cat("repeat = ", e$current.row$NumTimes, "\n")

      # GD: generate token values if necessary
      tt = token.generate(e$current.row, e$token.list) 
      e$token.list <- tt$token.list
      e$current.row = tt$row

      # Prepend the row's swirl class to its class attribute
      class(e$current.row) <- c(e$current.row[,"Class"], 
                                       class(e$current.row))
    }

    save(e, file = "e.RData")
    # Execute the current instruction
    e$instr[[e$iptr]](e$current.row, e)
    # Check if a side effect, such as a sourced file, has changed the
    # values of any variables in the official list. If so, add them
    # to the list of changed variables.
    for(nm in names(e$snapshot)){
      if(exists(nm, globalenv()) &&
           !identical(e$snapshot[[nm]], get(nm, globalenv()))){
        e$delta[[nm]] <- get(nm, globalenv())
      }
    }
  }
  
  e$prompt <- FALSE
  esc_flag <- FALSE
  return(TRUE)
}

swirl <- function(resume.class="default", ...){

  token.list <- NULL
  # Creates an environment, e, defines a function, cb, and registers
  # cb as a callback with data argument, e. The callback retains a
  # reference to the environment in which it was created, environment(cb),
  # hence that environment, which also contains e, persists as long
  # as cb remains registered. Thus e can be used to store infomation
  # between invocations of cb.
  removeTaskCallback("mini")
  # e lives here, in the environment created when swirl() is run
  e <- new.env(globalenv())
  # This dummy object of class resume.class "tricks" the S3 system
  # into calling the proper resume method. We retain the "environment"
  # class so that as.list(e) works.
  class(e) <- c("environment", resume.class)
  # The callback also lives in the environment created when swirl()
  # is run and retains a reference to it. Because of this reference,
  # the environment which contains both e and cb() persists as
  # long as cb() remains registered.
  cb <- function(expr, val, ok, vis, data=e){
    # The following will modify the persistent e
    e$expr <- expr
    e$val <- val
    e$ok <- ok
    e$vis <- vis
    cat("==== cb, token list === \n")
    print(e$token.list) 
    cat("\n\nin cb, now resume ==== \n\n")
 
    # The result of resume() will determine whether the callback
    # remains active
    return(resume(e, ...))
  }
  cat("\n\n== add task to callback ==\n\n")
  addTaskCallback(cb, name="mini")
  invisible()
}


