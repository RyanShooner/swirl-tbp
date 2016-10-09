waitUser.default <- function(current.row, e){
  r = readline("...")
  e$row <- 1 + e$row
  e$iptr <- 1
  r
}

waitUser.multi_cmd_question <-function(current.row, e) {
 e$prompt <- TRUE
 # Enter 'play' mode so that user can mess around in the console
 e$playing <- TRUE
 # Advance lesson
 e$iptr <- 1 + e$iptr
}
