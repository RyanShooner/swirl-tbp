
## add rpt
info <- function(){
  swirl_out("When you are at the R prompt (>):")
  swirl_out("-- Typing skip() allows you to skip the current question.", skip_before=FALSE)
  swirl_out("-- Typing rpt() allows you to repeat the previous question.", skip_before=FALSE)
  swirl_out("-- Typing play() lets you experiment with R on your own; swirl will ignore what you do...", skip_before=FALSE)
  swirl_out("-- UNTIL you type nxt() which will regain swirl's attention.", skip_before=FALSE)
  swirl_out("-- Typing bye() causes swirl to exit. Your progress will be saved.", skip_before=FALSE)
  swirl_out("-- Typing main() returns you to swirl's main menu.", skip_before=FALSE)
  swirl_out("-- Typing info() displays these options again.", skip_before=FALSE, skip_after=TRUE)
  invisible()
}



