# will be called at the start of lesson
# can use to specify token functions
gen.var <-function() {
  vars = c("x", "y", "v", "num", "X", "Y")
  sample(vars, 1)
}
