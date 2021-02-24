# Create the linear regression function
run_linear_regression <- function(data, formula){
  #This is a simple function which allows the lm function to work with pipe operators.
  lm(formula, data)
}


