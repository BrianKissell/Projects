# Write a function to run a Logistic regression
run_logistic_regression <- function(data, formula){
  glm(formula, data, family = "binomial")
}

