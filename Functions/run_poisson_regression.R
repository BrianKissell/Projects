# Write a function to run a Poisson regression
run_poisson_regression <- function(data, formula){
  glm(formula, data, family = "poisson")
}