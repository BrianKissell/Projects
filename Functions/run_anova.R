# Create the anova function
run_anova <- function(data, formula){
  # Simple function which allows the pipe operator to work with pipe operators.
  aov(formula, data)
}