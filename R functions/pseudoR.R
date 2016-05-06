# function to calculate pseudo R2

pseudoR <- function(model_name){
  (model_name$null.deviance - model_name$deviance)/model_name$null.deviance
}