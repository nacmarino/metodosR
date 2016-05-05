# function to calculate pseudo R2

pseudoR <- function(null.deviance, deviance){
  (null.deviance - deviance)/null.deviance
}