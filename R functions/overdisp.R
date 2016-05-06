# function to calculate overdispersion
# you just have to enter the object corresponding to the model ("model_name") and the function returns the overdisperion factor

overdisp <- function(model_name) {
  model_name$deviance/model_name$df.residual
}