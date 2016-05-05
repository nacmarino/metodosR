# function to calculate overdispersion

overdisp <- function(deviance, df.residual) {
  deviance/df.residual
}