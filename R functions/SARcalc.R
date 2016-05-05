#' @title SARcalc
#' 
#' Function to create data relating the number of species to the area, based on the well know formulae:
#' log(S) = log(c) + z * log(A)
#' 
#' This function must be used for educational purposes only, for obvious reasons.
#'
#' @param seed numeric, the number used to set.seed for the random number generator
#' @param size numeric, the number of observations you want to create
#' @param meanlog numeric, the value of the meanlog of the 'rlnorm' function - the mean of the lognormal distribution on the log scale
#' @param sdlog numeric, the value of the sdlog of the 'rlnorm' function - the standard deviation of the lognormal distribution on the log scale
#' @param z numeric, the desired value of z from the species-area relationship - this value represents the regression slope of the relationship
#' @param c numeric, the desired value of c from the species-area relationship - this value is the intercept of the relationship and should be provided in the log scale
#' @param noise numeric, this will be the value of the standard deviation of a random normal variable, created to add some noise to the estimated value of the area from the species-area equation. This is done so that the relationship is not 'perfect' for the educational purposes
#' @param ID a number or character that will be added to the output, as a way to provide a factor level to it
#' 
#' @return A data frame with three columns: the richness, area and island type
#' 
#' @author Nicholas A. C. Marino
#

SARcalc <- function(seed, size, meanlog, sdlog, z, c, noise, ID) {
  # criando um vetor com a riqueza de espécies, a partir de um distribuição log normal
  set.seed(seed)
  log_riqueza <- rlnorm(n = size, meanlog = meanlog, sdlog = sdlog)
  
  # area calculada a partir dos valores de riqueza, z e c e adição de um ruído
  log_area <- ((log_riqueza - c)/z) + rnorm(n = size, sd = noise)
  
  data.frame(riqueza = round(10^log_riqueza), area = 10^log_area, ilha = rep(x = ID, times = size))
}
