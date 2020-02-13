utils::globalVariables(c("BWsigma", "sigma_data"))

listSigma <- function(sourcevar, origin, country="USA", use_SITC=FALSE) {
  return(lapply(sourcevar, sigma, origin=origin, country=country, use_SITC=use_SITC, give_avg=FALSE))
}