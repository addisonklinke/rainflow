#' Minute by minute meteorological data from Cleveland, OH.
#' 
#' A dataset containing 6 months of irradiance, wind, temperature, relative humidity, 
#' and pressure readings.
#' 
#' @docType data
#' @usage data(weather)
#' 
#' @format A data frame with 156,631 rows and 8 variables:
#' \describe{
#'   \item{date}{ Date stamp, in format yyyy-mm-dd}
#'   \item{time}{ Time stamp, in format hh:mm}
#'   \item{ghi}{ Global horizontal irradiance, in watts per square meter}
#'   \item{windDir}{ Wind direction, in degrees relative to north}
#'   \item{windSpeed}{ Wind speed, in meters per second}
#'   \item{temp}{ Air temperature, in degrees celsius}
#'   \item{relHum}{ Relative humidity, as percentage}
#'   \item{press}{ Air pressure, in kilo Pascals}
#' }
#' @source Solar Durability and Lifetime Extension (SDLE) Research Center, Case Western
#' Reserve University
"weather"