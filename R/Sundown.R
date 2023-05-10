#' Calculate the time of sundown for a given day and location
#'
#' This function calculates the time of sundown for a given day and location, based on the method described in "Astronomical Algorithms" by Jean Meeus.
#'
#' @param n An integer representing the day of the year (1 = January 1, 365 or 366 = December 31)
#' @param phi A numeric value representing the latitude of the location in degrees
#'
#' @return A numeric value representing the time of sundown in hours (e.g. 18.75 = 6:45 PM)
#'
#' @examples
#' Sundown(200, 52.5) # calculates the time of sundown for July 19th at 52.5 degrees north latitude
#'
#' @importFrom stats pi
#' @importFrom stats acos
#' @importFrom stats sin
#' @importFrom stats cos
#' @importFrom stats tan
#' @importFrom graphics legend
#'
#' @export
Sundown <- function(n,phi) {

  # calculate the day angle
  dayangle <- (360*(n-1)/365)*pi/180

  # calculate the declination angle
  dec <- (0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2*dayangle)
          +0.000907*sin(2*dayangle)-0.002697*cos(3*dayangle)+0.00148*sin(3*dayangle))*180/pi

  # calculate the longitude correction factor
  L <- 2/15*acos(tan(-phi*pi/180)*tan(dec*pi/180))*180/pi

  # calculate the time of sundown
  Sundown <- 12 + L/2 + 16/60 + 1

  # return the result
  return(Sundown)
}
