#' Compute solar elevation angles for a year
#'
#' This function calculates the solar elevation angles for each day of a year for a given latitude.
#'
#' @param phi A numeric value representing the latitude in degrees.
#'
#' @return A numeric vector of length 365 representing the solar elevation angles in degrees.
#'
#' @examples
#' solarElev(49.01)
#'
#' @importFrom stats pi
#'
#' @export
solarElev <- function(phi) {

  result <- c()

  for (n in 1:365) {
    dayangle <- (360*(n-1)/365)*pi/180
    dec <- (0.006918-0.399912*cos(dayangle)+0.070257*sin(dayangle)-0.006758*cos(2*dayangle)
            +0.000907*sin(2*dayangle)-0.002697*cos(3*dayangle)+0.00148*sin(3*dayangle))*180/pi

    h <- 90 - phi + dec

    result[n] <- h
  }

  return(result)
}
