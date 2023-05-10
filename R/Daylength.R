#' Day Length Calculation
#'
#' Calculate the length of the day in hours for a given day number and latitude.
#'
#' @param n An integer representing the day of the year (1-365).
#' @param phi A numeric value representing the latitude in degrees (-90 to 90).
#'
#' @return A numeric value representing the length of the day in hours.
#'
#' @examples
#' DayLength(80, 45) # calculates the day length in hours for March 21st at latitude 45 degrees
#'
#' @importFrom stats pi acos tan
#'
#' @export
DayLength <- function(n, phi) {

  dayangle <- (360 * (n - 1) / 365) * pi / 180
  dec <- (0.006918 - 0.399912 * cos(dayangle) + 0.070257 * sin(dayangle) - 0.006758 * cos(2 * dayangle)
          + 0.000907 * sin(2 * dayangle) - 0.002697 * cos(3 * dayangle) + 0.00148 * sin(3 * dayangle)) * 180 / pi

  L <- 2 / 15 * acos(tan(-phi * pi / 180) * tan(dec * pi / 180)) * 180 / pi
  return(L)
}
