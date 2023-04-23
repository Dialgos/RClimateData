#' Plot Two Vectors
#'
#' This function plots two vectors against each other.
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param title The title of the plot.
#' @param xlab The label for the x-axis.
#' @param ylab The label for the y-axis.
#' @param linecolor The color of the plotted line.
#' @param linenames A character vector of length 2 containing the names of the lines to be plotted.
#'
#' @return A plot of the two vectors with a legend.
#'
#' @examples
#' StandardXYPlot(c(1, 2, 3), c(4, 5, 6), "My Plot", "X Axis", "Y Axis", "blue", c("Line 1", "Line 2"))
#'
#' @export
#'


StandardXYPlot <- function(x, y, title, xlab, ylab, linecolor, linenames) {
  plot(x, y, col = linecolor, type = "l", lwd = 2, xlab = xlab, ylab = ylab, main = title)
  legend("topright", legend = linenames, col = linecolor, lwd = 2)
}
