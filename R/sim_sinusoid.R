#' @title Simulate and visualise sinusoidal waves with different parameters
#' @description This function is used to simulate and visualise sinusoidal waves with different parameters.
#' @param x A numeric vector.
#' @param f The sinusoidal function (i.e., sin or cos, see \code{\link[base]{Trig}}).
#' @param a A number which defines the amplitude (i.e., the height) of the wave.
#' @param b A number which defines the horizontal stretch parameter. Alternatively, \code{period} can be provided.
#' @param period A number which defines the period (i.e., the number of units between successive waves) of the wave. This is silently ignored if \code{b} is provided.
#' @param c A number which defines the horizontal shift parameter.
#' @param d A number which defines the vertical shift parameter (i.e., the mean).
#' @param plot A logical input which defines whether or not to plot the simulated wave. This is \code{TRUE} by default.
#' @param type A character which defines the type of plot to produce (see \code{\link[graphics]{plot.default}}). A line plot (\code{type = "l"}) is the default.
#' @param return A logical input which defines whether or not to return the values of the wave. This is \code{FALSE} by default.
#' @param ... Other arguments passed to \code{\link[prettyGraphics]{pretty_plot}} to customise the plot.
#' @details Sinusoidal waves are simulated according to the equation \eqn{a f(bx + c) + d} where the parameters are as defined above and \code{b} is related to \code{period} by \eqn{b = \frac{2 \pi}{period}}.
#' @examples
#' # Simulate some x values with observations every 0.1 units (e.g., 0.1 s)
#' x <- seq(0, 10, by = 0.1)
#' # Visualise a sine wave with a period of 2 s
#' sim_sinusoid(x, period = 10)
#' # Or, equivalently, a wave with b = (2*pi)/10
#' sim_sinusoid(x, b = (2*pi)/10)
#' # Adjust other parameters, such as the amplitude:
#' sim_sinusoid(x, period = 10, a = 10)
#' # Adjust the sinusoidal function via f
#' sim_sinusoid(x, f = cos, period = 10, a = 10)
#' @author Edward Lavender
#' @export

sim_sinusoid <-
  function(x,
           f = sin,
           a = 1,
           b = NULL,
           period = 2*pi,
           c = 0,
           d = 0,
           plot = TRUE, type = "l",
           return = FALSE,...){
    if(is.null(b)) b <- (2*pi)/period
    y <- a * f(b * x + c) + d
    if(plot) prettyGraphics::pretty_plot(x, y, type = type,...)
    if(return) return(y)
  }
