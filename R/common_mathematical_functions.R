##############################################
##############################################
#### common_mathematical_functions


##############################################
#### linear

#' @title A linear function
#' @description This function evaluates a linear function.
#' @param x A numeric vector which defines the values at which the linear function will be evaluated.
#' @param a A numeric input which defines the intercept.
#' @param b A numeric input which defines the gradient.
#'
#' @return A number or a numeric vector.
#'
#' @examples
#' linear(1:10, 5, 3)
#'
#' @author Edward Lavender
#' @export
#'

linear <- function(x, a, b){
  y <- a + b*x
  return(y)
}


##############################################
#### quadratic

#' @title A quadratic function
#' @description   This function evaluates a quadratic function.
#' @param x A numeric vector which defines the values at which the quadratic function will be evaluated.
#' @param a A numeric value which defines the vertical stretch of the function.
#' @param b A numeric value which defines the horizontal stretch of the function.
#' @param h The x coordinate of the vertex.
#' @param k The y coordinate of the vertex.
#'
#' @return A number or a numeric vector.
#'
#' @examples
#' quadratic(x = 1:10, a = 1, b = 1, h = 0.5, k = 0.25)
#'
#' @author Edward Lavender
#' @export
#'

quadratic <- function(x, a, b, h, k){
  y <- a * (b * x - h)^2 + k
  return(y)
}


##############################################
#### sigmoidal

#' @title A sigmoidal function
#' @description This function evaluates a sigmoidal function.
#' @param x A numeric vector which defines the values at which the sigmoidal function will be evaluated.
#' @param x0 A numeric value which defines the x-value of the sigmoid's midpoint
#' @param L A numeric value which defines expected change in f(x) above/below 0. For example, if \code{L = 2}, then the function will stretch ± 1 units above/below 0 at f(0) = 0.
#' @param k A numeric value which affects the steepness of the function.
#'
#' @return A number or a numeric vector.
#'
#' @examples
#'
#' x <- 1:10
#' plot(x, sigmoid(x, 5, L = 5, k = 1), type = "b")
#' abline(h = 0, lty = 3)
#'
#' x <- 0:20
#' plot(x, sigmoid(x, 5, L = 2, k = 1), type = "b")
#' abline(h = 0, lty = 3)
#'
#' x <- -60:60
#' plot(x, sigmoid(x, 0, L = 2, k = 0.1), type = "b")
#' abline(h = 0, lty = 3)
#'
#' @author Edward Lavender
#' @export
#'

sigmoid <- function(x, x0, L, k){
  # note the minus L/2
  y <- L/(1 + exp(-k * (x - x0))) - L/2
  return(y)
}



#### End of code.
##############################################
##############################################
