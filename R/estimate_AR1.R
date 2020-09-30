#######################################
#######################################
#### estimate_AR1()

#' @title Estimate the AR1 parameter from the autocorrelation function of a model's residuals
#' @description This function estimates the AR1 parameter from the autocorrelation function of a model's residuals. For models of a single timeseries, the AR1 parameter is the value of the autocorrelation function at lag = 1 (i.e., between sequential residuals). However, for multiple timeseries, this value may be misleading if the AR1 parameter differs among independent timeseries. In this case, the function calculates the AR1 parameter separately for each timeseries and returns a summary.
#' @param resid A numeric vector of residuals.
#' @param AR.start A logical variable, of the same length as \code{resid}, in which the first observation of each independent section of AR1 correlation is denoted \code{TRUE}.
#' @param verbose A logical input which defines whether or not to print messages to the console. If \code{verbose = TRUE} and AR.start is not \code{NULL}, the function also returns a plot of the density of AR1 parameter values across independent timeseries.
#' @return The function returns a number which represents the estimated AR1 parameter.
#'
#' @examples
#' #### Example (1): Simulate a single timeseries and identify AR1 parameter
#' # Simulation parameters
#' n <- 1000
#' AR1_sim <- 0.9
#' sd_arima <- 10
#' d1 <- data.frame(t = 1:n, fct = 1)
#' d1$AR.start <- FALSE
#' d1$AR.start[1] <- TRUE
#' # Simulate observations from AR1 process
#' d1$y <- as.numeric(stats::arima.sim(list(order = c(1,0,0), ar = AR1_sim),
#'                                     n = nrow(d1),
#'                                     sd = sd_arima))
#' # Model using bam() and use the residuals to estimate the AR1 parameter:
#' mAR0 <- mgcv::bam(y ~ s(t), data = d1)
#' estimate_AR1(stats::resid(mAR0), AR.start = NULL)
#'
#' #### Example (2): Simulate two timeseries with a single AR1 parameter:
#' # Simulate timeseries
#' d2 <- d1
#' d2$fct <- 2
#' d2$y <- as.numeric(arima.sim(list(order = c(1,0,0), ar = AR1_sim),
#'                              n = nrow(d1),
#'                              sd = sd_arima))
#' d <- rbind(d1, d2)
#' # Model using bam()
#' mAR0 <- mgcv::bam(y ~ s(t), data = d)
#' # Estimate AR1 for the whole set of residuals or as a mean for each set:
#' estimate_AR1(stats::resid(mAR0), AR.start = NULL)
#' estimate_AR1(stats::resid(mAR0), AR.start = d$AR.start)
#'
#' #### Example (3): Simulate two timeseries with two very different AR1 parameters:
#' # Simulate timeseries
#' d3 <- d1
#' d3$fct <- 3
#' d3$y <- as.numeric(arima.sim(list(order = c(1,0,0), ar = 0.2),
#'                               n = nrow(d1),
#'                               sd = sd_arima))
#' d <- rbind(d1, d3)
# Model using bam()
#' mAR0 <- mgcv::bam(y ~ s(t), data = d)
#' # Examine AR1 computed for whole timeseries or as a mean from each independent timeseries
#' # Both reflect a compromise between the lower and higher autocorrelation
#' # ... but the second output makes it clear that we have very different levels of autocorrelation
#' # ... in the timeseries that we may need to account for.
#' estimate_AR1(stats::resid(mAR0), AR.start = NULL)
#' estimate_AR1(stats::resid(mAR0), AR.start = d$AR.start)
#'
#' @author Edward Lavender
#' @export
#'

estimate_AR1 <- function(resid, AR.start = NULL, verbose = TRUE){
  extract_AR1_from_acf <- function(resid) return(stats::acf(resid, plot = FALSE)$acf[2])
  if(is.null(AR.start)){
    if(verbose) cat("AR.start is NULL: extracting AR1 from ACF. \n")
    rho <- extract_AR1_from_acf(resid)
  } else{
    if(verbose) cat("AR.start provided: calculating mean AR1 from each independent ACF. \n")
    pos_start <- which(AR.start)
    split_f <- rep(seq_along(pos_start), times = diff(c(pos_start, length(AR.start) + 1)))
    resid_ls <- split(resid, f = split_f)
    rhos <- sapply(resid_ls, function(r) return(extract_AR1_from_acf(r)))
    if(verbose) {
      cat("Summary statistics for AR1 values from each independent timeseries: \n")
      print(summary(rhos))
      prettyGraphics::pretty_plot(stats::density(rhos), type = "l", xlab = "AR1", ylab = "Density")
    }
    rho <- mean(rhos)
  }
  return(rho)
}

#### End of code.
#######################################
#######################################
