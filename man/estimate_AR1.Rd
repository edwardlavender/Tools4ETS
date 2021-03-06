% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_AR1.R
\name{estimate_AR1}
\alias{estimate_AR1}
\title{Estimate the AR1 parameter from the autocorrelation function of a model's residuals}
\usage{
estimate_AR1(resid, AR.start = NULL, verbose = TRUE)
}
\arguments{
\item{resid}{A numeric vector of residuals.}

\item{AR.start}{A logical variable, of the same length as \code{resid}, in which the first observation of each independent section of AR1 correlation is denoted \code{TRUE}.}

\item{verbose}{A logical input which defines whether or not to print messages to the console. If \code{verbose = TRUE} and AR.start is not \code{NULL}, the function also returns a plot of the density of AR1 parameter values across independent time series.}
}
\value{
The function returns a number which represents the estimated AR1 parameter.
}
\description{
This function estimates the AR1 parameter from the autocorrelation function of a model's residuals. For models of a single time series, the AR1 parameter is the value of the autocorrelation function at lag = 1 (i.e., between sequential residuals). However, for multiple time series, this value may be misleading if the AR1 parameter differs among independent time series. In this case, the function calculates the AR1 parameter separately for each time series and returns a summary.
}
\examples{
#### Example (1): Simulate a single time series and identify AR1 parameter
# Simulation parameters
n <- 1000
AR1_sim <- 0.9
sd_arima <- 10
d1 <- data.frame(t = 1:n, fct = 1)
d1$AR.start <- FALSE
d1$AR.start[1] <- TRUE
# Simulate observations from AR1 process
d1$y <- as.numeric(stats::arima.sim(list(order = c(1,0,0), ar = AR1_sim),
                                    n = nrow(d1),
                                    sd = sd_arima))
# Model using bam() and use the residuals to estimate the AR1 parameter:
mAR0 <- mgcv::bam(y ~ s(t), data = d1)
estimate_AR1(stats::resid(mAR0), AR.start = NULL)

#### Example (2): Simulate two time series with a single AR1 parameter:
# Simulate time series
d2 <- d1
d2$fct <- 2
d2$y <- as.numeric(arima.sim(list(order = c(1,0,0), ar = AR1_sim),
                             n = nrow(d1),
                             sd = sd_arima))
d <- rbind(d1, d2)
# Model using bam()
mAR0 <- mgcv::bam(y ~ s(t), data = d)
# Estimate AR1 for the whole set of residuals or as a mean for each set:
estimate_AR1(stats::resid(mAR0), AR.start = NULL)
estimate_AR1(stats::resid(mAR0), AR.start = d$AR.start)

#### Example (3): Simulate two time series with two very different AR1 parameters:
# Simulate time series
d3 <- d1
d3$fct <- 3
d3$y <- as.numeric(arima.sim(list(order = c(1,0,0), ar = 0.2),
                              n = nrow(d1),
                              sd = sd_arima))
d <- rbind(d1, d3)
mAR0 <- mgcv::bam(y ~ s(t), data = d)
# Examine AR1 computed for whole time series or as a mean from each independent time series
# Both reflect a compromise between the lower and higher autocorrelation
# ... but the second output makes it clear that we have very different levels of autocorrelation
# ... in the time series that we may need to account for.
estimate_AR1(stats::resid(mAR0), AR.start = NULL)
estimate_AR1(stats::resid(mAR0), AR.start = d$AR.start)

}
\author{
Edward Lavender
}
