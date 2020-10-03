#' @title Compute the standard deviation of the stochastic part of an AR1 process given the AR1 parameter and the total standard deviation
#' @description In statistics, it is often instructive to simulate observations according to a known data-generating process and then model simulated observations in different ways, to determine the extent to which simulated observations are recovered under different scenarios. To do this for a simple Gaussian model, the user might specify a formula for the linear predictor and a sigma parameter (i.e. the standard deviation among residuals) and then simulate observations from this model. However, the simulation of autocorrelated residuals with these desired properties is slightly more complex because the standard deviation of an autoregressive order 1 (AR1) process is affected by both the deterministic and stochastic components of an AR1 process, not just the stochastic process as is the case for models without autocorrelation. This function allows the user calculate the necessary standard deviation of the stochastic process, given the extent of autocorrelation and the desired sigma value. This is useful for (a) simulating autocorrelated observations with known parameters, either from a user-specified data-generating process or from a model (i.e. posterior simulation, of the kind implemented by \code{\link[Tools4ETS]{simulate_posterior_obs}}).
#'
#' @param AR1 A numeric input which defines the AR1 parameter. This may be simulated or estimated.
#' @param sigma A numeric input which defines the standard deviation of the AR1 process. This could be a residual standard deviation (also misnomed also 'residual standard error', e.g., in \code{\link[stats]{summary.lm}}'s output, from a fitted model). This may be simulated or estimated (see also \code{\link[stats]{sigma}}).
#'
#' @returns The function returns a number which defines the SD parameter of the stochastic part of an AR1 process necessary to reproduce the inputted residual standard deviation (\code{sigma}).
#'
#' @examples
#' #### Simulate some values
#' # We'll simulate a reasonably large number of values to generate
#' # ... quite accurate estimates in the examples below
#' # ... but not so many as to make computation time too slow.
#' set.seed(1)
#' nobs <- 1e3
#' x <- runif(nobs, 0, 1000)
#' d <- data.frame(x = x)
#' # simulate expected values:
#' d$mu <- 0.001 * d$x^2
#'
#' #### An explanation: The SD of an AR1 process is different from the SD
#' # ... of the outcome of the process
#' # Define AR1 parameter
#' AR1_sim <- 0.8
#' # Define SD
#' sd_arima <- 100
#' # Compute autocorrelated observations
#' arima_sim1 <- arima.sim(list(order = c(1,0,0), ar = AR1_sim), n = nrow(d), sd = sd_arima)
#' # Compare SD of arima_sim1 (essentially the 'residual standard deviation' here, since
#' # ... we have not specified any explanatory variables, see below) to that simulated
#' stats::sd(arima_sim1); sd_arima
#' # We can see that the SD of the response is different from the simulated SD
#' # This is because the SD in arima.sim() is the SD of the white noise
#' # The SD of the process is affected by the AR1 parameter
#' # The sigma_arima() function provides a method to flick from the former to the latter.
#'
#' #### Use (1): (A) Compute a suitable sigma to simulate data with known residual standard error
#' # Define simulated AR1 parameter
#' AR1_sim <- 0.8
#' # Define desired sigma
#' sigma <- 100
#' # Compute corresponding sigma in white noise component of AR1 process
#' sd_arima <- sigma_arima(AR1 = AR1_sim, sigma = sigma)
#' # Use this sd_sigma parameter to simulate an AR1 process:
#' arima_sim2 <- arima.sim(list(order = c(1,0,0), ar = AR1_sim), n = nrow(d), sd = sd_arima)
#' # Compare the desired sigma parameter with standard deviation of the AR1 process:
#' stats::sd(arima_sim2); sigma
#' # We can see that the two values are very similar.
#' # These are very similar; this difference diminishes with sample size.
#' # We can show this by simulating observations of different sample sizes.
#' # You can use the code below to explore this by running simulations in parallel
#' # ... but we won't do that here because it takes a few minutes to run.
#' \dontrun{
#' nseq <- seq(100, 1e6, by = 100)
#' cl <- parallel::makeCluster(10L)
#' investigate_n <-
#'   pbapply::pblapply(nseq, cl = cl, function(n){
#'     # sigma is 100
#'     # this leads to an arima_sd of 60
#'     arima_sim <- arima.sim(list(order = c(1,0,0), ar = 0.8), n = n, sd = 60)
#'     diff <- stats::sd(arima_sim) - 100
#'     return(diff)
#'   })
#' parallel::stopCluster(cl)
#' # Plot the difference in sigmas as a function of sample size:
#' plot(nseq,nseq,
#'      type = "n",
#'      ylim = c(-20, 20),
#'      xlab = "The sample size (n)",
#'      ylab = "The difference in sigmas",
#'      main = "The difference in sigmas declines with sample size")
#' points(nseq, as.vector(unlist(investigate_n)), type = "l")
#' abline(h = 0, col = "red")
#' }
#'
#' #### Use (2) (A): For an AR1 model, compare a simulated and inferred residual standard deviation
#' # The desired sigma represents the residual standard deviation ('residuals SE'),
#' # ... (i.e., the variation around a model's expected values). Thus, sigma_arima()
#' # ... can be used to ensure the simulated residuals have the desired sigma value.
#' # Simulate some observations:
#' d$yAR.2 <- d$mu + arima.sim(list(order = c(1,0,0), ar = AR1_sim), n = nrow(d), sd = sd_arima)
#' # Now let's implement a bam() model with the correct AR1 parameter value
#' # ... (here, we know the true value, so for speed we'll use that; in reality, one method
#' # ... to estimate the value is shown in the following two hashed lines):
#' # mAR0 <- mgcv::bam(yAR.2 ~ s(x), data = d)
#' # AR1_bam <- stats::acf(stats::resid(mAR0))$acf[2]; AR1_bam
#' AR1_bam <- AR1_sim
#' mAR1 <- mgcv::bam(yAR.2 ~ s(x), rho = AR1_sim, data = d)
#' # The simulated SD and the estimated sigma are similar:
#' sigma; stats::sigma(mAR1)
#' # However, note that these are different from the overall variation in the response,
#' # ... which depends additively on the variation in mu and the variation of the AR1 process:
#' stats::sd(d$yAR.2)
#' sqrt(stats::var(d$mu) + sigma^2) # variance (not SDs) is additive
#'
#'
#' #### Use (2) (B): Compute the appropriate sigma from a model for posterior simulation
#' # ... of prediction intervals, accounting for an AR1 process.
#' # Simulate some new 'observed' values from a model, accounting for autocorrelation.
#' # This is the method that simulate_posterior_obs() used to simulate observed observations
#' # when type == "snapshot" for models with autocorrelation.
#' mu <- stats::fitted(mAR1)
#' sigma <- stats::sigma(mAR1)
#' sd_arima <- sigma_arima(AR1 = AR1_bam, sigma = sigma)
#' sim_obs <- mu + arima.sim(list(order = c(1,0,0), ar = AR1_bam), n = nrow(d), sd = sd_arima)
#' plot(d$x, d$yAR.2, cex = 0.5, col = "grey")
#' points(d$x, sim_obs, cex = 0.1, col = "red")
#' # Compare these results to a simulation of observed values which doesn't account for AR1 process:
#' sim_obs_AR0 <- stats::rnorm(nrow(d), mu, sigma)
#' points(d$x, sim_obs_AR0, cex = 0.1, col = "blue")
#' # It is difficult to see any clear difference from a single simulation, but simulate_posterior_obs()
#' # ... can be used to explore this across many simulations.
#'
#' #### Comment: in models without an AR(1) structure, arima.sim() and rnorm can return
#' # ... results with the same properties, but these are not identical:
#' set.seed(1)
#' arima_sim <- arima.sim(list(order = c(1,0,0), ar = 0), n = 1e3, rand.gen = rnorm, sd = 100)
#' rnorm_sim <- rnorm(100, 0, 100)
#' head(arima_sim); head(rnorm_sim)
#' sd(arima_sim); sd(rnorm_sim)
#' plot(density(arima_sim)); lines(density(rnorm_sim), col = "red")
#'
#' @author Edward Lavender
#' @export
#'

#############################################
#############################################
#### sigma_arima()

sigma_arima <-
  function(AR1,
           sigma){
    var_arima <- sigma^2 * (1 - AR1^2)
    sd_arima <- sqrt(var_arima)
    return(sd_arima)
  }

#### End of function.
#############################################
#############################################
