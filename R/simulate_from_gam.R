##############################################
##############################################
#### simulate_posterior_obs()

#' @title Simulate 'observed' values and prediction uncertainty from a Gaussian \code{mgcv::bam()} or \code{mgcv::gam()} model
#' @description  This function enables the user to simulate new 'observed' values from an Gaussian \code{\link[mgcv]{bam}} or \code{\link[mgcv]{gam}} model, including for models with an AR1 structure. There are three main options. The first option is for the user to simulate a single realisation from a model. Under this scenario (\code{type = "snapshot"}), the function samples a vector of observed values from a Gaussian distribution with a mean vector equal to the expected values and a standard deviation equal to the estimated sigma parameter (i.e. the standard deviation of residuals), accounting for autocorrelation in the sampling process if necessary. One reason as to why this is useful is to investigate whether the estimate of the residual standard deviation is appropriate: while the simulated timeseries will not reproduce the observed timeseries, the simulated timeseries should approximately reproduce the observed variation in the timeseries. However, this method only provides a single 'snapshot' realisation of a model, which does not capture uncertainty or calculate prediction intervals. When \code{type = "envelope"}, the function calculates the posterior distribution of predicted values in a two stage process which accounts for (a) uncertainty in the mean (possibly via uncertainty in the underlying fitted coefficients, see below) and (b) uncertainty in the predictions due to variation around expected values. To calculate a posterior distribution for the mean (i.e., expected values), there are two methods. Under \code{mu_method = 1}, the assumption is that the uncertainty in the expected values can be characterised by a Gaussian distribution. For each observation, \code{n} possible mean values are sampled from a Gaussian distribution with a mean equal to the expected value for that observation and a SD equal to the estimated SE around that mean for that observation. \code{mu_method = 2} is more sophisticated. Under \code{mu_method = 2}, uncertainty in the fitted coefficients is characterised by a multivariate normal distribution, with mean vector given by fitted coefficients and a covariance matrix equal to their (Bayesian) covariance matrix. For each observation, the function then samples \code{n} new fitted coefficient vectors from this multivariate distribution. Each sample is combined with the linear predictor matrix to generate a single simulated mean for each observation. This method is implemented by \code{\link[Tools4ETS]{simulate_posterior_mu}}(). With a posterior distribution for expected values, the function then samples \code{n} new 'observed' values using expected values, accounting for autocorrelation if necessary. The function returns the posterior distribution of simulated observed values, either in full (i.e., a matrix with a row for each observation and \code{n} realised values for new predicted values for that observation), a summary computed by \code{\link[Tools4ETS]{summarise_posterior}} (i.e., for each observation, the mean of the posterior distribution and a lower and upper quantile), or both.
#'
#' @param model A model from which to simulate (see \code{\link[mgcv]{predict.gam}}).
#' @param newdata A dataframe from which to make predictions (see \code{\link[mgcv]{predict.gam}}).
#' @param rho A numeric value which defines the \code{rho} value from an \code{\link[mgcv]{predict.gam}} model. For models without an AR1 structure, the default value (\code{rho = 0}) is appropriate.
#' @param ind A vector, of the same length as the number of rows in \code{newdata}, which defines independent sections of timeseries.
#' @param type A character input which defines the type of simulation. Options are: (1) \code{"snapshot"}, which returns a single realisation of the model; or (b) \code{"envelope"}, which performs multiple simulations and returns a posterior distribution of outcomes.
#' @param mu_method A numeric input (\code{1} or \code{2}) which defines the method used to simulate the mean, as described in the function Description. \code{mu_method = 2} is recommended.
#' @param seed A numeric value to set the seed.
#' @param n  A numeric value which defines the number of simulations.
#' @param return A numeric vector which defines the quantiles of the posterior distribution to return as the lower and upper confidence intervals, if \code{return = "summary"} or \code{return = "both"} (see \code{\link[Tools4ETS]{summarise_posterior}}).
#' @param probs A numeric vector which defines the quantiles of the posterior distribution to return as the lower and upper confidence intervals, if \code{return = "summary"} or \code{return = "both"} (see \code{\link[Tools4ETS]{summarise_posterior}}).
#' @param summary_format  A character input which defines the format of the summary distribution, if \code{return = "summary"} or \code{return = "both"} (see \code{\link[Tools4ETS]{summarise_posterior}}).
#'
#' @details The function makes two assumptions which may cause a slight underestimate in the 'true' uncertainty: both the model's \code{sigma} and, if applicable, the inputted \code{rho} are fixed (i.e. assumed to be known). These parameters could also be treated as uncertain, with their uncertainty characterised by probability distributions.
#'
#' @return The function returns a posterior distribution matrix for predictions, a summary of these predictions (as a list or matrix, see above).
#'
#' @examples
#'
#' #####################################################
#' #### Simulate some data and fit a GAM
#'
#' set.seed(1)
#' nobs <- 100
#' x <- stats::runif(nobs, 0, 1000)
#' mu <- 0.001 * x^2
#' y <- rnorm(nobs, mu, 100)
#' d <- data.frame(x = x, y = y)
#' m1 <- mgcv::gam(y ~ s(x), data = d)
#'
#' #### We can use simulate_posterior_mu to add a fitted line and CIs from posterior simulation
#' # ... This shows our confidence in the fitted line; but what about our confidence in predictions?
#' nd <- data.frame(x = seq(min(d$x), max(d$x), length.out = 100))
#' sim_mu <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = nd,
#'     n = 1000,
#'     return = "summary")
#' plot(d$x, d$y)
#' names(sim_mu)[1] <- "fit"
#' prettyGraphics::add_model_predictions(x = nd$x, CI = sim_mu)
#'
#'
#' #####################################################
#' #### Simulate 'snapshots' of 'observed' values
#'
#' #### Example (1): Simulate a single realisation of 'observed values'
#' sim_pos1 <-
#'   simulate_posterior_obs(
#'     model = m1,
#'     newdata = d,
#'     rho = 0,
#'     type = "snapshot")
#' # The function returns a dataframe with 'fit', 'se_fit' and 'obs_sim'.
#' # The function calculates fitted values and SEs from the model and then, assuming a Gaussian model,
#' # ... samples a single realisation of 'observed' values from a Gaussian distribution with a mean
#' # ... equal to the fitted values and an SD equal to the esimated SD
#' # ... 'obs_sim' are the simulated observed values.
#' utils::str(sim_pos1)
#' points(d$x, sim_pos1$obs_sim, col = "red")
#'
#'
#' #### Example (2): The function can simulate realisations accounting for autocorrelation:
#' # simulate some autocorrelated observations following an AR1 structure
#' d$yAR <- mu + arima.sim(list(order = c(1,0,0), ar = 0.95), n = nrow(d), sd = sigma_arima(0.95, 100))
#' # use mgcv::bam() to estimate the AR1 parameter and
#' # ... then implement a model accounting for autocorrelation
#' # with sufficient data here, the AR1 estimated using the ACF is very accurate:
#' mAR0 <- mgcv::bam(yAR ~ s(x), data = d)
#' AR1 <- stats::acf(stats::resid(mAR0))$acf[2]; AR1
#' mAR1 <- mgcv::bam(yAR ~ s(x), rho = AR1, data = d)
#' # mAR1 model effectively captured autocorrelation:
#' stats::acf(mAR1$std.rsd)$acf[2];
#' # Note that we've simulated data such that the sigma of a model with/without
#' # ... autocorrelation is the same
#' stats::sigma(m1); stats::sigma(mAR1)
#' # Simulate a single realised sequence of 'observations from this model,
#' # ... accounting for the estimated AR1 parameter
#' # ... (This is assumed to be fixed.)
#' sim_pos2 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = d,
#'     rho = AR1,
#'     ind = NULL,
#'    type = "snapshot")
#' # Examine a timeseries of uncorrelated observations, correlated observations and
#' # ... simulated observations which include autocorrelation:
#' d$index <- 1:nrow(d)
#' pp <- par(mfrow = c(2, 3))
#' # Initially simulated y values shouldn't show evidence of autocorrelation,
#' # ... while simularted yAR values/observations from the model should show autocorrelation
#' plot(d$index[1:100], d$y[1:100], type = "l")
#' plot(d$index[1:100], d$yAR[1:100], type = "l")
#' plot(d$index[1:100], sim_pos2$obs_sim[1:100], type = "l")
#' stats::acf(stats::resid(mgcv::bam(y ~ s(d$x))))$acf[2]
#' stats::acf(stats::resid(mgcv::bam(sim_pos2$obs_sim ~ s(d$x))))$acf[2]
#' par(pp)
#'
#'
#' #####################################################
#' #### Use simulation to create prediction intervals
#'
#' #### Introduction
#' # The above approach is useful in quickly demonstrating how
#' # ... well our model captures the observed variation, but it only represents
#' # ... a rough-and-ready 'snapshot' of predictions. Often, we want a prediction interval instead,
#' # ... computed across many simulations.
#'
#' #### Example (3): Simulate the full posterior distribution using mu_method = 1
#' # When mu_method = 1, the expected value, on the scale of the response, is assumed to be normally
#' # ... distributed around the true value. For each observation, n simulated values for the expected
#' # ... value are drawn. Then, for each one of those, an 'observed' value is simulated from
#' # ... the expected value (accounting for autocorrelation).
#' sim_pos3 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = d,
#'     rho = AR1,
#'     ind = NULL,
#'     type = "envelope",
#'     mu_method = 1,
#'     n = 250,
#'     return = "full")
#' # The function returns the full posterior matrix, i.e., a matrix
#' # ... in which the rows refer to observations and each column
#' # ... stored an observed value from one simulation.
#' utils::str(sim_pos3)
#'
#' #### Example (4): The full posterior distribution using mu_method = 2
#' # When mu_method = 2, the expected value, on the scale of the response, is simulated using
#' # ... simulate_posterior_mu(). This approach treats fitted cofficients as realisations
#' # ... of a multivariate normal distribution. New coefficients are sampled from this distribution,
#' # ... and combined with the linear predictor matrix to generate expected values. As above,
#' # ... observed values are then simulated from expected values.
#' sim_pos4 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = d,
#'     rho = AR1,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "full")
#' utils::str(sim_pos4)
#'
#'
#' #### Example (5): The full posterior distribution can be summarised to create a prediction interval
#' # ... from either mu_method = 1 or mu_method = 2
#' # ... by defining 'return = "summary" and, optionally, arguments passed to summarise_posterior()
#' # ... i.e., the CIs and the summary_format.
#' # Simulate prediction intervals accounting for AR using mu_method 2:
#' sim_pos5 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = nd, # predict from a regular sequence of values to add CIs
#'     rho = AR1,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "summary") # return a summary (default returns a list of 95% CIs which can be plotted)
#' # The function returns a list, with three elements that can be plotted using add_model_predictions()
#' utils::str(sim_pos5)
#' # Define a plot, using pretty_axis() to generate suitable axes that account for predictions:
#' names(sim_pos5)[1] <- "fit"
#' axis_ls <-
#'   prettyGraphics::pretty_axis(side = 1:2,
#'                            x = list(range(d$x),
#'                                     range(c(sim_pos5$fit,
#'                                             sim_pos5$lowerCI,
#'                                             sim_pos5$upperCI)
#'                                             )
#'                                           ),
#'                            pretty = list(n = 5), add = FALSE)
#' # Create plot and add axes and prediction intervals
#' plot(d$x, d$yAR, xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim, axes = FALSE)
#' prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)
#' prettyGraphics::add_model_predictions(nd$x, sim_pos5)
#' # Compare prediction interval to confidence interval for mu,
#' # ... which is much narrower:
#' sim_mu_AR1 <-
#'   simulate_posterior_mu(
#'     model = mAR1,
#'     newdata = nd,
#'     n = 250,
#'     return = "summary")
#' names(sim_mu_AR1)[1] <- "fit"
#' prettyGraphics::add_model_predictions(x = nd$x, CI = sim_mu_AR1,
#'                                   CI_gp = list(col = "dimgrey",
#'                                                 border = FALSE))
#'
#' #### Examine the effects of accounting for autocorrelation on CIs and prediction intervals
#'
#' # Compare prediction interval accounting for autocorrelation to one which
#' # ... doesn't account for autocorrelation. The interval which does not include
#' # ... autocorrelation is similar in this case.
#' # Simulate the full posterior prediction matrix from model mAR1 with/without accounting rho:
#' sim_pos6_AR0 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = nd,
#'     rho = 0,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "full")
#' sim_pos6_AR1 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = nd,
#'     rho = AR1,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "full")
#' # For a single simulation scenario, examine the ACF
#' # This shows that the residuals are not correleted under the first scenario,
#' # ... but they are correlated under the second scenario in which observations were simulated
#' # ... with an AR1 process:
#' sim_pos6_mAR0 <- mgcv::bam(sim_pos6_AR0[1:100, 1] ~ s(nd$x))
#' sim_pos6_mAR1 <- mgcv::bam(sim_pos6_AR1[1:100, 1] ~ s(nd$x))
#' pp <- par(mfrow = c(1, 2))
#' stats::acf(stats::resid(sim_pos6_mAR0))
#' sim_pos6_rho <- stats::acf(stats::resid(sim_pos6_mAR1))$acf[2]
#' par(pp)
#' # Accounting for autocorrelation makes a difference for confidence intervals.
#' # Usually, the effect is to make CIs wider, but interestingly CIs are narrower for the
#' # ... model with the AR1 model in this case:
#' pp <- par(mfrow = c(1, 2))
#' mgcv::plot.gam(mAR0)
#' mgcv::plot.gam(mAR1)
#' par(pp)
#'
#' # What about for prediction intervals?
#' # summarise intervals
#' sim_pos6_AR0_PI <- summarise_posterior(sim_pos6_AR0)
#' sim_pos6_AR1_PI <- summarise_posterior(sim_pos6_AR1)
#' names(sim_pos6_AR0_PI)[1] <- "fit"
#' names(sim_pos6_AR1_PI)[1] <- "fit"
#' # Plot
#' plot(d$x, d$yAR, cex = 0.1, col = "grey")
#' prettyGraphics::add_model_predictions(nd$x, sim_pos6_AR0_PI,
#'                                    CI_gp = list(col = "red",
#'                                                 border = FALSE),
#'                                    add_fitted = FALSE
#' )
#' prettyGraphics::add_model_predictions(nd$x, sim_pos6_AR1_PI,
#'                                   CI_gp = list(col = "blue",
#'                                                 border = FALSE),
#'                                    add_fitted = FALSE
#' )
#' # Prediction intervals are very similar. This is because the overall residual variation
#' # ... is similar inm both cases, which we can see if we compare the two original models:
#' stats::sigma(mAR0); stats::sigma(mAR1)
#' # The difference is that in the AR model, the residual variation
#' # ... is not all random, but some is 'deterministic'. This matters for uncertainty in the mean,
#' # ... but, once you've accounted for this, the observed variation around the mean is similar.
#'
#' #### Example (6) Comparison of mu_method = 1 and mu_method = 2.
#' # Implement posterior simulation:
#' sim_pos7_mu1 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = nd,
#'     rho = AR1,
#'     type = "envelope",
#'     mu_method = 1,
#'     n = 250,
#'     return = "summary")
#' sim_pos7_mu2 <-
#'   simulate_posterior_obs(
#'     model = mAR1,
#'     newdata = nd,
#'     rho = AR1,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "summary")
#' # Define axes:
#' axis_ls <-
#'   prettyGraphics::pretty_axis(side = 1:2,
#'                            x = list(range(d$x),
#'                                     range(
#'                                       c(sim_pos7_mu1$fit,
#'                                         sim_pos7_mu1$lowerCI,
#'                                         sim_pos7_mu1$upperCI,
#'                                         sim_pos7_mu2$fit,
#'                                         sim_pos7_mu2$lowerCI,
#'                                         sim_pos7_mu2$upperCI))
#'                                     ),
#'                            pretty = list(n = 5), add = FALSE)
#' # Plot prediction intervals:
#' plot(d$x, d$yAR,
#'      xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim,
#'      axes = FALSE, cex = 0.1,
#'      col = "grey")
#' prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)
#' names(sim_pos7_mu1)[1] <- "fit"
#' names(sim_pos7_mu2)[1] <- "fit"
#' prettyGraphics::add_model_predictions(nd$x, sim_pos7_mu1,
#'                                   CI_gp = list(col = "red", border = FALSE))
#' prettyGraphics::add_model_predictions(nd$x, sim_pos7_mu2,
#'                                    CI_gp = list(col = "skyblue",
#'                                    border = FALSE))
#' # similar results in this case.
#'
#' #### Problematic example 1
#' # One of the reasons as to why posterior simulation of prediction intervals is useful
#' # ... is because a model which explains a small amount of variation may still produce
#' # ... useful insights if the error distribution captures the unexplained variation properly.
#' # ... To investigate this, lets compare the results of two models, one in which the
#' # ... model is properly specified and another in which we introduce heterogeneity:
#'
#' #### Simulate data
#' # Simulate data so that the response (y2.1 or y2.2 is weakly related to a covariate,
#' # ... but in one case, we'll force heterogeneity):
#' x2 <- x
#' y2.1 <- rnorm(nrow(d), x2, 1000)
#' d2 <- data.frame(index = 1:length(x2), x2 = x2, y2.1 = y2.1)
#' d2$y2.2 <- rnorm(nrow(d2), x2, 1.725 * x2)
#' var(d2$y2.1); var(d2$y2.2) # overall variation is similar in both cases
#'
#' #### Visualise simulated data:
#' # y2.2 ~ x2 shows heterogeneity:
#' pp <- par(mfrow = c(1, 2))
#' plot(d2$x2, d2$y2.1)
#' plot(d2$x2, d2$y2.2)
#' par(pp)
#'
#' #### Models
#' m2.1 <- mgcv::gam(y2.1 ~ s(x2), data = d2)
#' m2.2 <- mgcv::gam(y2.2 ~ s(x2), data = d2)
#'
#' #### Both models show limited amounts of variation
#' summary(m2.1)$dev.expl
#' summary(m2.2)$dev.expl
#'
#' #### Posterior simulation from each model:
#' nd <- data.frame(x2 = seq(min(d2$x2), max(d2$x2), length.out = 100))
#' sim_pos_prob1.1 <-
#'   simulate_posterior_obs(
#'     model = m2.1,
#'     newdata = nd,
#'     rho = 0,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'    return = "summary")
#' names(sim_pos_prob1.1)[1] <- "fit"
#'
#' sim_pos_prob1.2 <-
#'   simulate_posterior_obs(
#'     model = m2.2,
#'     newdata = nd,
#'     rho = 0,
#'     type = "envelope",
#'     mu_method = 2,
#'     n = 250,
#'     return = "summary")
#' names(sim_pos_prob1.2)[1] <- "fit"
#'
#'
#' #### Plots: compare simulations of two models
#' pp <- par(mfrow = c(1, 2))
#' # Plot (1):
#' # Define axes:
#' axis_ls <-
#'   prettyGraphics::pretty_axis(side = 1:2,
#'                            x = list(range(d2$x2),
#'                                     range(
#'                                       c(sim_pos_prob1.1$fit,
#'                                         sim_pos_prob1.1$lowerCI,
#'                                         sim_pos_prob1.1$upperCI))
#'                                     ),
#'                            pretty = list(n = 5), add = FALSE)
#' # Plot prediction intervals:
#' plot(d2$x2, d2$y2.1, xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim, axes = FALSE)
#' prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)
#' prettyGraphics::add_model_predictions(nd$x2, sim_pos_prob1.1)
#'
#' # Plot (2):
#' # Define axes:
#' axis_ls <-
#'   prettyGraphics::pretty_axis(side = 1:2,
#'                            x = list(range(d2$x2),
#'                                     range(
#'                                       c(sim_pos_prob1.2$fit,
#'                                         sim_pos_prob1.2$lowerCI,
#'                                         sim_pos_prob1.2$upperCI))
#'                                         ),
#'                            pretty = list(n = 5), add = FALSE)
#' # Plot prediction intervals:
#' plot(d2$x2, d2$y2.2, xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim, axes = FALSE)
#' prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)
#' prettyGraphics::add_model_predictions(nd$x2, sim_pos_prob1.2)
#' # It is clear that we're not capturing the variation appropriately in one part of the plot.
#' # This appears as problematic diagnostics.
#' # ... However, in this case, our model inferences are not affected strongly:
#' pp <- par(mfrow = c(3, 4))
#' mgcv::plot.gam(m2.1)
#' mgcv::gam.check(m2.1, cex = 0.1, col = "grey")
#' mgcv::plot.gam(m2.2)
#' mgcv::gam.check(m2.2, cex = 0.1, col = "grey")
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

simulate_posterior_obs <-
  function(
    # Supply a model to make predictions
    model,
    # Supply a dataframe to make predictions
    newdata,
    # specify rho
    rho,
    # specify independent timeseries,
    ind = NULL,
    # Specify type as "snapshot" or "envelope"
    type = "snapshot",
    # If type = "envelope"...
    # Specify mu_method (1 or 2)
    mu_method = 1,
    # Specify seed
    seed = NULL,
    # Number of samples
    n,
    # Return the posterior distribution and summary?
    return = "both",
    # define probabilities to summaries posterior
    probs = c(0.025, 0.975),
    # Format for summary
    summary_format = "list"){

    #### Set up
    # Define the number of rows in newdata
    nrw_nd <- nrow(newdata)
    # obtain sigma (assume fixed, known)
    sigma <- stats::sigma(model)
    # define expected values
    mu <- stats::predict(model, newdata, se.fit = TRUE, type = "response")
    mu_dat <- data.frame(fit = mu$fit, se = mu$se.fit)
    # vectorise r nrorm
    rnorm.vec <- Vectorize(stats::rnorm, vectorize.args = "mean")
    # sigma arima
    if(rho != 0){
      sigma_arima <- sigma_arima(rho, sigma)
    }

    #### Type == "snapshot"
    if(type == "snapshot"){

      # Set seed if supplied
      if(!is.null(seed)){
        set.seed(seed)
      }

      #### For uncorrelated observations:
      # draw a single observation for each mean value
      # ... from a normal distribution:
      if(rho == 0){
        obs_sim <- stats::rnorm(n = nrw_nd, mean = mu$fit, sd = sigma)

      #### For correlated observations:
      # Loop over each independent timeseries and define autocorrelated observations using arima.sim()
      } else{
        # Define a list to loop over based on independent timeseries
        if(is.null(ind)){
          mu_ls <- list(mu_dat)
        } else{
          stopifnot(length(ind) == nrow(ind))
          if(is.factor(ind)){
            stopifnot(levels(ind) %in% ind)
          }
          mu_ls <- split(mu_dat, f = ind)
        }
        # Define a list of simulated observations, one element for each independent timeseries
        obs_sim_ls <-
          lapply(mu_ls, function(dind){
            # simulate observations using arima.sim
            # note the use of sigma_arima to use the appropriate sd value for sigma.arima()
            # ... so that the output of arima.sim() matches the estimated sigma value.
            obs_sim <- dind$fit + stats::arima.sim(list(order = c(1, 0, 0), ar = rho), n = nrow(dind), sd = sigma_arima)
            return(obs_sim)
          })
        # Unlist values
        obs_sim <- as.vector(unlist(obs_sim_ls))
      } # close else
      obs_sim <- as.numeric(obs_sim)
      obs_sim <- data.frame(fit = mu$fit, se_fit = mu$se.fit, obs_sim = obs_sim)
      # return the simulated values:
      return(obs_sim)

      #### Type == "envelope"
    } else if(type == "envelope"){

      #### Model uncertainity
      # (1) Method 1
      # We will assume that the distribution of means is normally distributed:
      if(mu_method == 1){
        # If stats::rnorm() was used here to simulate the response, the mean and sd arguments are recyled
        # ... we we generate one value of the posterior distribution for each mu value.
        # ... Instead, we want to generate 1000 values for each mu value,
        # ... hence we'll use rnorm.vec(), which is vectorised over the mean.
        # Therefore, this function generates a matrix which we can then
        # ... summarise.
        posterior_mu <- t(rnorm.vec(n, mu$fit, mu$se.fit))
      }

      # (2) Method 2
      if(mu_method == 2){
        # This method also generates a matrix of n realisations of mu for each
        # ... value of mu, but does not assume that the true mu is normally distributed
        # ... arounnd the expected mu, as assumed by mu_method = 1.
        posterior_mu <- simulate_posterior_mu(model = model, newdata = newdata, seed = seed, n = n, return = "full")
      }

      #### Prediction uncertainty
      # Obtain the full posterior distribution for the predictions
      # A) Considering uncorrelated data...
      # Define a blank matrix in which we'll store the posterior distribution for predictions
      pp <- posterior_mu
      pp[] <- NA

      if(rho == 0){
        # For each column (i.e. mu value), simulate observed values for every datapoint
        # Becxause n and posterior_mu[, j] are the same length, each mu is used once.
        for(j in 1:n){
          pp[, j]  <- stats::rnorm(nrw_nd, posterior_mu[, j], sigma)
        }

      # B) Considering correlated data...
      } else{
        # Split the posterior_mu matrix into a list of independent timeseries
        # so we generate a dataframe with n variables and nrw_nd observations
        posterior_mu_df <- as.data.frame(posterior_mu)
        pp_df <- as.data.frame(pp)
        if(is.null(ind)){
          posterior_mu_df_ls <- list(posterior_mu_df)
          pp_df_ls <- list(pp_df)
        } else{
          posterior_mu_df_ls <- split(posterior_mu_df, f = ind)
          pp_df_ls <- split(pp_df, f = ind)
        }
        # Loop over each independent timeseries; we generate a list with
        # ... an element for each independent timeseries, with each element consisting
        # ... of a dataframe with n columns and a number of observations
        ppls <- mapply(pp_df_ls, posterior_mu_df_ls, FUN = function(ppdf, pmdf){
          # Define the number of observations in that timeseries
          n_pmdf <- nrow(pmdf)
          # For each column in the subsetted posterior_mu matrix (now dataframe) i.e., a simulated mu value,
          # ... add autocorrelated errors
          for(j in 1:n){
            ppdf[, j] <- pmdf[, j] +
              # Use as.numeric, otherwise recognised as time series which can return unneccessary
              # ... warnings down the line (e.g.
              # ... 'In bind_rows_(x, .id) :
              # ... Vectorizing 'ts' elements may not preserve their attributes)
              as.numeric(stats::arima.sim(list(order = c(1, 0, 0), ar = rho), n = n_pmdf, sd = sigma_arima))
          }
          return(ppdf)
        }, SIMPLIFY = FALSE)
        pp <- do.call(rbind, ppls)
        pp <- as.matrix(pp)
        colnames(pp) <- 1:n
      }

      #### Return the full posterior matrix, if requested
      if(return == "full"){
        return(pp)
      }

      #### Summarise the posterior:
      pp_summary <- summarise_posterior(pp, probs = probs, summary_format = summary_format)

      #### Return outputs
      if(return == "summary"){
        return(pp_summary)
      } else if(return == "both"){
        ret <- list(posterior = pp, summary = pp_summary)
        return(ret)
      }

    } # close else if(type == "envelope"){

  } # close function



##############################################
##############################################
#### simulate_posterior_mu()

#' @title Simulate from the posterior distribution of an mgcv Generalised Additive Model (GAM)
#' @description This function simulates expected values from a GAM, on the scale of the the response, using posterior simulation. To do this, the function assumes that the uncertainty in fitted coefficients can be characterised by a multivariate normal distribution with a mean vector given by fitted coefficients and a covariance matrix equal to their (Bayesian) covariance matrix. The function samples \code{n} new coefficient vectors from this multivariate distribution. Each sample of coefficient vectors is combined with the linear predictor matrix (and the inverse link function) to generate a single, simulated mean value for each coefficient vector sample. This results in a posterior distribution matrix, in which each row contains \code{n} simulated mean values (stored in different columns) for that observation. The posterior distribution matrix can be summarised, by calculating the mean value and confidence intervals, for every observation, by internally implementing \code{\link[Tools4ETS]{summarise_posterior}}.
#'
#' @param model A model from which to simulate (see \code{\link[mgcv]{gam}}).
#' @param newdata A dataframe from which to make predictions (see \code{\link[mgcv]{predict.gam}}).
#' @param seed A numeric value to set the seed.
#' @param n A numeric value which defines the number of simulations.
#' @param return A character input which defines the output type. Options are the following: (1) \code{"full"}, returns the full posterior distribution; (2) \code{"summary"} returns a summary of the posterior distribution computed by \code{\link[Tools4ETS]{summarise_posterior}}; (3) \code{"both"}, which returns a list containing (a) the full posterior distribution and (b) a summary of the posterior distribution.
#' @param probs A numeric vector which defines the quantiles of the posterior distribution to return as the lower and upper confidence intervals, if \code{return = "summary"} or \code{return = "both"} (see \code{\link[Tools4ETS]{summarise_posterior}}).
#' @param summary_format A character input which defines the format of the summary distribution, if \code{return = "summary"} or \code{return = "both"} (see \code{\link[Tools4ETS]{summarise_posterior}}).
#'
#' @return The function returns a matrix, or list depending on the input to \code{return} and \code{summary_format}.
#'
#' @details This function was written for models of class "gam" (see \code{\link[mgcv]{gamObject}}).
#'
#' @source This function uses the method described in Simon Wood's lecture notes to simulate from a GAM: http://www.maths.bris.ac.uk/~sw15190/mgcv/tampere/mgcv-advanced.pdf.
#'
#' @examples
#'
#' #### Simulate some data and fit a GAM
#' set.seed(1)
#' nobs <- 100
#' x <- stats::runif(nobs, 0, 1000)
#' mu <- 0.001 * x^2
#' y <- stats::rnorm(nobs, mu, 100)
#' d <- data.frame(x = x, y = y)
#' m1 <- mgcv::gam(y ~ s(x), data = d)
#'
#' #### Example (1): Return the full posterior distribution
#' # ... based on 1000 simulations:
#' sim1 <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = d,
#'     n = 500,
#'     return = "full")
#' # The function returns a matrix, w
#' # ... with one row for each observation
#' # ... and one column for each simulated mean/fitted value (on the scale of the response)
#' # ... for that observation.
#' utils::str(sim1)
#'
#' #### Example (2): The function can summarise the posterior distribution internally
#' # ... using summarise_posterior()
#' sim2 <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = d,
#'     n = 500,
#'     return = "summary")
#' # The function returns a list, with 'mean', 'lowerCI' and 'upperCI'
#' utils::str(sim2)
#'
#' #### Example (3): summaries can be adjusted via arguments which are passed to summarise_posterior()
#' # ... These are 'probs' and 'summary_format'. For example, to return 89 % confidence intervals
#' # ... in a matrix:
#' sim3 <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = d,
#'     n = 500,
#'     return = "summary",
#'     prob = c(0.055, 0.945),
#'    summary_format = "matrix")
#' utils::str(sim3)
#' utils::head(sim3)
#'
#' #### Example (4): The function can be used in combination with add_model_predictions()
#' # This can be plotted using prettyGraphics::add_model_predictions()
#' nd <- data.frame(x = seq(min(d$x), max(d$x), length.out = 100))
#' sim4 <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = nd,
#'     n = 500,
#'     return = "summary")
#' plot(d$x, d$y)
#' names(sim4)[1] <- "fit"
#' prettyGraphics::add_model_predictions(x = nd$x, CI = sim4)
#'
#' #### Example (5): Both the full posterior matrix and the summary can be returned:
#' sim5 <-
#'   simulate_posterior_mu(
#'     model = m1,
#'     newdata = nd,
#'     n = 500,
#'     return = "both")
#' # The simulations are contained in a list
#' utils::str(sim5)
#' # The "posterior" element gives the full posterior distribution:
#' utils::str(sim5$posterior)
#' # The "summary" element gives the summary of the posterior distribution:
#' utils::str(sim5$summary)
#'
#' @author Edward Lavender
#' @export

simulate_posterior_mu <-
  function(
    # Define model from which predictions will be made
    model,
    # Define dataframe from which to make predictions
    newdata,
    # Define seed as a numeric value
    seed = NULL,
    # Define the number of simulations
    n = 1000,
    # Define what you want the function to return (the full posterior, the summary or both)
    return = "both",
    # If you want to return a summary of the posterior, define quantiles (e.g. 95% CI):
    probs = c(0.025, 0.975),
    # Choose whether to return summary info as a list (with mean fitted value, lowerCI and upperCI or a matrix)
    summary_format = "list"
  ){

    #### Define the XpXp matrix: this is the matrix that, once multiplied
    # ... by the model coefficients at the locations defined in newdata, gives the linear predictor:
    Xp <- mgcv::predict.gam(model, newdata, type = "lpmatrix")

    #### Collect the fitted model coefficients and their (Bayesian) covariance matrix
    beta <- stats::coef(model)
    Vb <- stats::vcov(model)

    #### Simulate from the posterior:
    # Set seed if supplied
    if(!is.null(seed)){
      set.seed(seed)
    }
    # Simulate n coefficient vectors from the posterior
    mrand <- MASS::mvrnorm(n, beta, Vb)

    #### Generate predictions for the n simulated models
    # ... on the scale of the response. We will store
    # ... the predictions in a matrix with n columns (with each row)
    # ... corresponding to a datapoint:
    pmat <- matrix(NA, nrow = nrow(newdata), ncol = n)
    inv.link <- stats::family(model)$linkinv
    for (i in seq_len(n)) {
      pmat[, i]   <- inv.link(Xp %*% mrand[i, ])
    }
    # Return this matrix, i.e. the posterior distribution
    # ... if requested
    if(return == "full"){
      return(pmat)
    }

    #### Otherwise, proceed to summarise posterior distribution
    pmat_summary <- summarise_posterior(pmat, probs = probs, summary_format = summary_format)

    #### Return outputs
    if(return == "summary"){
      return(pmat_summary)
    } else if(return == "both"){
      ret <- list(posterior = pmat, summary = pmat_summary)
      return(ret)
    }

  } # close function



##############################################
##############################################
#### summarise_posterior()

#' @title Summarise a posterior distribution matrix
#' @description This function calculates summarises of a posterior distribution matrix, in which each row is an observation and each column is a sample from the posterior distribution for that observation, that can be used to create confidence intervals (or similar) around a fitted line (or similar)based on posterior simulation.
#'
#' @param pmat A matrix which defines the full posterior distribution. Each row is an observation; each column is a sample from the posterior distribution for that observation.
#' @param probs A numeric vector of probabilities which define the desired lower and upper confidence intervals (or similar) respectively.
#' @param summary_format A character input which defines the output format desired. If \code{"list"}, a list is returned with three elements: \code{mean}, \code{lowerCI}, \code{upperCI}. Otherwise, a matrix is returned with a column for the mean, lowerCI and upperCI values for each observation.
#'
#' @return The function returns a list or a matrix, depending on the input to \code{summary_format}.
#'
#' @examples
#'
#' #### Simulate some data and fit a GAM
#' set.seed(1)
#' nobs <- 100
#' x <- stats::runif(nobs, 0, 1000)
#' mu <- 0.001 * x^2
#' y <- stats::rnorm(nobs, mu, 100)
#' plot(x, y)
#' d <- data.frame(x = x, y = y)
#' m1 <- mgcv::gam(y ~ s(x), data = d)
#'
#' #### Simulate the posterior distribution matrix with simulate_posterior_mu()
#' nd <- data.frame(x = seq(min(d$x), max(d$x), length.out = 100))
#' sim1 <- simulate_posterior_mu(
#'   model = m1,
#'   newdata = nd,
#'   n = 1000,
#'   return = "full")
#' # Now we have a matrix in which each row is an observation,
#' # ... and each column is a simulated value for the mean
#' utils::str(sim1)
#'
#' #### Example (1) Summarise the posterior distribution matrix and return a list:
#' summary1 <-
#'   summarise_posterior(pmat = sim1,
#'                       probs = c(0.025, 0.975),
#'                       summary_format = "list")
#' utils::str(summary1)
#' # This can be plotted with pretty.plot::add_model_predictions()
#' # ... see (below).
#'
#' #### Example (2) Summarise the posterior in a matrix:
#' # any value for summary_format other than "list" will return a matrix
#' summary2 <-
#'   summarise_posterior(pmat = sim1,
#'                       probs = c(0.025, 0.0975),
#'                       summary_format = "matrix")
#' utils::str(summary2)
#' utils::head(summary2)
#'
#' #### Example (3) Adjust the quantiles of the distribution returned:
#' summary3 <-
#'   summarise_posterior(pmat = sim1,
#'                       probs = c(0.055, 0.945),
#'                       summary_format = "list")
#' utils::str(summary3)
#'
#' #### Summarised posterior distributions can be plotted with add_model_predictions()
#' plot(d$x, d$y)
#' # 95 % CIs with mean
#' names(summary1)[1] <- "fit"
#' prettyGraphics::add_model_predictions(x = nd$x, CI = summary1)
#' # 89% CIs:
#' names(summary3)[1] <- "fit"
#' prettyGraphics::add_model_predictions(x = nd$x,
#'                                   CI = summary3,
#'                                    CI_gp = list(col = "dimgrey",
#'                                                 border = FALSE
#'                                    )
#' )
#'
#'
#' @author Edward Lavender
#' @export
#'

summarise_posterior <-
  function(
    # The full posterior distribution as a matrix (one row for each datapoint,
    # ... each column as a sample from the posterior distribution for that datapoint)
    pmat,
    # Probabilities used to calculate CIs
    probs = c(0.025, 0.975),
    # Return the summary as a list or a matrix?
    summary_format = "list"){

    #### Calculate the mean of the posterior for each datapoint
    posterior_mean <- apply(pmat, 1, mean)

    #### Calculate CIs:
    # posterior_SD <- apply(pmat, 2, stats::sd)
    # posterior_lowerCI <- posterior_mean - 1.96 * posterior_SD
    # posterior_upperCI <- posterior_mean + 1.96 * posterior_SD
    CI <- t(apply(pmat, 1, stats::quantile, probs))
    pmat_summary <- cbind(posterior_mean, CI)
    elements <- c("mean", "lowerCI", "upperCI")
    colnames(pmat_summary) <- elements

    #### Format summary as requested
    # If summary_format is a list, then we'll overwrite the matrix as a list format:
    if(summary_format == "list"){
      pmat_summary <- lapply(seq_len(ncol(pmat_summary)), function(i) pmat_summary[, i])
      names(pmat_summary) <- elements
    }

    #### Return output:
    return(pmat_summary)

  } # close function


#### End of code.
##############################################
##############################################
