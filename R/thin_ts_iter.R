#' @title Determine the amount of thinning required to reduce autocorrelation to a given level
#' @description Thinning (i.e. retaining every nth observation in a dataset) is a commonly used method to reduce serial autocorrelation and simplify models. The effectiveness of thinning as a strategy to deal with autocorrelation depends on the volume of available data and the level of thinning required to remove autocorrelation. This function implements an iterative thinning approach to determine how the estimated autocorrelation parameter and the volume of data change as a dataset is thinned by progressively greater amounts. To do this, the user must supply a dataframe to be thinned along with parameters that are passed to \code{\link[Tools4ETS]{thin_ts}}, which implements thinning. The user supplies a starting value for the thinning index (i.e., specifying that every nth observation should be retained on the first iteration of the algorithm), and an increment which specifies the increase in the thinning index with each iteration of the algorithm. (For models with high serial autocorrelation, the user is advised to begin with a reasonably large increase in the thinning index with each iteration (\code{increment}), which will result in faster convergence but less smooth profiles (i.e. estimated changes in the autocorrelation parameter with the thinning index.) On each iteration, user-defined functions are used to (a) evaluate a model using the thinned dataset and (b) compute the autocorrelation parameter. These functions provide the flexibility to implement the approach for a wide variety of models in which the fitting procedure may or may not estimate the value of the autocorrelation parameter (e.g. \code{\link[mgcv]{bam}} versus \code{\link[mgcv]{gamm}}). This process increases until the autocorrelation parameter falls below a user-defined threshold. Following algorithm convergence (i.e. the reduction in the autocorrelation parameter below a user-defined threshold), a list of outputs is returned which can be saved. A customisable plot can also be produced to demonstrate the change in (a) the estimated autocorrelation parameter and (b) the volume of data with (c) the level of thinning. This indicates the extent of thinning necessary to reduce autocorrelation consistently below the level expected from white noise (a 95 percent confidence interval is added to the plot), and the volume of data which remains for model fitting after this level of thinning. To facilitate plot customisation, a list of outputs from previous function iterations can be passed to the function, which bypasses the algorithm implementation and simply produces the plot, enabling quick adjustments to each plot component.
#'
#' @param dat A dataframe which is thinned and used to fit the model on the first iteration.
#' @param ind A character which defines the column name in \code{dat} which uniquely distinguishes each independent time series (e.g. \code{flag3} returned by \code{\link[Tools4ETS]{flag_ts}}). If \code{NULL}, \code{dat} is assumed to comprise a single time series. This is passed to \code{\link[Tools4ETS]{thin_ts}}.
#' @param flag1 A character which defines the column name in \code{dat} which defines the start of each independent time series with \code{TRUE}. This is passed to \code{\link[Tools4ETS]{thin_ts}}.
#' @param first A numeric value which defines the starting position in each independent time series from which every \code{nth} observation will be selected. This is passed to \code{\link[Tools4ETS]{thin_ts}}. Unlike \code{\link[Tools4ETS]{thin_ts}}, this should be a single number. However, \code{\link[Tools4ETS]{thin_ts_iter}} can be implemented using different values for \code{first} under the same degree of thinning to compare results among different thinned subsets of data.
#' @param nth A numeric input that defines the starting thinning index value. For a given \code{nth} value, every \code{nth} value is selected. This is passed to \code{\link[Tools4ETS]{thin_ts}}. The default starting value is 0; i.e., the algorithm initially implements a model without thinning, which forms the baseline. The \code{nth} value is then progressively increased by a user-defined increment (see \code{increment}, below). On each iteration, the model is re-run and the estimated value of the autocorrelation parameter is saved until this falls below a user-defined threshold (\code{AR1_req}, see below).
#' @param increment A numeric value that defines the value by which the thinning index, \code{nth}, should increase on each iteration of the algorithm. Larger values result in faster convergence but less smooth profiles. Therefore, it may be advisable to start with a reasonably large value to assess algorithm duration. Based on algorithm speed, the starting value of the thinning index (\code{nth}) can be adjusted and the increment (\code{increment}) decreased to create a smoother profile in the region of interest. The minimum increment is 1.
#' @param AR1_req A numeric input which defines the desired number to which the AR1 parameter should be reduced by thinning.
#' @param eval_mod A function which acts as a wrapper for implementing a model. The only input to this function should be the data used to evaluate the model.
#' @param resid_method A function which extracts the residuals from a model. \code{\link[stats]{resid}} is the default option, which is usually appropriate.
#' @param est_AR1 A function which computes an autocorrelation parameter for a model, such as an AR1 parameter. The only input to this function should be the model. For models without a correlation structure, the default option is usually appropriate: this uses the autocorrelation function of residuals to estimate the approximate AR1 parameter. However, some adjustments may be required to this function (e.g. to extract model residuals appropriately or to extract a different parameter). In other cases, a model may estimate the autocorrelation parameter and the user can define a function here to extract the model estimate from the model object.
#' @param plot A logical input which defines whether or not to produce a plot demonstrating the change in (a) the autocorrelation parameter and (b) the volume of data with (c) the thinning index.
#' @param thin_ts_iter_ls A named list of outputs from a previous implementation of \code{\link[Tools4ETS]{thin_ts_iter}} which can be used to produce a plot. This bypasses the need to re-run the algorithm to produce plots.
#' @param p1_pretty_axis_args A named list of arguments passed to \code{\link[prettyGraphics]{pretty_axis}} to make the x and y axes. These pertain to the thinning index and the autocorrelation parameter. The default options usually result in pretty axes.
#' @param p2_pretty_axis_args A named list of arguments passed to \code{\link[prettyGraphics]{pretty_axis}} to make the second y axis. This pertains to the volume of data left after thinning. The default options usually result in a pretty axis.
#' @param p1_args A named list of arguments to customise the first plot, which demonstrates the decline in the autocorrelation parameter with the thinning index.
#' @param p2_args A named list of arguments to customise the second plot, which is added ontop of the first plot to demonstrate the simultaneous decline in the volume of data with the thinning index.
#' @param add_error_envelope_args A list of arguments passed to \code{\link[prettyGraphics]{add_error_envelope}} to customise the 95 percent confidence envelope for an AR1 parameter in white noise.
#' @param add_legend A logical input which defines whether or not to add a legend.
#' @param legend_args A named list of arguments passed to \code{\link[graphics]{legend}} to customise the legend. Most parameters should be successfully extracted internally from \code{p1_args}, \code{p2_args} and \code{pretty_axis_args}. The placement of the legend is implemented automatically but can be overridden by the user here. Legend placement is implemented in relation to the x axis and the second y axis.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to add axes labels. A nested list is used to control each axis independently.
#' @param verbose A logical input which defines whether or not to print messages; namely, the estimated autocorrelation parameter on each algorithm iteration which can be used to monitor algorithm process/speed of convergence.
#'
#' @return The function returns a list and/or a plot. On the first implementation of the algorithm, a list is contained with five elements: (1) \code{start_time}, the start time of the algorithm; (2) \code{end_time}, the end time of the algorithm; (3) \code{iteration_duration}, the duration (in minutes) of the algorithm; (4) \code{iteration_record}, a dataframe providing a record of algorithm outputs including (a) \code{nth} (the value of the thinning index on each iteration), (b) \code{AR1_est} (the value of the autocorrelation parameter on each iteration), (c) \code{nrw_log} (the logarithm of the number of observations remaining in \code{dat} on each iteration), (d) \code{lowerCI} (the lower 95 percent confidence interval for the AR1 parameter in white noise) and (e) \code{upperCI} (the upper 95 percent confidence interval for the AR1 parameter in white noise) and (4) \code{CI}, a list with three elements (\code{x}, the values of nth, as above; \code{lowerCI}, as above; \code{upperCI}, as above) used to add confidence intervals to the plot via \code{\link[prettyGraphics]{add_error_envelope}}.
#'
#' @examples
#' #### Define model parameters and simulate observations
#' # Imagine the depth of an animal changes in a concave down pattern throughout the year.
#' # Define x values, the number of days since January 1st
#' set.seed(1)
#' x <- 1:365
#' # Define expected y values based on a concave-down effect of x:
#' quadratic <- function(a, b, x, h, k){
#'   step1 <- (b* x - h)^2 + k
#'   step2 <- a * step1
#'   return(step2)
#' }
#' mu <- quadratic(a = -0.001, b = 1, x = x, h = median(x), k = 100)
#' # Define observed y values with autocorrelation
#' y <- mu + arima.sim(list(order = c(1, 0, 0), ar = 0.6), n = length(mu), sd = 5)
#' # Define dataframe
#' d <- data.frame(x = x, y = y)
#' # Visualise simulated observations
#' graphics::plot(d$x, d$y, type = "l")
#'
#' #### Flag independent sections of time series
#' d <- cbind(d, flag_ts(x = d$x, duration_threshold = 2880, flag = 1:3))
#' head(d)
#'
#' #### Example (1): Implement thin_ts_iter with default options
#' thin_ts_iter_ls1 <-
#'   thin_ts_iter(dat = d,
#'                ind = "flag3",
#'                flag1 = "flag1",
#'                first = 1,
#'                AR1_req = 0.01,
#'                nth = 0,
#'                increment = 1,
#'                eval_mod = function(data){ mgcv::bam(y ~ s(x), data = data) },
#'                resid_method = function(mod) { stats::resid(mod) },
#'                est_AR1 = function(mod){ stats::acf(stats::resid(mod), plot = FALSE)$acf[2] },
#'                thin_ts_iter_ls = NULL,
#'                plot = TRUE,
#'                p1_pretty_axis_args = list(side = 1:2, pretty = list(n = 5)),
#'                p2_pretty_axis_args = list(side = 4, pretty = list(n = 5)),
#'                p1_args = list(type = "b",
#'                               pch = 21,
#'                               bg = "black",
#'                               col = "black",
#'                               cex = 0.5
#'                ),
#'                p2_args = list(type = "p",
#'                               pch = 24,
#'                               bg = "dimgrey",
#'                               col = "dimgrey",
#'                               cex = 0.5
#'                ),
#'                add_error_envelope_args = list(),
#'                add_legend = TRUE,
#'                legend_args = list(),
#'                mtext_args = list(list(side = 1, text = "Thinning Index", line = 2.5),
#'                                  list(side = 2,
#'                                       text = expression(paste(hat(AR1)["lag =" ~ 1])),
#'                                       line = 2.5),
#'                                  list(side = 4, text = "log[n(obs)]", line = 1)
#'                ),
#'                verbose = TRUE
#'   )
#'
#'
#' @author Edward Lavender
#' @export

###########################################
###########################################
#### thin_ts_iter()

thin_ts_iter <-
  function(
    dat,
    ind = NULL,
    flag1,
    first = 1,
    AR1_req = 0.01,
    nth = 0,
    increment = 1,

    eval_mod,
    resid_method = function(mod){ stats::resid(mod) },
    est_AR1 = function(mod){ stats::acf(stats::resid(mod), plot = FALSE)$acf[2] },

    thin_ts_iter_ls = NULL,
    plot = TRUE,
    p1_pretty_axis_args = list(side = 1:2, pretty = list(n = 5)),
    p2_pretty_axis_args = list(side = 4, pretty = list(n = 5)),
    p1_args = list(type = "b",
                   pch = 21,
                   bg = "black",
                   col = "black",
                   cex = 0.5
                   ),
    p2_args = list(type = "b",
                   pch = 21,
                   bg = "dimgrey",
                   col = "dimgrey",
                   cex = 0.5
                   ),
    add_error_envelope_args = list(),
    add_legend = TRUE,
    legend_args = list(),
    mtext_args = list(list(side = 1, text = "Thinning Index", line = 2.5),
                      list(side = 2, text = expression(paste(hat(AR1)["lag =" ~ 1])), line = 2.5),
                      list(side = 4, text = "log[n(obs)]", line = 2.5)
                      ),
    verbose = TRUE
    ){


    ###########################################
    #### Implement iterative thinning (and processing)
    # ...if thin_ts_iter_ls not provided

    if(is.null(thin_ts_iter_ls)){

      #### Set up
      if(verbose){
        cat("(1) Setting up function... \n")
      }

      #### Loop prerequistes
      # Save nth value (i.e. degree of thinning)
      nth_ls <- list()
      # Estimated AR1 values
      AR1_est_ls <- list()
      # number of rows left in df
      nrw_ls <- list()
      # upper limit of white noise
      wnupper_ls <- list()
      # Set starting values that are not in input
      AR1_est <- 1

      if(verbose){
        cat("(2) Iteratively thinning the data and computing the AR1 parameter; the AR1 or model errors from each iteration will be printed... \n")
      }

      #### Iteractively thin the data and compute AR1 parameter
      while_t1 <- Sys.time()
      while(AR1_est > AR1_req){
        # print AR1_est to monitor progress
        if(verbose){
          print(AR1_est)
        }
        # Increase nth
        nth <- nth + increment
        # Thin data with appropriate nth value
        dat_thin_iter <- thin_ts(dat = dat,
                                 ind = ind,
                                 flag1 = flag1,
                                 first = first,
                                 nth = nth
                                 )
        # Define model; use tryCatch to avoid errors
        # ... (sometimes the number of knots chosen is more than allowed by the data)
        m1a <-
          tryCatch(
            eval_mod(dat_thin_iter),
            error = function(e) return(e)
          )

        # Add values to the lists:
        nth_ls[[nth]] <- nth
        nrw_ls[[nth]] <- nrow(dat_thin_iter)

        # If the model returned an error, we'll print the error  add NAs to the list
        if(inherits(m1a, "error")){
          if(verbose){
            print(m1a)
          }
          AR1_est_ls[[nth]] <- NA
          wnupper_ls[[nth]] <- NA
        } else{
          # Compute AR1 estimate and add to list
          AR1_est <- est_AR1(m1a)
          AR1_est_ls[[nth]] <- AR1_est
          wnupper_ls[[nth]] <- nth
          # Compute upper limit of white noise and add to list
          wnupper <- acf_in_white_noise(stats::acf(resid_method(m1a), plot = FALSE))
          wnupper_ls[[nth]] <- wnupper
        }
      }
      while_t2 <- Sys.time()
      tdiff <- difftime(while_t2, while_t1, units = "mins")


      ###########################################
      #### Process outputs

      AR1_ests <- as.vector(unlist(AR1_est_ls))
      nths <- as.vector(unlist(nth_ls))[1:length(AR1_ests)]
      nrws <- as.vector(unlist(nrw_ls))
      wnuppers <- as.vector(unlist(wnupper_ls))
      thinreq <- data.frame(nth = nths, AR1_est = AR1_ests, nrw = nrws, nrw_log = log(nrws))
      thinreqCI <- list(lowerCI = 0 - wnuppers, upperCI = 0 + wnuppers)
      notNA <- which(!is.na(thinreq$AR1_est))
      thinreq$lowerCI <- thinreqCI$lowerCI
      thinreq$upperCI <- thinreqCI$upperCI
      thinreqCI <- list(x = thinreq$nth[notNA], lowerCI = thinreqCI$lowerCI[notNA], upperCI = thinreqCI$upperCI[notNA])

    } # close if(is.null(thin_ts_iter_ls)){


    ############################################
    #### Extract dataframes to plot (if inputted)

    if(!is.null(thin_ts_iter_ls)){
      thinreq <- thin_ts_iter_ls$iteration_record
      thinreqCI <- thin_ts_iter_ls$CI
    }


    ############################################
    #### Plot the graph of AR1 ~ thinning and nrow(data) ~ thinning

    if(plot){

      #### Define axis parameters
      axis_ls <- prettyGraphics::implement_pretty_axis_args(list(thinreq$nth, thinreq$AR1_est), p1_pretty_axis_args)
      # Parameters for the new axis (side = 4), which needs to be added afterwards.
      p2_pretty_axis_args$axis$side <- 4
      p2_pretty_axis_args$axis$pos <- max(axis_ls[[1]]$lim)
      axis_ls_2nd <- prettyGraphics::implement_pretty_axis_args(list(thinreq$nrw_log), p2_pretty_axis_args)

      #### Blank plot of AR1 ~ nth
      # Blank plot
      plot(thinreq$nth, thinreq$AR1_est, type = "n",
           axes = FALSE,
           xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim,
           xlab = "", ylab = "")

      #### Clip
      usr <- graphics::par("usr")
      graphics::clip(axis_ls[[1]]$lim[1], axis_ls[[1]]$lim[2], axis_ls[[2]]$lim[1], axis_ls[[2]]$lim[2])

      #### Add acf CIs (added at this stage so CI envelope is the 'bottom' layer)
      # Define arguments
      damp <- list(x = thinreqCI$x,
                   ci = thinreqCI,
                   type = "poly",
                   add_ci = list(col = "lightgrey", border = FALSE),
                   add_fit = NULL
                   )
      add_error_envelope_args <- list_merge(damp, add_error_envelope_args)
      # Add CI envelope
      do.call(prettyGraphics::add_error_envelope, add_error_envelope_args)

      #### Restore clip
      do.call("clip", as.list(usr))

      #### Plot of AR1 ~ nth
      # Define a list of arugments
      dp1 <- list(x = thinreq$nth,
                  y = thinreq$AR1_est)
      p1_args <- list_merge(dp1, p1_args)
      # Make plot
      do.call(graphics::lines, p1_args)

      #### Add initial axes
      prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)

      #### Add axis_titles
      prettyGraphics::implement_mtext_args(mtext_args)

      #### Plot nrow(data) ~ nth ontop using a second y axis
      graphics::par(new = TRUE)
      dp2 <- list(x = thinreq$nth,
                  y = thinreq$nrw_log,
                  axes = FALSE,
                  xlim = axis_ls[[1]]$lim, ylim = axis_ls_2nd[[1]]$lim,
                  xlab = "", ylab = "")
      p2_args <- list_merge(dp2, p2_args)
      do.call(graphics::plot, p2_args)

      #### Add second axes
      prettyGraphics::pretty_axis(axis_ls = axis_ls_2nd, add = TRUE)

      # Add legend
      if(add_legend){
        # Define basline list of arguments
        dl <- list(x = axis_ls[[1]]$axis$at[length(axis_ls[[1]]$axis$at)-1],
                   y = axis_ls_2nd[[1]]$lim[2],
                   pch = c(21, 21),
                   lty = c(1, 1),
                   legend =
                     c(
                       expression(paste(hat(AR1)["lag =" ~ 2])),
                       "nobs"
                     )
        )
        # Remove lty/pch elements as appropriate depending on inputs
        if(p1_args$type == "p") dl$lty[1] <- NA
        if(p2_args$type == "p") dl$lty[2] <- NA
        if(p1_args$type == "l") dl$pch[1] <- NA
        if(p2_args$type == "l") dl$pch[2] <- NA
        # Adjust arguments for points
        l_args = c("pch", "lty", "col", "pt.bg", "pt.cex", "pt.lwd") # names of arguments for legend
        p_args = c("pch", "lty", "col", "bg", "cex", "lwd")          # names of arguments in points lists
        p_args_ls <- list(p1_args, p2_args)
        for(i in 1:6){
          p_arg <- p_args[i]
          l_arg <- l_args[i]
          for(j in 1:2){
            if(!is.null(p_args_ls[[j]][[p_arg]])){
              dl[[l_arg]][j] <- p_args_ls[[j]][[p_arg]]
            }
          }
        }
        if(!is.null(axis_ls$`1`$axis$cex.axis)){
          dl$cex.axis <- axis_ls$`1`$axis$cex.axis
        }
        legend_args <- list_merge(dl, legend_args)
        do.call(graphics::legend, legend_args)
      }

    } # close if plot


    ###########################################
    #### Define a list of outputs if computed:

    if(is.null(thin_ts_iter_ls)){
      #### Define output list
      ls <- list(start_time = while_t1,
                 end_time = while_t2,
                 iteration_duration = tdiff,
                 iteration_record = thinreq,
                 CI = thinreqCI
                 )

      #### Return outputs
      return(ls)
    }

  } # close function

#### End of code.
###########################################
###########################################
