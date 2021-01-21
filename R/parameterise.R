########################################
########################################
#### parameterise_contrast_2l()

#' @title Define and visualise a contrast between two factor levels
#' @description This function is used to define and visualise a contrast between two factor levels (e.g. the difference in depth between males and females). Simulated contrasts can be compared to estimated contrasts from an \code{\link[mgcv]{mgcv}} generalised additive model (GAM). The function is intended primarily for use behind-the-scenes in \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param x A vector of two factor levels for which to define a contrast.
#' @param param A numeric input which defines the size of the contrast between two factor levels.
#' @param plot_gam A logical input which defines whether or not to add model estimates to the plot.
#' @param dat The dataframe used to fit the model.
#' @param model The model.
#' @param term A character input which defines the name of the term in the model.
#' @param cex.axis A numeric input which defines the size of the font for axis tick labels.
#' @param mtext_args A named list of arguments which are passed to \code{\link[graphics]{mtext}} to produce axis labels. A nested list can be used to control each axis (see also \code{\link[prettyGraphics]{pretty_ts}}).
#'
#' @return The function returns a plot showing the size of the contrast between factor levels, possibly in relation to that estimated by an \code{\link[mgcv]{mgcv}} GAM model.
#'
#' @author Edward Lavender
#' @export
#'

parameterise_contrast_2l <-
  function(
    x = factor(c("F", "M")),
    param = 0,
    plot_gam = FALSE,
    dat,
    model,
    term = "sex",
    cex.axis = 1,
    mtext_args = list(list(side = 1, text = "Sex (F, female; M, male)", line = 2.5),
                      list(side = 2, text = "Contrast (m)", line = 2.5)
    )
  ){


    ################################################
    #### Define y axis limits

    # If we're not making predictions,
    # Then we'll set the limits between the minimum and maximum values ± 2 as appropriate
    if(!plot_gam){
      # check whether 0 or the contrast is lower; minut 2
      ymin <- min(c(0, param))-2
      # check whether 0 or the contrast is higher; add 2
      ymax <- max(c(0, param))+2
      # define y values using ymin and ymax
      yat <- pretty(c(ymin, ymax), 5)

      # Else If we are making predictions, then we can set the limits
      # ... based on our confidence intervals around the fitted values
    } else if(plot_gam){

      # extract all of the covariates included in the model:
      terms_smooth <- sapply(model$smooth, function(element){return(element$term)})
      terms_fixed <- attr(model$pterms, "term.labels")
      terms <- c(terms_smooth, terms_fixed)
      # remove the covariate of interest (sex here)
      terms <- terms[which(terms != term)]

      # create a new data dataframe containing sex:
      nd <- data.frame(fct = levels(dat[, term]))
      colnames(nd) <- term
      # add other covariates, giving them values of 0
      for(q in terms){
        nd[, q] <- 0}

      # Obtain predictions from the model using the predict function:
      preds <- stats::predict(model, newdata = nd, type = "terms", se = T)

      # Obtain the fitted values for sex
      fits <- preds$fit[, term]

      # Obtain the SEs for sex (*2, as in termsplot - the function which plots these plots by default in plot.gam())
      ses <- preds$se.fit[, term]
      se_lower <- fits[2]-ses*1.96
      se_upper <- fits[2]+ses*2

      # define ymin, ymax and yat as above:
      ymin <- min(c(0, se_lower, param))
      ymax <- max(c(0, se_upper, param))
      yat <- pretty(c(ymin, ymax), 5)
    }

    # create a boxplot of the param ~ sex, hiding axes:
    graphics::boxplot(c(0, param) ~ c(0, 1),
                      axes = F,
                      xlim = c(0.5, 2.5),
                      ylim = c(min(yat), max(yat)),
                      xlab = "", ylab = "")

    # add the y axis
    # set pos = 0.5, which we set as the minimum point on the graph above
    graphics::axis(side = 2, at = yat, las = 2, cex.axis = cex.axis, pos = 0.5)

    # add the x axis
    # note that the final at = 3, is beyond the upper x limit (2.5), for neatness (avoid uneven ticks)
    # set the position to be the minimum value of yaxis
    graphics::axis(side = 1, at = c(0.5, 1, 2, 3), labels = c("", as.character(x), ""), pos = min(yat), cex.axis = cex.axis)

    # add axis labels
    prettyGraphics::implement_mtext_args(mtext_args)

    if(plot_gam){
      graphics::points(c(1, 2), fits, pch = 21, bg = "darkgrey", col = "darkgrey")
      graphics::segments(x0 = 2, y0 = se_lower, y1 = se_upper,
                         col = "dimgrey",
                         lwd = 2,
                         lty = 1)
    }

    # close the function
  }


########################################
########################################
#### parameterise_smooth()

#' @title Parameterise smooth functions and compare simulated and inferred smooths
#' @description This function helps the user to define reasonable functions for the effect of a covariate on a linear predictor/response for simulations. Then, model-based inferences of the effect of that covariate can be plotted on top of simulated effects to examine the performance of a model in terms of its ability to correctly estimate the simulated function under different scenarios. This function is intended for use primarily in \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param x A numeric sequence of values for which to evaluate the shape of a user-defined function (see below).
#' @param f A function which relates a covariate to a linear predictor/response.
#' @param param A named list of parameters and parameter values required to evaluate the function.
#' @param parameterise_smooth_ls A list from a previous implementation of \code{\link[Tools4ETS]{parameterise_smooth}}. This is useful following modelling when the aim is to compare simulated and model-inferred smooths. By supplying this list, the function does not have to recompute simulated smooths.
#'
#' @param plot A logical input which defines whether or not to create a plot.
#' @param add_sim_args A named list of arguments to customise the visualisation of the simulated effect of a covariate on the plot.
#' @param xlab A character input which defines the label for the x axis. By default, this is \code{""} so that labels can be added via \code{mtext_args} which is more flexible (see below). However, the label can be specified via \code{xlab} for speed.
#' @param ylab A character input which defines the label for the y axis. By default, this is \code{""} so that labels can be added via \code{mtext_args} which is more flexible (see below). However, the label can be specified via \code{ylab} for speed.
#'
#' @param plot_gam A logical input which defines whether or not to plot the output of a GAM on top of the simulated effect.
#' @param model The model used to fit the GAM.
#' @param dat The dataframe used to fit the GAM.
#' @param term A character specifying the term of interest.
#' @param plot_gam_ls The output of \code{\link[mgcv]{plot.gam}}. This is used as a quick method to add model predictions for the effect of a term on the response, confidence intervals and (optionally) partial residuals to the plot, since these are all computed by \code{\link[mgcv]{plot.gam}}.
#' @param add_error_envelope_args A named list of arguments passed to \code{\link[prettyGraphics]{add_error_envelope}} to add predictions to the plot.
#' @param residuals A logical input which defines whether or not to add partial residuals to the plot.
#' @param add_residuals_args A named list of arguments to customise the partial residuals on the plot.
#' @param shift_truth A number which defines a value by which to shift f(x) vertically.
#' @param shift_predictions A number which defines a value by which to shift model predictions/partial residuals vertically.
#'
#' @param add_rug A logical input which defines whether or not to plot a rug. If so, \code{dat}, the dataframe used to fit the model, should be provided (see above).
#' @param add_rug_args A named list of arguments passed to \code{\link[graphics]{rug}} to customise the rug.
#' @param add_moons A logical input which defines whether or not to add moons to a plot. This is useful for visualising the effects of lunar phase in models of animal movement time series.
#' @param add_moons_args A named list of arguments passed to \code{\link[prettyGraphics]{add_moons}}.
#'
#' @param pretty_axis_args A named list of arguments, passed to \code{\link[prettyGraphics]{pretty_axis}} to customise axes.
#' @param mtext_args A named list of arguments, passed to \code{\link[graphics]{mtext}}, to add axis labels.
#' @param ... Additional plotting parameters passed to \code{\link[prettyGraphics]{pretty_plot}}.
#'
#' @examples
#' #### Example 1: Simulate a quadratic effect of variable (e.g. Julian day) on a response (e.g. depth)
#' # The function creates a plot, so we can 'play' with the parameters
#' # .... until we're happy with the shape:
#' smooth_julian_day <-
#'   parameterise_smooth(x = 0:365,
#'                       f = quadratic,
#'                       param = list(a = -0.001, b = 1, h = 183, k = 15),
#'                       plot = TRUE)
#' # The function returns a list containing x, f, the parameters we chose and y values:
#' utils::str(smooth_julian_day)
#' # We could then use these parameters to simulate a response, model the response using a GAM
#' # ... and then compare the simulated and modelled relationship using parameterise_smooth() again
#' # ... (see the vignette or GAMS4DTS() for further examples).
#'
#' @author Edward Lavender
#' @export
#'

parameterise_smooth <-
  function(
    x,
    f,
    param = list(),
    parameterise_smooth_ls = NULL,

    plot = TRUE,
    add_sim_args = list(type = "l", lwd = 2),
    xlab = "",
    ylab = "",

    plot_gam = FALSE,
    dat,
    model,
    term,
    plot_gam_ls,
    add_error_envelope_args = list(),
    residuals = FALSE,
    add_residuals_args = list(),
    shift_truth = 0,
    shift_predictions = 0,

    add_rug = FALSE,
    add_rug_args = list(),

    add_moons = FALSE,
    add_moons_args = list(),

    pretty_axis_args = list(side = 1:2, pretty = list(n = 10), control_axis = list(las = TRUE)),
    mtext_args = list(),...

  ){

    #### Evaluate function
    if(is.null(parameterise_smooth_ls)){
      param$x <- x
      y <- do.call(f, param)
    } else{
      x <- parameterise_smooth_ls$x
      y <- parameterise_smooth_ls$y
    }
    if(shift_truth != 0) y <- y + shift_truth

    #### Parameters if plotting a GAM
    if(plot_gam){
      model_terms <- sapply(model$smooth, function(element){return(element$term)})
      term_no <- which(model_terms == term)
      p <- plot_gam_ls[[term_no]]

      p$fit <- as.numeric(p$fit)
      p$se <- as.numeric(p$se)
      p$p.resid <- as.numeric(p$p.resid)
      if(shift_predictions != 0){
        p$fit <- p$fit + shift_predictions
        p$p.resid <- p$p.resid + shift_predictions
      }

      CI <- prettyGraphics::list_CIs(pred = list(fit = p$fit, se.fit = p$se), plot_suggestions = FALSE)
    }

    #### Create plot
    if(plot){

      #### Pretty axes, depending on whether or not predictions and/or partial residuals are added
      # ... (Assume CIs always desired).
      if(plot_gam){
        pretty_axis_x <- c(x, p$x)
        if(residuals){
          pretty_axis_y <- c(y, p$fit, p$p.resid, CI$lowerCI, CI$upperCI)
        } else{
          pretty_axis_y <- c(y, p$fit, CI$lowerCI, CI$upperCI)
        }
      } else{
        pretty_axis_x <- x
        pretty_axis_y <- y
      }
      axis_ls <- prettyGraphics::implement_pretty_axis_args(list(x = range(pretty_axis_x),
                                                                 y = range(pretty_axis_y)),
                                                            pretty_axis_args)

      #### Blank plot with parameterisation
      check...(c("type", "axes", "xlim", "ylim"))
      plot(x, y,
           type = "n",
           axes = FALSE,
           xlab = xlab, ylab = ylab,
           xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim,...)

      #### Clip plotting area
      usr <- graphics::par("usr")
      graphics::clip(axis_ls[[1]]$lim[1], axis_ls[[1]]$lim[2], axis_ls[[2]]$lim[1], axis_ls[[2]]$lim[2])

      #### Add model predictions
      if(plot_gam){

        #### Add fitted values and CI envelope
        amp <- list(x = p$x,
                    CI = CI,
                    fCI = "poly",
                    CI_gp = list(col = "lightgrey", border = FALSE),
                    add_fitted = TRUE,
                    fitted_gp = list(col = "black", lwd = 1, lty = 1)
        )
        add_error_envelope_args <- list_merge(amp, add_error_envelope_args)
        do.call(prettyGraphics::add_error_envelope, add_error_envelope_args)

        #### Add partial residuals
        if(residuals){
          pra <- list(x = dat[, term], y = p$p.resid)
          add_residuals_args <- list_merge(pra, add_residuals_args)
          do.call(graphics::points, add_residuals_args)
        }
      }

      #### Add parameterisation
      asa <- list(x = x, y = y)
      add_sim_args <- list_merge(asa, add_sim_args)
      do.call(graphics::lines, add_sim_args)
      prettyGraphics::pretty_axis(axis_ls = axis_ls, add = TRUE)

      #### Add rug
      if(add_rug){
        ara <- list(x = dat[, term], pos = axis_ls[[2]]$lim[1])
        add_rug_args <- list_merge(ara, add_rug_args)
        do.call(graphics::rug, add_rug_args)
      }

      if(add_moons){
        dam <- list(side = 3, position = axis_ls[[2]]$lim[2], outer = TRUE, nv = 100, radius1 = 0.1, units = "radians")
        add_moons_args <- list_merge(dam, add_moons_args)
        do.call(prettyGraphics::add_moons, add_moons_args)
      }

      #### Add axis labels
      prettyGraphics::implement_mtext_args(mtext_args)

      #### restore clip
      do.call("clip", as.list(usr))

    } # close if(plot)


    #### Return y values
    if(is.null(parameterise_smooth_ls)){
      param$x <- NULL
      ls <- list(x = x, f = f, param = param, y = y)
      return(ls)
    } # close function
  }



#### End of code.
########################################
########################################
