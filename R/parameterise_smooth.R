#' @title Parameterise smooth functions and compare simulated and inferred smooths
#' @description This function helps the user to define reasonable functions for the effect of a covariate on a linear predictor/response for simulations. Then, model-based inferences of the effect of that covariate can be plotted on top of simulated effects to examine the performance of a model in terms of its ability to correctly estimate the simulated function under different scenarios.
#'
#' @param x A numeric sequence of values for which to evaluate the shape of a user-defined function (see below).
#' @param f A function which relates a covariate to a linear predictor/response.
#' @param param A named list of parameters and parameter values required to evaluate the function.
#' @param parameterise_smooth_ls A list from a previous implementation of \code{parameterise_smooth()}. This is useful following modelling when the aim is to compare simulated and model-inferred smooths. By supplying this list, the function does not have to recompute simulated smooths.
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
#' @param add_model_predictions_args A named list of arguments passed to \code{\link[plot.pretty]{add_model_predictions}} to add predictions to the plot.
#' @param residuals A logical input which defines whether or not to add partial residuals to the plot.
#' @param add_residuals_args A named list of arguments to customise the partial residuals on the plot.
#' @param shift A number which defines a value by which to shift model predictions/partial residuals vertically. This can be necessary because \code{\link[mgcv]{plot.gam}} smooths are centred.
#'
#' @param add_rug A logical input which defines whether or not to plot a rug. If so, \code{dat}, the dataframe used to fit the model, should be provided (see above).
#' @param add_rug_args A named list of arguments passed to \code{\link[graphics]{rug}} to customise the rug.
#' @param add_moons A logical input which defines whether or not to add moons to a plot. This is useful for visualising the effects of lunar phase in models of animal movement timeseries.
#' @param add_moons_args A named list of arguments passed to \code{\link[Tools4ETS]{add_moons}}.
#'
#' @param pretty_axis_args A named list of arguments, passed to \code{\link[plot.pretty]{pretty_axis}} to customise axes().
#' @param mtext_args A named list of arguments, passed to \code{\link[graphics]{mtext}}, to add axis labels.
#'
#' @examples
#'
#' #### Example 1: Simulate a quadratic effect of variable (e.g. Julian day) on a response (e.g. depth)
#' # The function creates a plot, so we can 'play' with the parameters
#' # .... until we're happy with the shape:
#' smooth_julian_day <-
#'   parameterise_smooth(x = 0:365,
#'                       f = utils.add::quadratic,
#'                       param = list(a = -0.001, b = 1, h = 183, k = 15),
#'                       plot = TRUE)
#' # The function returns a list containing x, f, the parameters we chose and y values:
#' utils::str(smooth_julian_day)
#'
#' @author Edward Lavender
#' @export
#'


########################################
########################################
#### parameterise_smooth()

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
    add_model_predictions_args = list(),
    residuals = FALSE,
    add_residuals_args = list(),
    shift = 0,

    add_rug = FALSE,
    add_rug_args = list(),

    add_moons = FALSE,
    add_moons_args = list(),

    pretty_axis_args = list(side = c(1, 2), pretty = list(n = 10), axis = list(cex.axis = 1.5, las = TRUE)),
    mtext_args = list()

  ){



    #### Evaluate function
    if(is.null(parameterise_smooth_ls)){
      param$x <- x
      y <- do.call(f, param)
    } else{
      x <- parameterise_smooth_ls$x
      y <- parameterise_smooth_ls$y
    }

    #### Parameters if plotting a GAM
    if(plot_gam){
      model_terms <- sapply(model$smooth, function(element){return(element$term)})
      term_no <- which(model_terms == term)
      p <- plot_gam_ls[[term_no]]

      p$fit <- as.numeric(p$fit)
      p$se <- as.numeric(p$se)
      p$p.resid <- as.numeric(p$p.resid)
      if(shift != 0){
        p$fit <- p$fit + shift
        p$p.resid <- p$p.resid + shift
      }

      CI <- list_CIs(pred = list(fit = p$fit, se.fit = p$se), plot_suggestions = FALSE)
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
      pretty_axis_args$x <- list(x = pretty_axis_x, y = pretty_axis_y)
      axis_ls <- implement_pretty_axis_args(pretty_axis_args)

      #### Blank plot with parameterisation
      plot(x, y,
           type = "n",
           axes = FALSE,
           xlab = xlab, ylab = ylab,
           xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim)

      #### Clip plotting area
      usr <- graphics::par("usr")
      graphics::clip(axis_ls[[1]]$lim[1], axis_ls[[1]]$lim[2], axis_ls[[2]]$lim[1], axis_ls[[2]]$lim[2])

      #### Add parameterisation
      asa <- list(x = x, y = y)
      add_sim_args <- utils.add::list_merge(asa, add_sim_args)
      do.call(graphics::lines, add_sim_args)
      pretty_axis(axis_ls = axis_ls, add = TRUE)

      if(plot_gam){

        #### Add fitted values and CI envelope
        amp <- list(x = p$x,
                    CI = CI,
                    fCI = "poly",
                    CI_gp = list(col = scales::alpha("lightgrey", 0.8), border = FALSE),
                    add_fitted = TRUE,
                    fitted_gp = list(col = "black", lwd = 1, lty = 1)
                    )
        add_model_predictions_args <- utils.add::list_merge(amp, add_model_predictions_args)
        do.call(add_model_predictions, add_model_predictions_args)

        #### Add partial residuals
        if(residuals){
          pra <- list(x = dat[, term], y = p$p.resid)
          add_residuals_args <- utils.add::list_merge(pra, add_residuals_args)
          do.call(graphics::points, add_residuals_args)
        }
      }

      #### Add rug
      if(add_rug){
        ara <- list(x = dat[, term], pos = axis_ls[[2]]$lim[1])
        add_rug_args <- utils.add::list_merge(ara, add_rug_args)
        do.call(graphics::rug, add_rug_args)
      }

      if(add_moons){
        dam <- list(side = 3, position = axis_ls[[2]]$lim[2], outer = TRUE, nv = 100, radius1 = 0.1, units = "radians")
        add_moons_args <- utils.add::list_merge(dam, add_moons_args)
        do.call("add_moons", add_moons_args)
      }

      #### Add axis labels
      implement_mtext_args(mtext_args)

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
