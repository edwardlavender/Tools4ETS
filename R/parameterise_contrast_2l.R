#' @title Define and visualise a contrast between two factor levels
#' @description This function is used to define and visualise a contrast between two factor levels (e.g. the difference in depth between males and females). Simulated contrasts can be compared to estimated contrasts from an \code{mgcv} generalised additive model (GAM). The function is intended primarily for use behind-the-scenes in \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param x A vector of two factor levels for which to define a contrast.
#' @param param A numeric input which defines the size of the contrast between two factor levels.
#' @param plot_gam A logical input which defines whether or not to add model estimates to the plot.
#' @param dat The dataframe used to fit the model.
#' @param model The model.
#' @param term A character input which defines the name of the term in the model.
#' @param cex.axis A numeric input which defines the size of the font for axis tick labels.
#' @param mtext_args A named list of arguments which are passed to \code{\link[graphics]{mtext}} to produce axis labels. A nested list can be used to control each axis (see also \code{\link[Tools4ETS]{plot_ts}}).
#'
#' @return The function returns a plot showing the size of the contrast between factor levels, possibly in relation to that estimated by an \code{mgcv} GAM model.
#'
#' @author Edward Lavender
#' @export
#'

########################################
########################################
#### parameterise_contrast_2l()

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
  implement_mtext_args(mtext_args)

  if(plot_gam){
    graphics::points(c(1, 2), fits, pch = 21, bg = "darkgrey", col = "darkgrey")
    graphics::segments(x0 = 2, y0 = se_lower, y1 = se_upper,
                       col = scales::alpha("darkgrey", 0.4),
                       lwd = 2,
                       lty = 1)
  }

  # close the function
}



#### End of code.
################################################
################################################
