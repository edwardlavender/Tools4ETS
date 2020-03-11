#' @title Compute confidence intervals around an autocorrelation function (ACF)
#' @description This function calculates confidence intervals around an ACF. This represents the upper limit of autocorrelation in white noise. This is useful for producing pretty plots.
#'
#' @param acf An ACF object, produced by \code{\link[stats]{acf}}.
#' @param ci An numeric value between 0 and 1 which defines the confidence interval (CI) to be calculated. The default is 0.95; i.e, a 95 percent CI.
#' @param return A character input specifying \code{"upper"} or \code{"CI"}. If \code{return = "upper"}, a single number is returned which is the upper 95 percent confidence limit of the extent of autocorrelation in a sample of white noise. If \code{return = "CI"} a list comprising \code{CI$fit (0)}, \code{CI$lowerCI} (the lower CI) and \code{CI$upperCI} (the upper CI) is returned. This can be plotted by \code{\link[plot.pretty]{add_model_predictions}}.
#'
#' @return The function returns a number or a list, depending on the input to \code{return} (see above).
#'
#' @seealso \code{\link[stats]{acf}}, \code{\link[plot.pretty]{add_model_predictions}}
#'
#' @author Edward Lavender
#' @export
#'

###########################################
###########################################
#### acf_in_white_noise()

acf_in_white_noise <-
  function(
    acf,
    ci = 0.95,
    return = "upper"){

    #### Checks
    stopifnot(return %in% c("upper", "CI"))

    #### Compute white noise upper limits
    wn_upper <- stats::qnorm((1 + ci)/2)/sqrt(acf$n.used)
    if(return == "upper"){
      return(wn_upper)
    }

    #### Define CI list
    CI <- list(fit = c(0, 0), lowerCI = rep(0 - wn_upper, 2), upperCI = rep(0 + wn_upper, 2))
    if(return == "CI"){
      return(CI)
    }
  }


#### End of code.
###########################################
###########################################
