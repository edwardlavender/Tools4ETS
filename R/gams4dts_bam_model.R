################################################
################################################
#### gams4dts_bam_model()

#' @title Evaluate an \code{mgcv::bam()} model for GAMS4DTS
#' @import mgcv
#'
#' @description This function is a wrapper for \code{\link[mgcv]{bam}} which passes user-defined inputs from \code{\link[Tools4ETS]{GAMS4DTS}} to \code{\link[mgcv]{bam}}. The function is not intended to be called outside of \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param f A formula (see \code{\link[mgcv]{bam}}).
#' @param likelihood A list specifying the likelihood distribution and link function.
#' @param rho rho (see \code{\link[mgcv]{bam}}).
#' @param knots knots (see \code{\link[mgcv]{bam}}).
#' @param gamma gamma (see \code{\link[mgcv]{bam}}).
#' @param dat A dataframe containing the data used to fit the model (see \code{\link[mgcv]{bam}}).
#'
#' @return An \code{\link[mgcv]{bam}} model object.
#' @author Edward Lavender
#'

gams4dts_bam_model <-
  function(
    # define the formula
    f,
    # define the likelihood (distribution and link function)
    likelihood = list(distribution = "gaussian",
                      link = "identity"),
    # define the rho value (default is 0)
    rho = 0,
    # If rho > 0, the assumption is that there is a
    # ...column in the dataframe dat$start_event which contains details of start events
    # define knots
    knots,
    # gamma value
    gamma = 1,
    # define the data
    dat){

    #### run the model
    model <- eval(bquote(bam(.(f),
                             # There is assumed to be a column called start_event
                             # ... in the dataframe. This must be defined within the formula environment;
                             # see bam.formula.define()
                             AR.start = dat$start_event,
                             rho = rho,
                             gamma = gamma,
                             family = eval(parse(text = likelihood$distribution))(link = likelihood$link),
                             knots = knots,
                             data = dat)))

    #### return model
    return(model)

    # close function
    }



#### End of code.
################################################
################################################
