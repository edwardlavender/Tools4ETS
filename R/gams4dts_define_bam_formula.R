#' @title Define an \code{mgcv::bam()} formula in GAMS4DTS
#' @description This function is used to pass user-defined variables in \code{\link[Tools4ETS]{GAMS4DTS}} into an \code{\link[mgcv]{bam}} formula. The function is not intended to be called outside of \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param response A character input specifying the name of the response variable in \code{dat} (i.e. \code{"depth"}).
#' @param fixed_covariates A list containing covariates implemented as fixed effects.
#' @param smooth_covariates A list containing smooth covariates, including their names and basis functions.
#' @param dat A dataframe.
#'
#' @returns An \code{\link[mgcv]{bam}} model formula for GAMS4DTS.
#'
#' @author Edward Lavender
#'

################################################
################################################
#### gams4dts_define_bam_formula()

# define a model object from a statement about covariates
gams4dts_define_bam_formula <-
  function(
    # define the repsonse variable
    response = "depth",
    # define the fixed covariates in a list: sex = list(), length = list()
    fixed_covariates,
    # define the smooth covariates in a list, including their names and basis functions
    smooth_covariates,
    # data frame
    dat
    # close inputs and begin functions
    ){


    ################################################
    #### Error checking/ warnings for the user:

    # prevent sex from being inputed into the formula
    # ... if there is only one sex in the dataframe:
    if("sex" %in% names(fixed_covariates)){
      if(length(unique(dat$sex)) < 2){
        stop("There is only one sex in the dataframe. Do not input 'sex' as a fixed effect.")
      }
    }

    # prevent length from being inpuuted if there is only one individual
    if("length" %in% names(smooth_covariates)){
      if(length(unique(dat$individual)) < 2){
        stop("There is only one individual in the dataframe. Do not input 'length' as a covariate.")
      }
    }

    # prevent individual from being inputed as a random effect if there is only one individual:
    if("individual" %in% names(smooth_covariates)){
      if(length(unique(dat$individual)) < 2){
        stop("There is only one individual in the dataframe. Do not input 'individual' as a random intercept.")
      }
    }


    ################################################
    #### AR.start

    # We need to define AR.start within the environment of the formula
    # ... to stop an errors later on when we fit the model:
    # Error in model.frame.default: variable lengths differ (found for '(AR.start)')
    # see also bam.model()
    AR.start <- dat$start_event



    ################################################
    #### build the formula

    # fixed effect component of formula
      if(length(fixed_covariates)>0){
        f_fixed <- paste(paste(names(fixed_covariates), collapse = " + "), "+ ")
      } else{
        f_fixed <- ""
      }

    # smooth effects component of formula
    f_smooth <-
      paste0(
        sapply(names(smooth_covariates), function(covariate){
          smooth <-
            paste0(
              "s(", covariate,
              ", bs = ", paste0("'", smooth_covariates[[covariate]]$bs, "'"),
              ", k = ",  smooth_covariates[[covariate]]$k,
              ")"
              )
          }),
        collapse = " + "
        )

    # full right hand side of formula
    rhs <- paste0(f_fixed, f_smooth)

    # full formula as a string
    f_full <- paste(response, " ~ ", rhs)

    # convert to a formula object
    f <- stats::as.formula(f_full)



    ################################################
    #### return formula and close function

    # return the formula
    return(f)

    # close function
    }



#### End of code.
################################################
################################################
