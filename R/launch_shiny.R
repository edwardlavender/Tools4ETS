#' @title Launch one of Tools4ETS's Shiny applications
#' @description A wrapper function for Shiny applications which enables these to be launched using a common framework.
#'
#' @param app A character vector defining the name of the application to be launched. The currently supported options are: \code{"vis_ts"}, which calls \code{\link[Tools4ETS]{vis_ts}}; \code{"GAMS4DTS"}, which calls \code{\link[Tools4ETS]{GAMS4DTS}}.
#' @param data A dataframe passed to \code{\link[Tools4ETS]{vis_ts}}. As a minimum, this should includes a column defining timestamps.
#'
#' @return An interactive Shiny-Dashboard interface.
#'
#' @examples
#'
#' #### Example (1) vis_ts
#' \dontrun{
#' launch_shiny("vis_ts", data = dat_flapper)
#' }
#'
#' #### Example (2) GAMS4DTS
#' \dontrun{
#' launch_shiny("GAMS4DTS")
#' }
#'
#' @author Edward Lavender
#' @export
#'
#'

#######################################
#######################################
#### launch_shiny()

launch_shiny <- function(app, data = NULL){

  apps <- c("vis_ts", "GAMS4DTS")
  stopifnot(app %in% apps)

  if(app == "vis_ts"){
    stopifnot(!is.null(data))
    return(vis_ts(data = data))
  }

  if(app == "GAMS4DTS"){
    GAMS4DTS()
  }

}

#### End of function.
#######################################
#######################################
