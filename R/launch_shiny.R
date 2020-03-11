#' @title Launch one of Tools4ETS's Shiny applications
#' @description A wrapper function for Shiny applications which enables these to be launched using a common framework.
#'
#' @param app A character vector defining the name of the application to be launched. The currently supported options are: \code{"visTS"}, which calls \code{\link[Tools4ETS]{visTS}}; \code{"GAMS4DTS"}, which calls \code{\link[Tools4ETS]{GAMS4DTS}}.
#' @param data A dataframe passed to \code{\link[Tools4ETS]{visTS}}. As a minimum, this should includes a column defining timestamps.
#'
#' @return An interactive Shiny-Dashboard interface.
#'
#' @examples
#'
#' #### Example (1) visTS
#' \dontrun{
#' launch_shiny("visTS", data = dat_flapper)
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

  apps <- c("visTS", "GAMS4DTS")
  stopifnot(app %in% apps)

  if(app == "visTS"){
    stopifnot(!is.null(data))
    return(visTS(data = data))
  }

  if(app == "GAMS4DTS"){
    GAMS4DTS()
  }

}

#### End of function.
#######################################
#######################################
