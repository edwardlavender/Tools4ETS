#' @title Explore potential, unrecorded recapture events within depth timeseries
#' @description This function highlights potential recapture events on unknown dates based on a depth threshold; for each day in the depth timeseries, all the timestamps in which the individual was above this depth are considered possible recapture events. These timestamps can be saved by the function and inspected graphically.
#'
#' @param data A dataframe with two named columns: 'timestamp' (in POSIXct format) and 'depth' (of a given individual).
#' @param threshold_depth A numeric value which defines the depth threshold; i.e. the depth such that when the animal is at or above (shallower) than this depth, a recapture event may have occurred and should be checked.
#' @param plot A logical input which defines whether or not to create a plot.
#' @param window A numeric value which defines the number of seconds either side of first and last potential 'recapture' event on any given day for which the depth timeseries is plotted. The default is 43200 s (i.e. 12 hours).
#' @param prompt A logical value which defines whether or not to pause following the display of each graph. If TRUE, the user needs to press Enter prior to moving on to the next recapture event.
#' @param xlab A label for the x axis.
#' @param ylab A label for the y axis.
#' @param ndates_warning A numeric value defining the number of dates with putative recapture events above which the function will warn the user to reconsider \code{prompt = TRUE}.
#' @param ... Extra arguments passed to \code{\link[graphics]{plot}}, excluding \code{xlim} which is set based on the window argument provided.
#'
#' @return A dataframe with 4 columns: 'timestamp' (as inputted), 'depth' (as inputted), 'recapture_window' (a logical value specifying whether the position is inside a recapture window, defined by \code{window}), and 'above_depth_threshold' (a logical value which specifies the positions at which the depth is shallower than the depth threshold). A plot can also be returned, with depth (negated) shown over time. Red lines show the recapture window under consideration and blue lines show any possible moments of recapture in this window.
#'
#' @author Edward Lavender
#'
#' @examples
#'
#' #### Example (1): Suggest recapture events for a single individual
#' suggested_recap <-
#'   suggest_recapture(data = dat_flapper[dat_flapper$id == "A", ],
#'                     threshold_depth = 1,
#'                     plot = TRUE,
#'                     window = 60 * 60 * 12,
#'                     xlab = "Timestamp",
#'                     ylab = "Depth",
#'                     prompt = FALSE,
#'                     ndates_warning = 100,
#'                     type = "l"
#'   )
#' utils::str(suggested_recap)
#'
#' #### Example (2) Suggest recapture events for multiple individuals by implementing lapply()
#' dat_flapper_ls <- split(dat_flapper, f = dat_flapper$id)
#' suggested_recap_ls <-
#'   lapply(dat_flapper_ls, function(df){
#'     suggested_recap <-
#'       suggest_recapture(data = df,
#'                         threshold_depth = 1,
#'                         plot = TRUE,
#'                         window = 60 * 60 * 12,
#'                         xlab = "Timestamp",
#'                         ylab = "Depth",
#'                         prompt = FALSE,
#'                         ndates_warning = 100,
#'                         type = "l"
#'       )
#'     return(suggested_recap)
#'   })
#' utils::str(suggested_recap_ls)
#'
#' @export
#'


##########################################################
##########################################################
#### suggest.recapture

suggest_recapture <-
  function(data,
           threshold_depth = 1,
           plot = TRUE,
           window = 60*60*12,
           xlab = "Timestamp",
           ylab = "Depth",
           prompt = TRUE,
           ndates_warning = 100,...){

    #### Define dataframe to be returned
    dat <- data
    dat$above_depth_threshold <- FALSE
    dat$recapture_window <- FALSE

    #### Define dates
    tz <- attributes(data$timestamp)$tzone
    data$date <- as.Date(data$timestamp, tz = tz)

    #### Define positions where depth is < threshold
    pos_less_than_thresh <- which(data$depth < threshold_depth)
    times_less_than_thresh <- data$timestamp[pos_less_than_thresh]
    dates_less_than_thresh <- unique(as.Date(times_less_than_thresh, tz = tz))
    ldates <- length(dates_less_than_thresh)

    #### If there are dates when depth is less than the threshold...
    if(ldates > 0){

      #### Warning about prompt based on the number of dates
      if(prompt){
        if(ldates > ndates_warning){
          adj_prompt <-
            utils::askYesNo(paste0("The number of dates with recapture events is ",
                                   ldates,
                                   ". Do you want to change prompt = TRUE to prompt = FALSE?"))
          if(adj_prompt){
            prompt <- FALSE
          }
        }
      }

      #### Loop over each date and create a plot, if requested
      if(plot){
        out <-
          lapply(dates_less_than_thresh, function(dd){

            #### Define positions of potential recaptures on that date
            pos <- which(data$date == dd & data$depth < threshold_depth)

            #### Define graph limits
            x <- data$timestamp[pos]
            xlim <- range(x)
            xlim[1] <- xlim[1] - window
            xlim[2] <- xlim[2] + window

            #### Redefine pos for plotting
            pos2plot <- which(data$timestamp >= xlim[1] & data$timestamp <= xlim[2])
            x2plot <- data$timestamp[pos2plot]
            xlim_old <- xlim
            xlim <- range(x2plot)
            y2plot <- data$depth[pos2plot]

            # Plot a graph
            plot(x2plot, y2plot*-1,
                 xlim = xlim, xlab = xlab, ylab = ylab,...)

            # Add lines demarking each recapture event
            graphics::abline(v = x, col = "blue", lty = 2)
            # Add lines delimiting the window
            graphics::abline(v = xlim_old, col = "red", lty = 3)
            # Add lines delimiting the day
            # dd2 <- dd + 1
            # dd1 <- as.POSIXct(dd, tz = tz)
            # dd2 <- as.POSIXct(dd2, tz = tz)
            # graphics::abline(v = as.numeric(c(dd1, dd2)), col = "black")

            if(prompt){
              readline(prompt = "Press [enter] to move on to the next date with possible recapture event(s) or [Esc] to exit...")
            }

          }) # close lapply()
      } # close if(plot)

      #### Update dataframe
      dat$above_depth_threshold[pos_less_than_thresh] <- TRUE
      pos_within_window <- lapply(times_less_than_thresh, function(t){
        pww <- which(dat$timestamp >= (t - window) & dat$timestamp <= (t + window))
        return(pww)
        })
      pos_within_window <- as.vector(unlist(pos_within_window))
      dat$recapture_window[pos_within_window] <- TRUE

    } # close if(ldates > 0)

    #### Return dataframe
    return(dat)

  } # close function


#### End of function.
##########################################################
##########################################################
