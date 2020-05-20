#' @title Flag independent segments of timeseries
#' @description The function 'flags' independent sections of timeseries using up to three methods. This is useful for identifying gaps in timeseries and/or for models of timeseries data in which independent segments of timeseries need to be treated as such. To flag timeseries, the function can consider two drivers of independence: (a) a grouping factor (\code{fct}) which defines inherently independent timeseries in a dataset (e.g. a dataset may comprise timeseries for different individuals) and (b) gaps in the timeseries which, when greater than a user-defined threshold (\code{duration_threshold}), separate timeseries that can be considered effectively independent, even if they are derived from the same level of a grouping factor. Using these two criteria, timeseries can be flagged using three methods which different modelling approaches may require.
#'
#' @param x A vector of timestamps, either as an integer/numeric object, a \code{\link[base]{DateTimeClasses}} or a \code{\link[base]{Date}}.
#' @param fct A grouping factor defining independent segments of timeseries (e.g. individuals).
#' @param dat (optional) A dataframe with a column \code{x} and, optionally, a column \code{fct}, can be supplied instead of \code{x} and \code{fct}.
#' @param duration_threshold A numeric input defining the number of units (if \code{x} is an integer/numeric) or the number of minutes (if \code{x} is \code{\link[base]{DateTimeClasses}} or a \code{\link[base]{Date}}) between records after which separated timeseries are considered effectively 'independent'. This could be informed by a the autocorrelation function of the residuals of a model without any autocorrelation.
#' @param flag A numeric input/vector of \code{1}, \code{2} or \code{3} specifying the flag type to be returned. \code{flag = 1} returns a logical vector with \code{TRUE} marking the first observation in each independent segment of timeseries (e.g. as required by \code{\link[mgcv]{bam}}. \code{flag = 2} returns identifies the first, second,... nth, independent segment of each factor level's timeseries. \code{flag = 3} provides a unique identifier for each segment of timeseries (e.g. as required by \code{\link[mgcv]{gamm}}).
#'
#' @details Inputs should be ordered by ordered by \code{fct} (if applicable) then \code{x}.
#'
#' @return A dataframe with the duration between each observation and the next observation and corresponding flags.
#'
#' @examples
#'
#' #### Define some irregularly spaced timeseries
#' t <- c(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "6 hours"),
#'        as.POSIXct("2016-01-02 12:00:00"),
#'        seq.POSIXt(as.POSIXct("2016-01-02 18:00:00"), as.POSIXct("2016-01-03"), by = "5 hours")
#' )
#'
#' #### Example (1) Supply a vector of timestamps to flag independent sections of timeseries
#' flag_ts(
#'   x = t,
#'   duration_threshold = 8*60,
#'   flag = 1:3)
#'
#' #### Example (2) Supply a dataframe with a timestamp column instead
#' flag_ts(dat = data.frame(x = t),
#'         duration_threshold = 8*60,
#'         flag = 1:3)
#'
#' #### Example (3) Supply a factor level which separates unique timeseries
#' fct_levels <- c(rep(1, length(t)), rep(2, length(t)))
#' t2 <- rep(t, 2)
#' flag_ts(
#'   x = t2,
#'   fct = fct_levels,
#'   duration_threshold = 8*60,
#'   flag = 1:3)
#'
#' #### Example (4) Supply timeseries and a factor in a dataframe organised by fct then timestamp
#' flag_ts(dat = data.frame(x = t2, fct = fct_levels),
#'         duration_threshold = 8*60,
#'         flag = 1:3)
#'
#' #### Example (5) Numeric example without factor
#' x <- c(seq(1, 5, by = 1), seq(100, 105, by = 1))
#' flag_ts(x = x,
#'         fct = NULL,
#'         dat = NULL,
#'         duration_threshold = 5,
#'         flag = 1:3)
#'
#' #### Example (6) Numeric example with factor
#' fct_levels <- c(rep(1, length(x)), rep(2, length(x)))
#' x <- rep(x, 2)
#' flag_ts(x = x,
#'         fct = fct_levels,
#'         dat = NULL,
#'         duration_threshold = 5,
#'         flag = 1:3)
#'
#' #### Example (7) Numeric example with factor via dat argument
#' cbind(x,
#'       flag_ts(dat = data.frame(x = x, fct = fct_levels),
#'               duration_threshold = 5,
#'               flag = 1:3)
#' )
#'
#' @author Edward Lavender
#' @export
#'

#########################################
#########################################
#### Define function

flag_ts <-
  function(
    x,
    fct = NULL,
    dat = NULL,
    duration_threshold,
    flag = 1:3){


    #########################################
    #### Define dat if not provided.

    if(is.null(dat)){
      dat <- data.frame(x = x)
    }

    if(!is.null(fct)){
      stopifnot(length(fct) == nrow(dat))
      dat$fct <- fct
    }


    #########################################
    #### Implement flagging

    #### Order the dataframe by individual and then timestamp
    # This must be completed outside the function - otherwise, when we return the column
    # ... "start_event" the values will be in the wrong order.
    # dat <- dat %>% arrange(.data[[id_column]], .data[[timestamp_column]])

    #### Convert the inputted dataframe to a list, with a separate element
    # ... for each fct level
    if(!is.null(dat$fct)){
      dat_id_ls <- split(dat, f = dat$fct)
    } else{
      dat_id_ls <- list(dat)
    }

    #### Update dat_id_ls
    dat_id_ls <-
      # loop over every individual
      lapply(dat_id_ls, function(df){
        # Check the dataframe is sorted by time...
        if(is.unsorted(df$x)){
          stop("Inputted data ('x' (and 'fct', if applicable) or 'dat', must be ordered by 'fct', if applicable', then 'x'.")
        }
        # Use the utils.add::serial_difference function to define the duration from one timestamp to the next.
        # If timestamp input, we'll implement units = "mins"
        df$duration <- utils.add::serial_difference(df$x, units = "mins")
        # Define the positions at which duration minutes exceeds the inputted threshold
        # Add one to these positions because the serial_duration returns the duration from one position
        # ... to the next one. And we want to distinguish that next one with the previous one
        # ... in each case.
        pos <- which(df$duration > duration_threshold) + 1
        # Define a new column, start event, which will distinguish unique sections of timeseries
        # By default, this is FALSE:
        df$start_event <- FALSE
        # Set the first value for each individual to be TRUE
        # (because that value is assumed to be independent from other individuals)
        df$start_event[1] <- TRUE
        # Also define a column in which we'll define the id of each unique start event
        # by default this is one; we'll update this as necessary if there are multiple such start_events:
        df$start_event_id <- 1
        # If there are some positions for that individual in which the
        # ... gap between records exceed the duration_threshold (in minutes)...:
        lp <- length(pos)
        if(lp > 0){
          # Then, at those positions, we'll also define TRUE
          # ... to make sure they are recognised as unique positions
          df$start_event[pos] <- TRUE
          # For each pos value...
          for(i in 1:lp){
            if(i <= (lp-1)){
              nrw <- pos[i + 1] - pos[i]
            } else if(i == lp){
              nrw <- nrow(df) - pos[i]
            }
            # from that pos value until the position next one
            # ... we'll give a unique id
            # (starting at i + 1 = 2... because we've already dealt with the first one above)
            df$start_event_id[pos[i]:(pos[i]+nrw)] <- i+1
          }

        } # close if(length(pos) > T){
        # return the df
        return(df)
        # close function, lapply,
      })

    #### Define an overall dataframe
    dat <- dplyr::bind_rows(dat_id_ls)

    #### Define flag1, a vector of start events
    # (defined above)

    #### Define flag2, a vector of ids
    # (defined above)

    #### Define flag3, a vector of gamm_seids
    # Define a new column:
    dat$gamm_seid <- 0
    # Define the positions of start events:
    start_events <- which(dat$start_event)
    # Add the total number of rows to this:
    start_events <- c(start_events, nrow(dat))
    # For each position....
    for(i in 1:(length(start_events)-1)){
      # print(i)
      # print(i)
      #  set the rows from that position to the next start event
      #  ... or, in the case of the final true position, to the number of rows in the df,
      # to be equal to i.
      dat$gamm_seid[start_events[i]:start_events[i + 1]] <- i
    }
    # Set as a factor
    dat$gamm_seid <- factor(dat$gamm_seid)


    #########################################
    #### Return and close function

    if(!is.null(dat$fct)){
      flag_df <- data.frame(fct = dat$fct, duration = dat$duration)
    } else{
      flag_df <- data.frame(duration = dat$duration)
    }

    if(1 %in% flag){ flag_df$flag1 = dat$start_event}
    if(2 %in% flag){ flag_df$flag2 = dat$start_event_id }
    if(3 %in% flag){ flag_df$flag3 = dat$gamm_seid }

    # return flag_df:
    return(flag_df)

   #### close function
  }

#### End of function.
#########################################
#########################################
