#' @title Identify and describe recapture events from depth time series on known dates
#' @description Identifying 'recapture' events in depth time series (i.e. moments when electronically tagged animals are (re)caught, brought to the surface and possibly released again) is often useful. This function uses a (semi-)automatic approach to define the precise time of recapture on a set of pre-known dates when the individual was recaptured; note that this assumes the individual was only caught once on any given date. (Identifying recapture events without pre-defined windows in which these are known to have occurred is challenging, but \code{\link[Tools4ETS]{suggest_recapture}} implements one approach.) The function can use three methods to define likely times of recapture on inputted dates. Under the automatic approach, the time of recapture identified by the first method is taken as correct and saved, along with all of the positions in a user-defined window around this recapture time that encapsulate the recapture event. The user can inspect a plot of depth ~ time to visually assess the performance of these algorithms. Under the semi-automated approach, the user can use point-and-click on the plot to re-define the time of recapture, which is often difficult to define exactly without manual input (although the algorithm will usually come close), and/or other time stamps during the recapture process which are biologically important. The function returns a list which includes the estimated times of recapture events and, depending on user inputs, may include a processed dataframe from which all positions around estimated recapture positions have been removed.

#' @param data_depth A dataframe in which recapture events need to be identified based on depth or temperature time series. This must contain the following named columns: a unique identifier for each individual ('id'); time stamps in POSIXct format ('timestamp'); depth ('depth'); and (optionally) temperature ('temp'). Note that depth must be a positive number.
#' @param data_recapture A dataframe which specifies the dates on which recapture events are known to have occurred, but for which the precise timing needs to be resolved. This must contain the following named columns: a unique identifier for each individual ('id'), that matches IDs in \code{data_depth}, and the date ('Date').
#' @param method A numeric vector that defines the method(s) used to define the timing of recapture events: \code{1}, identifies the time of minimum depth (i.e. the shallowest moment); \code{2}, identifies the time preceding the highest average decrease in depth (i.e., movement shallower); and \code{3} specifies the time of maximum temperature. See Details for further information.
#' @param bin A string that defines the duration of the bin over which the mean change in depth is calculated. This is required if method 2 is included. The default is \code{"10 mins"}.
#' @param bound_am A numeric value that defines the number of seconds after midnight on any given day before which a 'recapture event' is implausible. The default is 28800 (i.e., any recapture event identified before 08:00 hours on a given day will be flagged with a warning indicating the identified time of recapture is unlikely).
#' @param bound_pm A numeric value, as above, which defines the number of seconds on any given day after which a 'recapture event' is considered implausible. The default is 61299 (i.e., any recapture event identified after 17:00 hours on a given day will be flagged with a warning).
#' @param define_time_before A function which takes a single argument (the recapture time, in POSIXct format) and outputs a time (in POSIXct format) which defines the lower bound of a recapture window. The default is a function which returns a time 2 hours before a given recapture window.
#' @param define_time_after A function, as above, which defines the upper bound of a recapture window. The default is a function which returns midnight on the day of recapture.
#' @param inspect_plot A logical input which defines whether or not to create a plot of depth (negated) ~ time stamp, with the recapture window and the identified time(s) of recapture highlighted. This is useful for visualising the performance of the default options for defining the time of recapture.
#' @param before4plot A numeric input defining the number of seconds before the recapture time, estimated using the first method inputted, which is used to define the lower x limit of the plot. The default is 3600 (i.e. the time series plot will begin from one hour before the estimated recapture time).
#' @param after4plot A numeric input, as above, used to define the upper x limit of the plot. The default is 3600 (i.e. the time series plot will extend until one hour after the estimated recapture time).
#' @param ylim A numeric vector of two which specifies the y limits of the plot. Note that depth is negated for plotting purposes (see \code{inspect_plot}), so \code{ylim} needs to be negated too. The default is \code{ylim = c(-250, 0)}.
#' @param vcols A vector of colours. The recapture time estimated by each method is added to the time series plot as a vertical line, with the specified colour.
#' @param type,pch,col,bg,cex,... Additional graphical customisation options passed to \code{\link[prettyGraphics]{pretty_plot}}, excluding \code{xlim}, \code{pretty_axis_args} and \code{main} which are handled internally.
#' @param prompt A logical input which defines whether or not to pause following the creation of a plot (see above). If \code{prompt = TRUE}, the function proceeds once the user has digested the plot and pressed Enter. Otherwise, the user can press Esc to end the algorithm.
#' @param manual_flag A logical input which defines whether or not to manually flag points on the plot. This is useful because the function's estimate of the recapture time may not be exactly correct and may need to be refined by the user, and because the user may want to define flag points in the recapture window which represent the times of biologically important events. If \code{TRUE}, the function pauses whilst the user clicks points on the plot to be saved.
#' @param select A numeric value which, if \code{manual_flag = TRUE}, specifies the position of the click which defines the exact time of recapture. (This is useful if the user uses multiple clicks to define important points in the recapture window, only one of which represents the 'moment' of recapture.)
#' @param remove_recaptures A logical input defining whether or not to remove identified recaptures (either based on the first input method or, if \code{manual_flag = TRUE}, the time defined by the click number given by the \code{select} argument). If \code{TRUE}, the function will also return a vector of all the positions in recapture windows and a reduced dataframe (\code{data_depth_rem_recap}) in which these have been removed.
#'
#' @details Three methods can be used to define the time of recapture on pre-specified dates. The first method identifies the time when depth is shallowest. This method is effective if (a) depth data are accurate in shallow water and (b) individuals spent a relatively short amount of time at the surface. However, if depth data are inaccurate in shallow water, then the time when depth is shallowest may not actually reflect the moment of recapture. Moreover, if individuals spend a long time at/near the surface, the identified moment of recapture using this method may be too late. The second method identifies the time when the change in depth is greatest. To do this, the function calculates the change in depth between sequential observations and then the average change in each user-specified bin (e.g. 10 mins). This method can be more effective than the first window in cases where individuals are rapidly brought to the surface. The third method identifies the time of maximum temperature. This method can be effective in situations in which temperature data are more accurate than depth data and spikes in temperature are expected with movement from deeper water into shallower water/air. Note that this is not necessarily always the case; some environments lack strong thermoclines and there may be times and/or areas in which an individual could rise through the water column and/or into air and experience a decline, increase or no change in temperature. Hence, implementing all three methods is often beneficial. Each method will usually identify a similar time window, but the exact time of recapture can be adjusted by the user. Otherwise, the time identified by the first method is used (e.g. to define all the observations within the user-specified recapture window).
#'
#' @return A list with the following elements: (1) 'recapture_event_details' (a list); (2) 'recapture_times' (a dataframe); and, if the user has selected to remove recaptures, (3) 'recapture_rows' (a numeric vector) and (4) 'data_depth_rem_recap' (a dataframe). (1) 'recapture_event_details' is a list which contains an element for each recapture event. Each element is itself a list which contains: (a) 'data_recapture_event', the data row from \code{data_recaptures} which describes the ID/time stamp of the event in question; (b) 'rc_time_ls', a list defining the estimated time of recapture by each method; (c) 'rc_time', a list containing the chosen estimated time of recapture, the method used (1, 2, 3 or manual) and, if the manual method is used, the click number which was used to define the time of recapture); (d) 'reasonable_bounds', a list containing the time window within which recapture events are considered plausible and outside of which a warning is flagged; (e) 'recapture rows', a numeric vector which defines all the positions in \code{data_depth} within the user-defined recapture window around the chosen estimated time of recapture for the individual in question. If no data is associated with a recapture event, a warning is given and other elements are NA/NULL. (2) 'recapture_times' is a dataframe which, for each row in 'data_recapture' (i.e., individual and date of recapture) specifies the time stamp of each recapture event and the method used (i.e., this is a synthesis of the information provided by each element in 'recapture_event_details'). (3) 'recapture_rows_all' is a numeric vector which specifies all the positions in \code{data_depth} which lie within recapture windows. (4) 'data_depth_rem_recap' is the \code{data_depth} dataframe with these positions removed.
#'
#' @examples
#'
#' #### Method
#' # We will precisely identify recapture events in the two example flapper skate time series,
#' # ... based on knowledge of the following recapture dates:
#' # Individual A was recaught on "2016-05-16"
#' # Individual B was recaught on "2016-05-10"
#'
#' #### Pre-processing
#' # dat_flapper has all required columns except date:
#' dat_flapper$date <- as.Date(dat_flapper$timestamp)
#' # define dates of recapture for data_recapture argument:
#' dat_recap <- data.frame(id = c("A", "B"), date = as.Date(c("2016-05-16", "2016-05-10")))
#'
#' #### Example (1): Define recaptures for two individuals on known dates
#' define_recap <-
#'   define_recapture(
#'     # Supply depth time series and recapture dataframes
#'     data_depth = dat_flapper,
#'     data_recapture = dat_recap,
#'     # Implement method 1:3
#'     method = 1:3,
#'     # bin is required for method 2
#'     bin = "10 mins",
#'     # Define reasonable boundaries
#'     bound_am = 3600*8,
#'     bound_pm = 3600*17,
#'     # We'll consider a recapture window between 1 hour before and after the estimated time
#'     define_time_before = function(recapture_time) { recapture_time - 3600 },
#'     define_time_after = function(recapture_time) { recapture_time + 3600 },
#'     # We'll examine the plot with default graphical parameters:
#'     inspect_plot = TRUE,
#'     before4plot = 3600,
#'     after4plot = 3600,
#'     vcols = c("black", "green", "blue"),
#'     # prompt = TRUE and manual_flag are advisable, but we'll turn them off here
#'     # ... for the purposes of illustration
#'     prompt = FALSE,
#'     manual_flag = FALSE,
#'     # Select is ignored because manual_flag = FALSE but we'll remove recapture
#'     # ... windows around the estimated recapture time
#'     select = 1,
#'     remove_recaptures = TRUE
#'     )
#' # The function returns multiple outputs
#' summary(define_recap)
#' # We have a list of recapture details for each recapture event
#' utils::str(define_recap$recapture_event_details)
#' # We have a dataframe defining the precise times of recapture
#' utils::str(define_recap$recapture_times)
#' # We have all the positions in the raw data (data_depth) with lie within recapture windows
#' utils::str(define_recap$recapture_rows_all)
#' # We also opted to return a reduced dataframe without those positions:
#' utils::str(define_recap$data_depth_rem_recap)
#'
#' @author Edward Lavender
#' @export



###################################################
###################################################
#### define_recapture()

define_recapture <-
  function(
    data_depth,
    data_recapture,
    method = 1:3,
    bin = "10 mins",
    bound_am = 60*60*8,
    bound_pm = 60*60*17,
    define_time_before = function(recapture_time){ recapture_time - 2*60*60 },
    define_time_after = function(recapture_time){ as.POSIXct(as.Date(recapture_time) + 1, tz = "UTC") },
    inspect_plot = TRUE,
    before4plot = 3600,
    after4plot = 3600,
    ylim = c(-250, 0),
    vcols = c("black", "green", "blue"),
    type = "b", cex = 0.5, pch = 21, col = "black", bg = "black",
    prompt = TRUE,
    manual_flag = TRUE,
    select = 1,
    remove_recaptures = FALSE,
    ...
  ){


    ###################################################
    #### Data preprocessing

    # Define stage
    cat("Step 1: Setting up function....\n")

    # New columns in data_depth required for algorithm
    data_depth$date <- as.Date(data_depth$timestamp, tz = attributes(data_depth$timestamp)$tzone)
    if(2 %in% method){
      data_depth$time_bin <- cut(data_depth$timestamp, bin)
      data_depth$time_bin <- as.character(data_depth$time_bin)
      data_depth$time_bin_depth_change <- NA
      data_depth$va <- NA
    }

    # Recaptures list
    data_recapture_ls <- split(data_recapture, f = 1:nrow(data_recapture))


    ###################################################
    #### Loop over each recapture event

    #### Loop over each row in the recaptures dataframe...
    cat("Step 2: Moving over each row in the data_recapture dataframe for inspection...\n")
    out <- pbapply::pblapply(data_recapture_ls, cl = NULL, function(recap){

      #### Testing
      # recap <- data_recapture_ls[[1]]

      #### Define a blank list in which we'll store all the outputs for each recapture event
      recap_ls <- list()
      recap_ls$data_recapture_event <- NULL
      recap_ls$rc_time_ls <- NULL
      recap_ls$reasonable_bounds <- NULL
      recap_ls$rc_time <- list()
      recap_ls$rc_time$time <- NA # use NA to avoid errors at the bottom when we define a df for all individuals
      recap_ls$rc_time$method <- NA
      recap_ls$recapture_rows <- NULL
      recap_ls$manual_flag_timestamps <- NULL
      recap_ls$warning <- NULL

      #### Add recapture information
      recap_ls$data_recapture_event <- recap

      #### Define the rows in data_depth are on the same day as the recapture event for that individual
      pos_data_depth <- which(data_depth$id == recap$id & data_depth$date == recap$date)
      data_depth_recap <- data_depth[pos_data_depth, ]

      #### If there are data associated with this event
      if(length(pos_data_depth) > 0){


        ###################################################
        #### Define the timing of recaptures

        #### Determine the time of recapture using each method
        rc_time_ls <- lapply(method, function(methodID){
          ## (1) minimise depth
          if(1 == methodID){
            # Define the time which minimises the depth (depth must not be 'negative')
            rc_time_depth_max <- data_depth_recap$timestamp[which.min(data_depth_recap$depth)]
            return(rc_time_depth_max)
          ## (2) maximise vertical activity in a timebin
          } else if(2 == methodID){
            # maximising the depth doesn't always work perfectly...
            # ... e.g. some individuals show a sharp increase in depth and then a tail and the
            # ... max depth is along this tail, so simply using the max depth isn't great.
            # ... so we'll also identify the time of a sharp increase in depth,
            # ... compare this to the time above and select the minimum
            # calculate the average depth change in each 10 min bin...
            data_depth_recap$va <- data_depth_recap$depth - dplyr::lag(data_depth_recap$depth)
            av_depth_change <- tapply(data_depth_recap$va, data_depth_recap$time_bin, mean)
            rc_time_max_av_depth_change <- names(av_depth_change)[which.min(av_depth_change)] # (technically most negative depth change)
            rc_time_max_av_depth_change <- as.POSIXct(rc_time_max_av_depth_change, tz = "UTC")
            return(rc_time_max_av_depth_change)
          ## (3) maximise temp
          } else if(3 == methodID){
            rc_time3 <- data_depth_recap$timestamp[which.max(data_depth_recap$temp)]
            return(rc_time3)
          }
        }) # close lapply() over each method

        # Update names
        names(rc_time_ls) <- paste0("method", method)
        # Add to recap_ls()
        recap_ls$rc_time_ls <- rc_time_ls

        #### Print a warning if the recapture time is outside reasonable bounds
        # print warning if the rc_time is not at a likely moment in time.
        reasonable1  <- as.POSIXct(recap$date, tz = attributes(recap$date)$tzone) + bound_am
        reasonable2 <- as.POSIXct(recap$date, tz = attributes(recap$date)$tzone) + bound_pm
        recap_ls$reasonable_bounds <- list(times = c(reasonable1, reasonable2))
        # treat rc_time estimated by first method as example
        rc_time <- rc_time_ls[[1]]
        # add to list for now; this will be updated later if manual_flag
        recap_ls$rc_time = list(time = rc_time, method = 1)
        if(rc_time <= reasonable1 | rc_time >= reasonable2){
          # Define warning
          main <- paste("\n The estimated time of recapture,", rc_time,
                        ", is outside reasonable bounds defined by", reasonable1, "and", reasonable2,
                        ", according to selected method (used to check feasibility).")
          # Add to list
          recap_ls$reasonable_bounds$warning <- main
          # Show warning
          warning(main, immediate. = TRUE)
        } else{main <-  ""}



        ###################################################
        #### Define approximate recapture window based on recapture time defined by the 1st method

        # Define approx recap window
        # using define_time_before() and define_time_after() functions supplied
        # that contains the recapture_rows
        # ... used for plot below and represents recapture rows based on first method
        # ... if manual_flag == FALSE
        pos_approx_recap_window <-
          which(data_depth$id == recap$id &
                  data_depth$date == recap$date &
                  data_depth$timestamp >= define_time_before(recapture_time = rc_time) &
                  data_depth$timestamp <= define_time_after(recapture_time = rc_time)
                )
        # Add to list for now; this will be updated later
        # ... if manual_flag == TRUE
        recap_ls$recapture_rows <- pos_approx_recap_window


        ###################################################
        #### Plot the time series

        if(!inspect_plot & manual_flag){
          warning(paste0("inspect_plot = ", inspect_plot, " so manual_flag = ", manual_flag, " is ignored."))
        }

        #### If the user has selected to inspect a plot...
        if(inspect_plot){
          check...(c("xlim", "main", "pretty_axis_args"),...)
          # Message
          # print("Manually check the graph. The red lines enclose the approximate recapture window around the time of recapture estimated by the first inputted method. Each line corresponds with the time of recapture estimated by one method. Ensure that the recapture time (this should be in the daytime!) and interval are suitably defined; the precise timing of recapture can then be determined manually...")

          # Define plotting window around estimated recapture time.
          # rows_to_plot <- (min(pos_approx_recap_window) - before4plot):(max(pos_approx_recap_window) + after4plot)
          # rows_to_plot <- rows_to_plot[which(data_depth$id[rows_to_plot] == recap$id)]
          start_recap <- data_depth[min(pos_approx_recap_window), "timestamp"]
          end_recap <- data_depth[max(pos_approx_recap_window), "timestamp"]
          rows_to_plot <- which(data_depth$id == recap$id &
                                  data_depth$timestamp >= (rc_time - before4plot) &
                                  data_depth$timestamp <= (rc_time + after4plot)
                                )
          # rows_to_plot <- (rows_to_plot - before4plot):(rows_to_plot + after4plot)
          # rows_to_plot <- rows_to_plot[which(data_depth$id[rows_to_plot] == recap$id)]
          xlim <- range(data_depth$timestamp[rows_to_plot])
          dat2plot <- data_depth[rows_to_plot, c("timestamp", "depth")]
          dat2plot$depth <- dat2plot$depth * -1
          # Define plot
          prettyGraphics::pretty_plot(dat2plot$timestamp, dat2plot$depth,
                                      pretty_axis_args = list(side = 3:2, control_axis = list(las = TRUE)),
                                      xlim = xlim,
                                      ylim = ylim,
                                      main = paste0("ID ", recap$id, " (", recap$date, ")"),
                                      type = type, cex = cex, pch = pch, col = col, bg = bg,
                                      ...)
          graphics::lines(rep(start_recap, 2), ylim, col = "red")
          graphics::lines(rep(end_recap, 2), ylim, col = "red")
          for(i in 1:length(rc_time_ls)){
            graphics::lines(rep(rc_time_ls[[i]], 2), ylim, col = vcols[i])
          }

          # Prompt
          if(prompt){
            readline(prompt = "Inspect the plot; then press [enter] to proceed...")
          }



          ###################################################
          #### Manually identify the desired time stamps of desired moments of recapture
          # ... e.g. onset of sharp increase in depth; finish of sharp increase in depth;

          #### If the user has chosen to manually flag recapture timing using the figure...
          if(manual_flag){

            #### Explanatory message
            cat("\n Define the precise time of recapture/phases of recapture by clicking on the points desired then pressing [Esc]... \n")

            #### Define the times of recapture
            manual_flag_timestamps <- graphics::identify(dat2plot, order = TRUE, plot = TRUE)
            manual_flag_timestamps <- data.frame(pos = manual_flag_timestamps$ind,
                                                 click_order = manual_flag_timestamps$order)
            manual_flag_timestamps <- manual_flag_timestamps %>% dplyr::arrange(manual_flag_timestamps$click_order)
            manual_flag_timestamps$timestamp <- dat2plot$timestamp[manual_flag_timestamps$pos]
            recap_ls$manual_flag_timestamps <- manual_flag_timestamps[, c("timestamp", "click_order")]

            #### Re-define time of recapture using 'select' argument
            rc_pos <- manual_flag_timestamps$pos[select]
            rc_time <- dat2plot$timestamp[rc_pos]
            recap_ls$rc_time <- list(time = rc_time, method = "manual", select = select)

            #### Define rows in full dataframe to be flagged/removed
            recapture_rows <-
              which(data_depth$id == recap$id &
                      data_depth$date == recap$date &
                      data_depth$timestamp >= define_time_before(recapture_time = rc_time) &
                      data_depth$timestamp <= define_time_after(recapture_time = rc_time)
              )
            recap_ls$recapture_rows <- recapture_rows
          }

        } # close if(inspect_plot)


        ###################################################

      } else{
        msg <- "No data associated with the following recapture event, skipping...\n"
        cat(msg)
        print(recap)
        recap_ls$warning <- msg
      }


      #### Return list
      return(recap_ls)

    }) # close pbapply::pblapply( over each recapture date.

    #### Process out list
    cat("Step 3: Processing outputs...\n")
    # 'out' is a list with an element for each recapture event
    # ... each element is a list which contains all the details
    # ... associated with each recapture event
    # Collate the details for each individual into one component of the list:
    out <- list(recapture_event_details = out)
    # Define a dataframe of recapture times for all individuals
    recapture_times <- lapply(out$recapture_event_details, function(elem){
      d <- elem$data_recapture_event
      d$timestamp <- elem$rc_time$time
      d$method <- as.character(elem$rc_time$method)
      return(d)
      })
    recapture_times <- do.call(rbind, recapture_times)
    out$recapture_times <- recapture_times
    # Remove recaptures if requested and add to list:
    if(remove_recaptures){
      recapture_rows_all <- sapply(out$recapture_event_details,
                                   function(elem) elem$recapture_rows) %>% unlist() %>% as.vector()
      if(length(recapture_rows_all) > 0){
        data_depth_rem_recap <- data_depth[-c(recapture_rows_all), ]
      } else{
        data_depth_rem_recap <- data_depth
      }
      out$recapture_rows_all <- recapture_rows_all
      out$data_depth_rem_recap <- data_depth_rem_recap
    }

    #### Return output list
    return(out)

  } # close define_recapture() function



#### End of code.
########################################################
########################################################
