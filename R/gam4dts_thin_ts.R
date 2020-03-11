#' @title GAMS4DTS Thinning Methods
#' @importFrom magrittr %>%
#'
#' @description This function implements thinning behind the scenes for \code{\link[Tools4ETS]{GAMS4DTS}}. The function is not intended to be called outside of this environment.
#'
#' @param dat A dataframe to be thinned.
#' @param id_column A character input defining the column name in \code{dat} which distinguishes individuals (i.e. \code{"individual"}).
#' @param timestamp_column A character input defining the column name in \code{dat} which contains timestamps (i.e. \code{"timestamp"}).
#' @param start_event_id_column A character input defining the column name in \code{dat} which identifies start events (i.e. \code{"start_event_id"}).
#' @param start_event_logic_column A character input defining the column name in \code{dat} which defines start events with TRUE/FALSE (i.e. \code{"start_event"})
#' @param method A character input defining the thinning method to be implemented. Supported options are: \code{"swa"}, \code{"sma"} or \code{"sps"}.
#' @param parameters A list of parameters that are required to implement thinning depending on the method.
#'
#' @return A thinned dataframe.
#' @author Edward Lavender
#'

################################################
################################################
#### gams4dts_thin_ts()

#### Comments
# In cases where we have a dataframe with covariates and depths, the user may choose to
# ...  thin this dataframe (if requested), to make it easier to deal with
# ... and perhaps to reduce the extent of serial autocorrelation
# ... Thinning can be achieved using the thin.dat function
# ... At the moment, only one thinning method is implemented:
# ... The user specified the time interval and, for each individual,
# ... depths are averaged over that time interval
# ... The values for smooth covariates are re-calculated for the new time interval.

#### Assumptions
# 1) depth column is called "depth"

# define function:
gams4dts_thin_ts <-
  function(
    # define the dataframe to be thinned
    dat,
    # id_column and timestamp column
    id_column = "individual",
    timestamp_column = "timestamp",
    # Define the name of the column column which, for each individual, defines independent time series
    # (Needed for sps and sma methods)
    start_event_id_column = "start_event_id",
    start_event_logic_column = "start_event",
    # define method for thinning:
    # swa, static window average;
    # sma, simple moving average;
    # sps, systamtic point selectin
    method     = c("swa", "sma", "sps"),
    # define parameters required, depending on method:
    parameters = list(
      # if the method is swa, these are the parameters required:
      swa = list(time_mins = 60,
                 latitude = 56.41041,
                 longitude = 5.469723,
                 # define the covariates you want to include in the thinned dataframe
                 # (the function adds these back in as appropriate)
                 covariates = c("sex",
                                "length",
                                "sun_angle",
                                "tidal_height",
                                "lunar_phase",
                                "julian_day"),
                 independence_threshold = 720),
      # if the method is sps, these are the parameters required:
      sps = list(
        # a vector of starting values:
        # ... if you supply more than one, then you will have that many dataframes
        # ... for each individual, each using a different subset of data
        first = c(1, 2),
        # ... select every nth value:
        nth = 10),
      sma = list()
      # if the method is sma, these are the parameters required:
      ) # close parameters list
    ){ # close function inputs and open function:



  ################################################
  ################################################
  #### Checks

  # make sure a supported method is implemented
  if(!(method %in% c("swa", "sma", "sps"))){
    stop("Inputted method is not supported. Currently implemented methods are: 'swa', 'sps' or 'sma'.")
  }



  ################################################
  ################################################
  #### swa method

  if(method == "swa"){

    ################################################
    #### Create a basic thinned dataframe

    # convert time in mins to appropriate object:
    time_mins <- paste0(parameters$swa$time_mins, " mins")

    # create a dataframe with a new column (timestamp window) and group by this column and indvidual
    dat_adj <- dat
    dat_adj$timestamp_window <- cut(dat_adj[, timestamp_column], time_mins)
    # head(dat_adj)

    # Define a new dataframe "thin"
    thin_tbl <- tapply(dat_adj$depth, list(dat_adj$individual, dat_adj$timestamp_window), mean)
    cn <- colnames(thin_tbl)
    lcn <- length(cn)
    rn <- rownames(thin_tbl)
    lrn <- length(rn)
    thin <- data.frame(individual = rep(rn, lcn),
                       timestamp_window = sort(rep(cn, lrn)),
                       depth = as.numeric(thin_tbl))
    thin <- dplyr::arrange(thin, thin$individual, thin$timestamp_window)

    # Round depths to the nearest cm
    thin$depth <- round(thin$depth, digits = 1)

    # Ensure timestamp_window is recognised as.POSIXct object
    # And add half of the number of seconds between timestamps
    # ... because the average depth is best represented at the depth of the middle of the adjusted timewindows
    # ... (not at the start or the end).
    # In this way, temporal covariates are also calculated appropriately.
    thin$timestamp_window <- fasttime::fastPOSIXct(thin$timestamp_window, tz = "UTC") + ((parameters$swa$time_mins/2) * 60)
    thin$timestamp_window <- fasttime::fastPOSIXct(thin$timestamp_window, tz = "UTC")

    # recalculate hourofday using hour() function from lubridate
    thin$hourofday <- lubridate::hour(thin$timestamp_window) + lubridate::minute(thin$timestamp_window)/60


    ######################################################
    #### Redefine start events for AR.start in bam

    # use the flag_ts function
    # list output of dat.independent:
    # note that timestamps in thin are, at this stage of the function, called timestamp_window:
    dio <- flag_ts(x = thin$timestamp_window,
                   fct = thin[, id_column],
                   duration_threshold = parameters$swa$independence_threshold,
                   flag = 1:2)

    # add start_event to thin
    thin$start_event <- dio$flag1

    # add start_event_id to thin
    thin$start_event_id <- dio$flag2


    ################################################
    #### Add back fixed effects by matching with the original dataframe

    #### sex
    if("sex" %in% parameters$swa$covariates){
      thin$sex <- dat$sex[match(unlist(as.vector(thin[, id_column])), dat[, id_column])]
    }

    #### size
    if("length" %in% parameters$swa$covariates){
      thin$length <- dat$length[match(unlist(as.vector(thin[, id_column])), dat[, id_column])]
    }



    ################################################
    #### add back temporal variables
    #### NB: this section is simply copied from the dat.assembly() function.

    # add back temporal variables if specified
    if("sun_angle" %in% parameters$swa$covariates){

      # create a dataframe consisting only data needed for suncalc
      suncalc_data <- data.frame(date = thin$timestamp_window + ((parameters$swa$time_mins/2) * 60),
                                 lat = parameters$swa$latitude,
                                 lon = parameters$swa$longitude)

      # calculate the angle of the sun relative to the horizon at each time (in radians)
      sun_angle <- suncalc::getSunlightPosition(data = suncalc_data, keep = "altitude")

      # convert this angle to degrees:
      sun_angle$altitude_deg <- sun_angle$altitude * (180/pi)

      # add the data to the thin dataframe
      thin$sun_angle <- sun_angle$altitude_deg

      # round to the nearest 3 digits
      thin$sun_angle <- round(thin$sun_angle, digits = 3)

      # close if() statement
    }


    #### tidal height
    # If tidal_height gave been specified, obtain these for timestamp
    if("tidal_height" %in% parameters$swa$covariates){
      # thin$tidal_height
      # close if() statement
    }


    #### lunar phase
    # If lunar_phase have been specified, obtain these for timestamp
    # ... using the lunar package
    if("lunar_phase" %in% parameters$swa$covariates){
       # calculate lunar phase
       thin$lunar_phase <- lunar::lunar.phase(thin$timestamp_window,
                                              shift = thin$hourofday - 12) # number of hours until midday
       # round to the nearest 3 digits
       thin$lunar_phase <- round(thin$lunar_phase, digits = 3)
     }


     #### julian day
     # If julian_day has been specified, calculate this for each timestamp
     # ... using lubridate package
     if("julian_day" %in% parameters$swa$covariates){
       # add julian day to dataframe
       thin$julian_day <- lubridate::yday(thin$timestamp_window - ((parameters$swa$time_mins/2) * 60))
       }



    ################################################
    #### Compatibility adjust

    # rename timestamp window for compatibility with functions in shiny app
    thin$timestamp <- fasttime::fastPOSIXct(thin$timestamp_window, tz = "UTC")
    thin$timestamp_window <- NULL

    # keep order consistent with original dataframe
    thin <- thin %>% dplyr::select(colnames(dat))

    # It is necessary to force data.frame() structure to avoid compatibility issues in shiny
    # i.e. so the structure of dat_thin() is identical to dat_full()
    thin <- data.frame(thin)



    ################################################
    ################################################
    #### method == "sps"

    # close if(method == "swa"){ and open next possibility
    } else if(method == "sps"){

    # Create a list of with an element for each individual:
    # ... Each individual is a list with each element corresponding to
    # ... a dataframe for a specific first value
    thin_ls <-
      # loop over every individual...
      lapply(unique(dat[, id_column]),

             # for every individual...
             function(id){

               # filter the dataframe (dat) to focus on that individual:
               id_df <- dat[which(dat[, id_column] == id), ]

               # Create a list of timeseries for each individual
               # Each element in this list is a thinned timeseries,
               # ... with a starting position corresponding to one of the
               # ... positions specified in parameters$sps$first
               id_ls <-
                 # Loop over every supplied starting position:
                 lapply(parameters$sps$first, function(first_val){

                   # Loop over every unique time series to create a thinned
                   # ... dataframe which starts at a specific 'first' value:
                   thin_from_specific_first <-
                     # loop over every unique start_event...
                     lapply(unique(id_df[start_event_id_column]),
                            function(seid){
                              # subset id_df to only consider those rows:
                              id_df_sbt <- id_df[which(id_df[start_event_id_column] == seid), ]
                              # determine the rows we want to keep for that specific individual
                              # ...within the current timeseries:
                              row_retain <- seq(first_val, nrow(id_df_sbt), by = parameters$sps$nth)
                              # create a temporary dataframe
                              dtmp <- data.frame(id_df_sbt[row_retain, ])
                              # Ensure that the first row is recognised as the start of an independent timeseries
                              # Using start_event_logic column:
                              dtmp[1, start_event_logic_column] <- TRUE
                              # return dtmp
                              return(dtmp)
                              #  close function, lapply and bind all dataframes for that individual:
                            }) %>% dplyr::bind_rows()

                   # Return dataframe for that individual starting at a specific first value to the list:
                   return(thin_from_specific_first)

                 }) # close function and lapply(parameters$sps$first, function(first_val){
               return(id_ls)
             }) # close function and lapply(unique(dat[, id_column])

    #### Process the list to be more intuitive so we have one element
    # ... for each first value that is a dataframe containing all the data
    # ... across all individuals
    thin <-
      # Loop over every firat value
      lapply(1:length(parameters$sps$first), function(first_val){
        # Define a dataframe with the thinned dataframes across all individuals
        # ... for that first value
        thin_df <-
          # ... by looping over each individual
          lapply(thin_ls, function(thin_df_id){
            # Extracting the rows for that individual and that start value
            # ... and binding rows for all individuals
            return(thin_df_id[[first_val]]) }) %>% dplyr::bind_rows()
        # Return the dataframe with all individual's thinned
        # ... data to the list, thin:
        return(thin_df)
    })

    # Return the list - use the first list for GAMS4DTS
    return(thin[[1]])

    ################################################
    ################################################
    #### method == "sma"

    # close else if(method == "sps"){ and open next possibility
    } else if(method == "sma"){

      warning("You have selected to calculate state-space simple moving averages. This may take a few minutes...",
              immediate. = TRUE)

      thin <-
        # loop over every individual...
        lapply(unique(dat[, id_column]),
               # for every individual...
               function(id){

                 # filter the dataframe (dat) to focus on that individual:
                 id_df <- dat[which(dat[, id_column] == id), ]

                 # loop over every unique timeseries...
                 id_df_thin <-
                   lapply(unique(id_df[start_event_id_column]),
                        # loop over every unique start_event (start_event_id: seid)...
                        function(seid){

                          # subset id_df to only consider those rows:
                          id_df_sbt <- id_df[which(id_df[, start_event_id_column] == seid), ]

                          # identify the duration between successive depths
                          # for now, we'll just look at the difference between 1st and 2nd depths and assume this is always the same
                          # (but see comment below...)
                          # (also assume times are in the same time zone for simplicity)
                          dft <- difftime(id_df_sbt[2, timestamp_column],
                                          id_df_sbt[1, timestamp_column],
                                          units = "secs") %>% as.numeric()

                          # NB: the current assumption is that the duration between successive readings is constant
                          # this is required to define a 'time series' object in R.
                          # However, if the duration between detections is not always constant
                          # ... we can implement an additional loop here which
                          # ... loops over each sub-time series, and then pieces them together afterwards (not yet implemented)

                          # Define a time series object:
                          # NB: depth column is assumed to be called "depth"
                          tsy <- stats::ts(data = id_df_sbt[, "depth"],
                                           start = as.numeric(id_df_sbt[1, timestamp_column]),
                                           end = as.numeric(max(id_df_sbt[, timestamp_column])),
                                           # The frequency is 1/(dft expressed in seconds)
                                           frequency = 1/dft)

                          # simple moving average (default options)
                          # set h = 1, this is the length of the forcasting period
                          # we don't need to produce a forecast but it produces a warning message if h = 0
                          # ... that may concern users unnecessarily.
                          sma_def <- smooth::sma(tsy, h = 1, silent = "all")

                          # Extract fitted values and add them to the dataframe
                          id_df_sbt$sma_fitted <- sma_def$fitted %>% as.numeric()

                          # Add residuals
                          id_df_sbt$sma_residuals <- sma_def$residuals %>% as.numeric()

                          # return dataframe:
                          return(id_df_sbt)

                          # close function and lapply and bind_rows to join all the dataframes for a given individual
                        }) %>% dplyr::bind_rows()

                 # Return dataframe
                 return(id_df_thin)

                 # close function and lapply and bind_rows to join all dataframes from across individuals
               }) %>% dplyr::bind_rows()

      # Rename some columns (so everything is labelled consistently for shiny)
      thin$depth_original <- thin$depth
      thin$depth <- thin$sma_fitted
      thin$sma_fitted <- NULL

      # For depth simulations, we can round fitted values and residuals
      # this is mainly for appearances sake on the shiny
      # but also reflects our limited accuracy when we measure depth
      thin$depth <- round(thin$depth, digits = 2)
      thin$sma_residuals <- round(thin$sma_residuals, digits = 2)

    } # close } else if(method == "sma"){



  ################################################
  ################################################
  #### Return and close function

  # return thinned dataframe
  return(thin)

  # close function
  }






#### End of code.
################################################
################################################
