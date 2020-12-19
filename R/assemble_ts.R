#' @title Simulate a time-series dataframe
#' @importFrom magrittr %>%
#'
#' @description This function assembles a basic dataframe structure that defines, for each factor level (i.e., individual) inputted, a sequence of timesteps, possibly differing in duration and resolution across factor levels. Since this function was inspired by the simulation of depth time-series, commonly assumed drivers of depth (namely, sex, length, sun angle, lunar phase and Julian day) can be included in the dataframe by supplying these to the \code{covariates} argument. In this case, sex is simulated for each individual from a discrete distribution with a user-defined parameter (the probability of sampling a female) and length is simulated for each individual from a Gamma distribution with user-specified parameters. Sun angle, lunar phase and Julian day are calculated for each timestamp using \code{\link[suncalc]{getSunlightPosition}}, \code{\link[lunar]{lunar.phase}} and \code{\link[lubridate]{yday}} respectively. For other covariates/ecological time-series, the user can use this dataframe to define covariate values. In both cases, this information can then be used simulate values of a response (see \code{\link[Tools4ETS]{sim_ts}}).
#'
#' @param start_date A character specifying the date of the first observation, specified as "yyyy-mm-dd".
#' @param start_date_variable A logical input specifying whether or not the start date for simulated time-series should differ among individuals (if multiple individuals are specified). This is controlled via the \code{parameters} argument.
#'
#' @param max_duration_days A number specifying the maximum duration, in days, over which to simulate data.
#' @param duration_days_variable A logical input specifying whether or not the duration of time-series for each factor level should vary (if multiple levels have been specified).
#' @param resolution_minutes A number or vector specifying the duration, in minutes, between consecutive simulated timestamps. If a single number is specified, the resolution is taken to be the same across all individuals. If more than one number is defined, supplied numbers are taken to be the resolutions at which individual time-series will be sampled. The duration between consecutive simulated timestamps for each individual is sampled randomly from this vector, according to the specifications in the \code{parameters} list (see below).
#' @param n_individuals A number specifying the number of individuals (factor levels) for which to simulate data.
#' @param longitude A number specifying the longitude (decimal degrees) of the simulated location. This is required to calculate the covariate \code{sun_angle} (see below).
#' @param latitude A number specifying the latitude (decimal degrees) of the simulated location. This is required to calculate the covariate \code{sun_angle} (see below).
#' @param tz A character specifying the time zone. The default is \code{"UTC"}.
#' @param covariates A character vector specifying the covariates to be included in the dataframe. Currently supported covariates are: (1) \code{"sex"}, a factor which distinguishes sexes (F, female; M, male); (2) \code{"length"} (cm); (3) \code{"sun_angle"}, the angle (degrees) of the sun above the horizon (see \code{\link[suncalc]{getSunlightPosition}}); (4) \code{"lunar_phase"}, the lunar phase (radians; see \code{\link[lunar]{lunar.phase}}); (5) \code{"julian_day"} (the number of days since January 1st).
#' @param parameters A nested list specifying additional parameters. This currently supports the following elements. (1) An element which adjusts the variation in \code{start_date} among individuals, if \code{start_date_variable = TRUE}. \code{ndays} is the maximum number of days, from the \code{start_date}, at which an individual time-series can begin. Start dates are simulated between the dates defined by \code{start_date} and \code{start_date + ndays} using \code{\link{sample}}. \code{prob} is a vector of probabilities which defines the probability of sampling any given date between the \code{start_date} and \code{start_date + ndays}. By default \code{prob = NULL}; i.e. start dates are sampled from a uniform distribution between \code{start_date} and \code{start_date + ndays}. If \code{prob} is specified, this should be a vector of length \code{ndays +1}. \code{replace} defines whether or not to sample the vector of possible start dates with, or without replacement. By default, \code{replace = TRUE}. (2) An element which defines the parameters for \code{\link[base]{sample}} in order to simulate variation in the duration (days) of each time-series, if \code{duration_days_variable = TRUE}. (3) An element which defines the parameters for \code{\link{sample}} in order to simulate variation in the resolution (minutes) of each time-series, if a vector of length > 1 is supplied to \code{resolution_minutes}. (4) An element that defines the distribution from which sex is simulated. \code{"Pf"} defines the probability that any given individual simulated is a female (and, therefore, the probability of any given individual being male, which is 1 - Pf). (5) An element which adjusts the distribution from which individual lengths (cm) are simulated. Lengths are assumed to be drawn from a Gamma distribution, defined by parameters \code{shape} and \code{scale} (see \code{\link[stats]{GammaDist}}). \code{plot_density_curve} is a logical input which, if \code{TRUE}, causes the function to return a theoretical density curve of the distribution from which lengths are simulated.
#'
#' @return The function outputs a dataframe, with the following columns: (1) 'individual', an integer which distinguishes each unique individual; (2) 'timestamp', a time in POSIXct format, which defines each unique observation/timestep, at the specified resolution, (3) 'hourofday', an integer which defines the hour of day; and (4) columns for each of the inputted covariates (if applicable).
#'
#' @details A dataframe comprising a sequence of timestamps (as possible covariate values) is simulated in order to set up a dataframe which can be used to simulate values of a response. \code{\link[Tools4ETS]{sim_ts}} provides a starting framework to simulate the response.
#'
#' @seealso \code{\link[stats]{GammaDist}}, \code{\link[base]{sample}}, \code{\link[Tools4ETS]{sim_ts}}
#'
#' @examples
#' # Simulate a dataframe for a single individual:
#' assemble_ts(start_date = "2017-01-01",
#'              start_date_variable = FALSE,
#'              max_duration_days = 10,
#'              duration_days_variable = FALSE,
#'              resolution_minutes = 720,
#'              n_individuals = 1,
#'              longitude = 5,
#'              latitude = 65,
#'              tz = "UTC",
#'              covariates = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
#'              parameters = list(
#'                           sex = list(Pf = 0.5, replace = TRUE),
#'                           length = list(shape = 10, scale = 4, plot_density_curve = TRUE)
#'                           )
#' )
#'
#' # Simulate data from  multiple individuals with variable
#' # .. start dates,  durations and resolutions
#' assemble_ts(start_date = "2018-01-01",
#'              start_date_variable = TRUE,
#'              max_duration_days = 21,
#'              duration_days_variable = TRUE,
#'              resolution_minutes = c(2, 30, 60),
#'              n_individuals = 3,
#'              longitude = 5,
#'              latitude = 65,
#'              tz = "UTC",
#'              covariates = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
#'              parameters = list(start_date = list(ndays = 100, prob = NULL, replace = TRUE),
#'                                duration_days = list(prob = NULL, replace = TRUE),
#'                                resolution_mins = list(prob = NULL, replace = TRUE),
#'                                sex = list(Pf = 0.5, replace = TRUE),
#'                                length = list(shape = 10, scale = 4,
#'                                              plot_density_curve = TRUE)
#'                           )
#' )
#'
#' @export



################################################
################################################
#### assemble_ts()

assemble_ts <-
  function(
    start_date,
    start_date_variable = TRUE,
    max_duration_days,
    duration_days_variable = FALSE,
    resolution_minutes, # one number or more
    n_individuals,
    longitude,
    latitude,
    tz = "UTC",
    covariates = NULL,
    parameters = list(start_date = list(ndays = max_duration_days-1, prob = NULL, replace = TRUE),
                      duration_days = list(prob = NULL, replace = TRUE),
                      resolution_mins = list(prob = NULL, replace = TRUE),
                      sex = list(Pf = 0.5, replace = TRUE),
                      length = list(shape = 25, scale = 4, plot_density_curve = TRUE)
                      )
    ){



  ################################################
  #### User warnings

  # Check the user has input correct covariates
  # Define the number of covariates not in the allowed list:
  covariates_ni_allowed <-
    which(!(covariates %in% c("sex", "length", "sun_angle", "lunar_phase", "julian_day")))
  if(length(covariates_ni_allowed)){
    stop("You have entered an unsupported covariate. Please check spelling.")
  }

  # If the user has only inputted one individual...
  if(n_individuals == 1){
    # If duration_days_variable == TRUE, then add warning:
    if(duration_days_variable){
      warning(paste0("You have defined duration_days_variable = TRUE but n_individuals = 1.\n",
                      "duration_days_variable = TRUE will be ignored."))
    }
    # If length(resolution_minutes) >1, then add warning:
    if(length(resolution_minutes) > 1){
      warning(paste0("You have defined mutliple values for resolution_minutes, but n_individuals = 1.\n",
                      "Only the first value for resolution_minutes will be used."))
    }
    # If "sex" and "length" are to be included in the dataframe, then add warning:
    if("sex" %in% covariates | "length" %in% covariates){
      warning(paste0(
        "You have defined 'sex' and/or 'length' to be included in the dataframe, but n_individuals = 1.\n",
        "You may not be able to incorporate these variables in models of depth time-series."))
    }
  }



  ################################################
  #### Set up basic dataframe structure with timestamps

  # convert start_date to POSIXct object
  start_date <- as.POSIXct(start_date, tz = tz)

  # if the start date from which individual time-series will be simulated is set to vary
  # and if the number of individuals > 1 (variable start_dates only make sense then)...
  if(start_date_variable & n_individuals > 1){
    # define last possible start date:
    lpsd <- start_date + (parameters$start_date$ndays * 24 * 60 * 60)
    # define a sequence of days from start date to last possible start date:
    start_date_vector <- seq.POSIXt(start_date, lpsd, by = "day")
    # sample at random from our vector of possible start dates,
    # ... using parameters specifed
    id_start <- sample(start_date_vector,
                         size = n_individuals,
                         replace = parameters$start_date$replace,
                         prob = parameters$start_date$prob
                         ) # close sample
  # else, we'll simply make id_start the original start_date object
  } else{
    id_start <- start_date
  }

  # If the duration for which data is simulated for each individual is to be variable...
  # and the user has input multiple individuals ...
  if(duration_days_variable & n_individuals > 1){
    # Define a vector, id_days, which will hold the number of days for each individual
    # ... for which data will be simulated.
    # By randomly sampling a number from 1:max_duration_days
    # ... using probabilities specified in the parameters list
    id_days <- sample(x = 1:max_duration_days,
                      size = n_individuals,
                      replace = parameters$duration_days$replace,
                      prob = parameters$duration_days$prob)
    # Otherwise of the number of days of data for each individual will
    # ... just equal max_duration_days:
    } else{
    id_days <- max_duration_days
  }

  # If the resolution_minutes is to be variable (i.e., if the user has specified more than one value):
  # and the user has specified more than one individual ...
  if(length(resolution_minutes) > 1 & n_individuals > 1){
    # Create a new vector, id_res, which holds the resolution for each individual
    # This is sampled from resolution_minutes at random using parameters specified:
    id_res <-
      sample(x = resolution_minutes,
             size = n_individuals,
             parameters$duration_days$replace,
             prob = parameters$resolution_minutes$prob)
  # Otherwise, the id_res vector contains an identical value for each individaul
  } else{
    # select the first value for resolution_minutes
    # this avoids errors if the user has only inputted one individual
    # ... but resolution_minutes has length > 1
    id_res <- resolution_minutes[1]
  }

  # Define a list of dataframes (one per individual), with that individual's
  # ... id number and timestamp
  # Set timestamp and individual to NULL outside of the loop below to keep CRAN happy
  # ... (avoid: no visible binding for global variable ‘individual’)
  individual <- NULL
  timestamp <- NULL
  dat_ls <-
    mapply(
      # simultaneously loop over id, start, duration and res
      function(id, start, duration, res){
        # create an individual-specific sequence of timestamps
        timestamp <- seq.POSIXt(from = start,
                                to = start + (duration * 24 * 60 * 60),
                                by = res * 60)
        # repeat the id number for each timestamp
        individual <- rep(id, length(timestamp))
        # group this into a dataframe
        d <- data.frame(individual = individual, timestamp = timestamp)
        # return the dataframe
        return(d)
        }, # close mapply function
      # specifiy arguments to loop over
      1:n_individuals, id_start, id_days, id_res, SIMPLIFY = FALSE
      ) # close mapply opening bracket

  # Bind this information into a dataframe
  dat <- dplyr::bind_rows(dat_ls)

  # make sure the timezone is recognised:
  attr(dat$timestamp, "tzone") <- tz

  # Add hourofday to the dataframe
  hourofday <- NULL # to avoid cran check issues
  dat$hourofday <- lubridate::hour(dat$timestamp) + lubridate::minute(dat$timestamp)/60

  # Arrange columns and rows (by individual, then timestamp, then hourofday)
  # this is necessary for later stages in the code and also tidiness
  dat <- dat %>%
    dplyr::select(individual, timestamp, hourofday) %>%
    dplyr::arrange(individual, timestamp)



  ######################################################
  #### Define start events for AR.start in bam

  # return the date/time first record of each individual
  # start_event <- tapply(dat$timestamp, dat$individual, min)

  # add the date/time of the first record of each individaul to the dataframe
  # dat$start_event <- as.numeric(start_event[match(dat$individual, names(start_event))])

  # convert start_events to true/false to specify whether a particular date/time
  # ... is the first record for an individual (T) or not (N)
  # dat$start_event <- as.logical(dat$start_event == dat$timestamp)



  ################################################
  #### Add fixed effects

  if(!is.null(covariates)){



  #### sex
  # add sex, if specified, by randomly sampling a sex ("M" or "F") for each individual
  if("sex" %in% covariates){
    # for each individual,
    # ...randomly sample a sex (M or F)
    # ... and repeat this for all the rows in the dataframe of that individual
    # note as.vector() is required because replicate() returns a matrix in this case
    # ... where all columns are the repeated values for a particular individual
    dat$sex <-
      lapply(dat_ls, function(df){
        rep(
          sample(x  = c("F", "M"),
                 size = 1,
                 prob = c(parameters$sex$Pf, 1-parameters$sex$Pf),
                 replace = parameters$sex$replace),
          nrow(df)
          )
      }) %>% unlist() %>% as.vector()
    # convert to factor
    dat$sex <- factor(dat$sex)
  }



  #### length
  # add length, if specified, by randomly drawing a size from a normal distribution
  # at the moment, the parameters of this distribution are specified internally,
  # ... to generate a reasonable distribution for flapper skate, but they could be
  # ... added to the function inputs instead for flexibility with other species.
  if("length" %in% covariates){
    # for each individual,
    # simulate a size
    dat$length <-
      lapply(dat_ls, function(df){
        rep(
          stats::rgamma(n = 1, shape = parameters$length$shape, scale = parameters$length$scale),
          nrow(df)
          )
      }) %>% unlist() %>% as.vector() %>% round(digits = 0)
    dat$length <- round(dat$length, digits = 0)

    # If the user has specifed to plot a density curve,
    # Set x to NULL to avoid CRAN issues
    x <- NULL
    if(parameters$length$plot_density_curve){
      graphics::curve(stats::dgamma(x,
                                    shape = parameters$length$shape,
                                    scale = parameters$length$scale),
                      from = min(dat$length)*0.1,
                      to = max(dat$length)*3,
                      xlab = "Length (cm)",
                      ylab = "Density",
                      las = TRUE
            ) # close curve(
    } # close if(parameters$length$plot_density_curve){
  } # close   if("length" %in% covariates){





  ################################################
  #### Add temporal covariates

  #### sun angle
  # if sun_angle gave been specified, calculate these for each timestamp
  # ... using the suncalc package
  if("sun_angle" %in% covariates){

    # create a dataframe consisting only data needed for suncalc
    suncalc_data <- data.frame(date = dat$timestamp,
                               lat = latitude,
                               lon = longitude)

    # calculate the angle of the sun relative to the horizon at each time (in radians)
    sun_angle <- suncalc::getSunlightPosition(data = suncalc_data, keep = "altitude")

    # convert this angle to degrees:
    sun_angle$altitude_deg <- sun_angle$altitude * (180/pi)

    # add the data to the dat dataframe
    dat$sun_angle <- sun_angle$altitude_deg

    # round
    dat$sun_angle <- round(dat$sun_angle, digits = 3)

    # close if() statement
  }


  #### tidal height
  # If tidal_height has been specified, obtain these for timestamp
  # NB: this is not yet implemented
  if("tidal_height" %in% covariates){

    # dat$tidal_height

    # close if() statement
  }


  #### lunar phase
  # If lunar_phase has been specified, obtain these for timestamp
  # ... using the lunar package
  if("lunar_phase" %in% covariates){
    # calculate lunar phase
    dat$lunar_phase <- lunar::lunar.phase(dat$timestamp,
                                          shift = dat$hourofday - 12) # number of hours until midday
    # round
    dat$lunar_phase <- round(dat$lunar_phase, digits = 3)
  }


  #### Julian day
  # If Julian_day has been specified, calculate this for each timestamp
  # ... using yday() function in the lubridate package
  if("julian_day" %in% covariates){
    # add Julian day to dataframe
    dat$julian_day <- lubridate::yday(dat$timestamp)
  }

  } # close if(!is.null(covariates))

  ################################################
  #### Return dataframe and close function

  # return dat
  return(dat)


  # close assemble_ts function
  }


#### End of code.
################################################
################################################
