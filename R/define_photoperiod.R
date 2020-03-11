#' @title Define photoperiod
#' @description This function can be used to define 'photoperiod' for a sequence of timestamps. This is useful because photoperiod is often assumed to be a cue for seasonal effects in biological systems. The function calculates photoperiod as the number of time units (e.g. hours) between two discrete sunlight times (e.g. dawn and dusk), defined by the user and calculated by \code{\link[suncalc]{getSunlightTimes}}, on each date along a regular sequence of dates between a user-specified (or data-informed) start and end date. These can be returned either as a dataframe, or as a vector which matches an existing dataframe. This latter option is particularly useful for large, high resolution datasets where it is unnecessarily costly to calculate photoperiod for multiple observations on each day; the use of \code{\link[base]{match}} minimises this cost. The sunlight times between which photoperiod is defined are user-specified because a biologically meaningful definition of photoperiod is system specific, depending on the light-sensitivity of the organism/system in question (see Details).
#'
#' @param start_date The start date, as a \code{\link[base]{Date}} object, from which photoperiod will be calculated. If \code{NULL}, the start date is taken as the first date (in time) in user-supplied vector of dates (see \code{match_date}).
#' @param end_date The end date, as a \code{\link[base]{Date}} object, to which photoperiod will be calculated. If \code{NULL}, the end date is taken as the last date (in time) in user-supplied vector of dates (see \code{match_date}).
#' @param lat A numeric input which defines the latitude (decimal degrees) at which to calculate the sunlight times for each interval (see \code{interval}) on each day of the timeseries. This is passed to \code{\link[suncalc]{getSunlightTimes}}. The location is assumed to be constant at least for each day of the timeseries.
#' @param lon  A numeric input which defines the longitude (decimal degrees) at which to calculate the sunlight times for each interval (see \code{interval}) on each day of the timeseries. This is passed to \code{\link[suncalc]{getSunlightTimes}}. The location is assumed to be constant at least for each day of timeseries.
#' @param interval A character vector with two elements which define the sunrise/sunset times between which photoperiod is calculated. This is passed to the \code{keep} argument of \code{\link[suncalc]{getSunlightTimes}} where further details are provided.
#' @param units A character input which defines the units of the time difference. This is passed to \code{\link[base]{difftime}}. Options are: "auto", "secs", "mins", "hours", "days" or "weeks".
#' @param match_date A sequence of dates to which appropriate photoperiods are matched (see Examples).
#'
#' @return The function returns a dataframe or a vector depending on whether or not \code{match_date} is provided.
#'
#' @details Photoperiod is a candidate cue, or proximate driver, of seasonal variation in many biological systems. Yet defining a meaningful metric of photoperiod is challenging. 'Possible sunshine duration' is one component of the definition (Yokoya and Shimizu, 2011). For example, a simple definition of photoperiod is the duration between the time of sunrise and sunset (when the angle of the sun relative to the horizon is 0°). However, in biology, possible sunshine duration may be a poor metric of photoperiod because it does not recognise species'-specific light sensitivity; i.e., the period of time over which there is sufficient light to be to detected or to stimulate a response may be shorter or longer than the possible sunshine duration. For this reason, in plant sciences, photoperiod is often defined as the duration between when the sun first rises above 6° below this horizon in the morning (i.e. the end of nautical twilight/the start of civil twilight) and dips below 6° again in the evening (i.e. the end of civil twilight/the start of nautical twilight), although for some species a slightly shorter or longer duration may be appropriate depending on the light levels to which that species (or individual) is sensitive (Saltani and Sinclair, 2012). The times of these events can be defined using \code{\link[suncalc]{getSunlightTimes}} with the 'dawn' and 'dusk' arguments respectively. This suggests effective day length – the duration over which light levels exceed a species-specific sensitivity threshold – is better measure of photoperiod (Yokoya and Shimizu, 2011). In other words, photoperiod is best defined in the context of the sensory capacity of the species of interest. \code{define_photoperiod()} allows for this necessary flexibility.
#'
#' @references
#'
#'Soltani, Afshin & Sinclair, T.R. (2012). Modeling physiology of crop development, growth and yield. Modeling Physiology of Crop Development, Growth and Yield. 1-322.
#'
#' Yokoya, Masana & Shimizu, Hideyasu. (2011). Estimation of Effective Day Length at Any Light Intensity Using Solar Radiation Data. International journal of environmental research and public health. 8. 4272-83. 10.3390/ijerph8114272.
#'
#' @examples
#'
#' #### Define photoperiod dataframe
#' photo1 <-
#'   define_photoperiod(start_date = as.Date("2016-05-02", tz = "UTC"),
#'                      end_date = as.Date("2016-05-23", tz = "UTC"),
#'                      lat = -5.47184,
#'                      lon = 56.41535,
#'                      interval = c("dawn", "dusk"),
#'                      match_date = NULL)
#' utils::str(photo1)
#' utils::head(photo1)
#'
#' #### Photoperiods can be added to a dataframe using match()
#' dat_flapper$date <- as.Date(dat_flapper$timestamp)
#' dat_flapper$photo1 <- photo1$photoperiod[match(dat_flapper$date, photo1$date)]
#'
#' #### define_photoperiod() can implement this under-the-hood
#' # ... if match_date is supplied
#' dat_flapper$photo2 <-
#'   define_photoperiod(start_date = as.Date("2016-05-02", tz = "UTC"),
#'                      end_date = as.Date("2016-05-23", tz = "UTC"),
#'                      lat = -5.47184,
#'                      lon = 56.41535,
#'                      interval = c("dawn", "dusk"),
#'                      match_date = dat_flapper$date)
#'
#' #### In this case, the function can pull the start/end dates
#' # ... automatically from the date sequence supplied:
#' dat_flapper$photo3 <-
#'   define_photoperiod(lat = -5.47184,
#'                      lon = 56.41535,
#'                      interval = c("dawn", "dusk"),
#'                      match_date = dat_flapper$date)
#'
#' #### Examine the outputs
#' utils::head(dat_flapper$photo1)
#' utils::head(dat_flapper$photo2)
#' utils::head(dat_flapper$photo3)
#' identical(dat_flapper$photo1, dat_flapper$photo2, dat_flapper$photo3)
#'
#' @author Edward Lavender
#' @export
#'

##########################################
##########################################
#### define_photoperiod()

# assume a single location across timeseries

define_photoperiod <-
  function(start_date = NULL,
           end_date = NULL,
           lat,
           lon,
           interval = c("dawn", "dusk"),
           units = "hours",
           match_date = NULL){

    #### Define a sequence of dates
    # Obtain dates from match_date if not provided
    if(is.null(start_date)){
      stopifnot(!is.null(match_date))
      start_date <- min(match_date, na.rm = TRUE)
    }
    if(is.null(end_date)){
      stopifnot(!is.null(match_date))
      end_date <- max(match_date, na.rm = TRUE)
    }
    # Define date sequence
    dates <- seq.Date(start_date, end_date, by = 1)

    #### Define a dataframe with the photoperiod on each date
    photoperiod_df <- suncalc::getSunlightTimes(date = dates,
                                                lat = lat,
                                                lon = lon,
                                                keep = interval)

    #### Calculate photoperiod in user-specified units :
    photoperiod_df$photoperiod <-
      as.numeric(difftime(photoperiod_df$dusk, photoperiod_df$dawn, units = units))

    #### Return the dataframe or a vector of photoperiod durations which
    # ... matches an inputted dataset.
    if(is.null(match_date)){
      return(photoperiod_df)
    } else{
      photoperiod4dat <- photoperiod_df$photoperiod[match(match_date, photoperiod_df$date)]
      return(photoperiod4dat)
    }

  }


#### End of code.
##########################################
##########################################
