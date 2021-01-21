#####################################
#####################################
#### hour_dbl()

#' @title Compute the time of day, in hours, as a double
#' @description Compute the time of day, in hours, from a \code{\link[base]{DateTimeClasses}} object, accounting for the hour of day, the number of minutes passed the hour and the number of seconds passed the minute.
#'
#' @param time A vector of times, of the class \code{\link[base]{DateTimeClasses}}.
#'
#' @return The function returns a double or vector of doubles which defines/define the time of day, in hours.
#'
#' @examples
#'
#' hour_dbl(as.POSIXct("2016-01-01 13:01:40"))
#'
#' @author Edward Lavender
#' @export
#'

hour_dbl <- function(time){
  return(lubridate::hour(time) + lubridate::minute(time)/60 + lubridate::second(time)/3600)
}


#####################################
#####################################
#### hour_nearest()

#' @title Compute the nearest integer hour of day at supplied times
#' @description Compute the nearest integer hour of day from a \code{\link[base]{DateTimeClasses}} object (i.e., nearest neighbour interpolation).
#'
#' @param time A vector of times, of the class \code{\link[base]{DateTimeClasses}}.
#'
#' @return The function returns an integer for each inputted time.
#'
#' @examples
#'
#' hour_nearest(as.POSIXct("2016-01-01 13:01:40"))
#' hour_nearest(as.POSIXct(c("2016-01-01 13:30:00")))
#' hour_nearest(as.POSIXct(c("2016-01-01 13:30:01")))
#' hour_nearest(as.POSIXct(c("2016-01-01 13:01:40", NA)))
#'
#' @author Edward Lavender
#' @export
#'

hour_nearest <- function(time){
  time_rnd <- lubridate::round_date(time, unit = "hour")
  hr_nrst <- lubridate::hour(time_rnd)
  return(hr_nrst)
}



#######################################
#######################################
#### mmyy()

#' @title Define month-year categories from time series
#' @description This function defines month-year categories from time series data (i.e., \code{\link[base]{Date}} and \code{\link[base]{DateTimeClasses}} objects).
#'
#' @param x An vector of class \code{\link[base]{Date}} or \code{\link[base]{DateTimeClasses}}.
#' @param levels A logical input which defines whether or not to return a factor with chronologically ordered levels (i.e., levels ordered by year then month, for the inputted \code{x}) or a character (\code{levels = FALSE}).
#'
#' @return The function returns a vector which specifies the month and year of each observation in \code{x} as mm-yyyy. By default (i.e., when \code{levels = TRUE}), this is a factor with chronologically ordered levels (i.e., levels ordered by year then month, for the inputted \code{x}). If \code{levels = FALSE}, a character vector is returned.
#'
#' @examples
#' #### Example (1): Extract the month-year category from Dates:
#' mmyy(as.Date("2016-01-01"))
#' mmyy(seq.Date(as.Date("2016-01-01"), as.Date("2016-12-01"), 10))
#'
#' #### Example (2): Extract the month-year category from POSIXct objects:
#' mmyy(as.POSIXct("2016-01-01"))
#' mmyy(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-12-01"), "20 days"))
#'
#' #### Example (3): By default, mmyy() returns an ordered factor
#' # ... with levels ordered chronologically:
#' mmyy(c(as.POSIXct(c("2017-01-02", "2016-01-02", "2016-03-01", "2017-02-03"))))
#'
#' #### Example (4): Ordered levels can be suppressed with levels = FALSE,
#' # ... in which case a character vector is returned:
#' mmyy(as.Date("2016-01-01"), levels = FALSE)
#' mmyy(seq.Date(as.Date("2016-01-01"), as.Date("2016-12-01"), 10), levels = FALSE)
#' mmyy(as.POSIXct("2016-01-01"), levels = FALSE)
#' mmyy(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-12-01"), "20 days"), levels = FALSE)
#'
#' @author Edward Lavender
#' @export
#'

mmyy <- function(x, levels = TRUE){
  #### Define months, adjusting those with only one digit to be 01
  mm <- as.character(lubridate::month(x))
  pos1  <- which(nchar(mm) == 1)
  if(length(pos1) > 0) mm[pos1] <- paste0(0, mm[pos1])
  #### Define years
  yy <- lubridate::year(x)
  #### Define month-year categories
  mmyy <- paste0(mm, "-", yy)
  #### Assign ordered factor levels, if requested.
  if(levels){
    dl <- data.frame(mm = mm, yy = yy, mmyy = mmyy)
    dl <- dl[order(dl$yy, dl$mm), ]
    mmyy <- factor(mmyy, levels = unique(dl$mmyy))
  }
  return(mmyy)
}


#######################################
#######################################
#### yday2date()

#' @title Convert a Julian day to a date/month/season
#' @description This function converts a Julian day to a date/month/season in any given year. This is useful, for instance, when making inferences from models of a response ~ Julian day, if you want to quickly draw inferences from models in terms of time units that are more familiar.
#'
#' @param yday A numeric input specifying the Julian day (the number of days since January 1st).
#' @param origin A character date (YYYY-MM-DD) that defines the first day of the year from which Julian day has been calculated.
#' @param verbose A logical input that defines whether or not to print the date, month and season.
#' @param return_list A logical input that defines whether or not to return the date, month  and year in a list.
#'
#' @examples
#' yday2date(16)
#' yday2date(16, return_list = FALSE)
#' yday2date(16, verbose = FALSE)
#'
#' @author Edward Lavender
#' @export

yday2date <-
  function(
    yday,
    origin = "2016-01-01",
    verbose = TRUE,
    return_list = FALSE){

    #### Define date, month and season
    # Define the date, based on a supplied origin:
    date <- as.Date(yday, origin = origin, tz = "UTC")
    # Define month
    month <- month.name[lubridate::month(date)]
    # Define season
    season <- lunar::terrestrial.season(date)

    #### Print output
    # Print the outputs if specified; normally this is sufficient
    # ... if yday2date is used to quickly gain an understanding of the
    # ... dates on plots with julian day
    if(verbose){
      print(paste("date:", date, "; month:", month, "; season:", season))
    }

    #### Return output
    # Return a list of the outputs if specified
    if(return_list){
      ls <- list(date = date, month = month, season = season)
      return(ls)
    } # close if(return_list){
  } # close function


#######################################
#######################################
#### yday_dbl

#' @title Compute the fraction of the year (days)
#' @description This function computes the Julian day on inputted times/dates as a fraction of the total number of days in each year.
#' @param time A vector of time(s) of class \code{\link[base]{Date}} or \code{\link[base]{DateTimeClasses}}.
#' @return The function returns a number for each element in \code{time} which is the Julian day as a fraction of the total number of days in that year.
#' @examples
#' # Compare the start of a non-leap and leap year:
#' yday_dbl(as.Date("2015-01-01"))
#' yday_dbl(as.Date("2016-01-01"))
#' # The end of the year
#' yday_dbl(as.Date("2015-12-31"))
#' yday_dbl(as.Date("2015-12-31"))
#' @author Edward Lavender
#' @export
#'

yday_dbl <-
  function(time){
    yday <- lubridate::yday(time)
    yr <- lubridate::year(time)
    nday <- lubridate::yday(as.Date(paste0(yr, "-12-31")))
    yday_dbl <- yday/nday
    return(yday_dbl)
  }


#### End of code.
#######################################
#######################################
