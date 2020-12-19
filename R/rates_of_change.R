########################################
########################################
#### difference()

#' @title Calculate the difference between two numbers or times
#' @description Calculate a difference between two numbers or times. For numeric objects, the difference is calculated as \code{x2 - x1}. For time objects (i.e., \code{\link[base]{DateTimeClasses}} or \code{\link[base]{Dates}}), the difference is calculated using \code{\link[base]{difftime}}. A function can be supplied to process outputs; e.g. \code{\link[base]{abs}}, or \code{\link[base]{numeric}} for \code{\link[base]{difftime}} objects.
#'
#' @param x2 A number, \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}} object.
#' @param x1 A number, \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}} object.
#' @param ... Other arguments passed to \code{\link[base]{difftime}} e.g. \code{units = "secs"}.
#' @param f A function to process outputs.
#'
#' @return The function returns the difference between two numbers or times.
#'
#' @examples
#'
#' #### Example (1) Numeric difference
#' difference(1, 2)
#'
#' #### Example (2) Numeric difference with post processing
#' difference(1, 2, f = abs)
#'
#' #### Example (3) Difference with times
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"))
#'
#' #### Example (4) Difference with times and units specified
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"), units = "mins")
#'
#' #### Example (5) Difference with times, specified units and post-processing
#' difference(as.POSIXct("2016-01-05"), as.POSIXct("2016-01-01"), units = "mins", f = as.numeric)
#'
#' @author Edward Lavender
#' @export
#'

difference <- function(x2, x1, f = NULL,...){
  if(class(x2)[1] %in% c("numeric", "integer")){
    d <- x2 - x1
  } else if(class(x2)[1] %in% c("POSIXct", "POSIXlt", "Date")){
    d <- difftime(x2, x1,...)
  }

  if(!is.null(f)){
    d <- as.numeric(d)
  }

  return(d)
}


#############################################
#############################################
#### serial_difference()

#' @title Calculate the difference between sequential observations
#' @description This function provides a quick method to calculate the duration between sequential observations. The function is based on \code{\link[Tools4ETS]{difference}} which can calculate the difference between numeric objects or timestamps (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}).
#'
#' @param x A vector of numbers or timestamps (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}) supported by \code{\link[base]{difftime}}.
#' @param na.rm A logical input defining whether or not to remove NAs from the output vector. If \code{FALSE}, the last value is \code{NA} because a difference cannot be calculated between the final observation and the 'next' observation. If \code{TRUE}, this \code{NA} (and any other NAs) are removed.
#' @param ... Other arguments passed to \code{\link[Tools4ETS]{difference}}. These include \code{units} (a character input specifying the units in which to return time differences) and \code{f}, a function to process differences.
#'
#' @return The function returns a vector of differences. Each value is the difference between one value and the next value in a sequence.
#'
#' @examples
#' #### Define some time-series data
#' d1 <- c(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-29"), by = "2 mins"),
#'         seq.POSIXt(as.POSIXct("2016-01-30"), as.POSIXct("2016-02-03"), by = "20 mins"),
#'         seq.POSIXt(as.POSIXct("2016-04-01"), as.POSIXct("2016-05-01"), by = "days")
#'         )
#' length(d1)
#'
#' #### Example (1): Calculate the duration between observations
#' duration <- serial_difference(d1)
#' table(duration)
#' head(duration)
#' tail(duration) # The last value is NA
#'
#' #### Example (2): Calculate the duration between observations in user-specified units
#' # ... by supplying the units argument, which is passed to difference() to and then to difftime()
#' duration <- serial_difference(d1, units = "mins")
#'
#' #### Example (3): Implement post-processing by supplying f to difference()
#' serial_difference(d1, units = "days", f = function(x){round(as.numeric(x), 3)})
#'
#' #### Example (4): Remove any NAs
#' duration <- serial_difference(d1, units = "days", na.rm = TRUE)
#' # The last value is no longer NA:
#' tail(duration)
#' # The sequence is one unit shorter than the input sequence due to the removal of the final NA:
#' length(d1) - length(duration)
#'
#' #### Example (5) Numeric example
#' x <- c(seq(0, 10, by = 1), seq(11, 20, by = 0.1))
#' serial_difference(x)
#' serial_difference(x, na.rm = TRUE)
#'
#' @author Edward Lavender
#' @export
#'

serial_difference <-
  function(
    x,
    na.rm = FALSE,...
  ){

    #### Updated: numeric and timestamp objects now supported.
    #### check format of timestamps:
    # if(!(class(x)[1] %in% c("POSIXct", "POSIXt", "Date"))){
    #   warning("Check the class of inputted timestamps. This should be a date-time or date object supported by difftime().")
    # }

    #### Define differences
    # dur <- as.numeric(difftime(dplyr::lead(x), x, units = units))
    dur <- difference(dplyr::lead(x), x,...)

    #### Process differences (update: implemented via difference() instead)
    # if(!is.null(fp)){
    #   dur <- fp(dur)
    # }

    #### Remove NA in output vector
    if(na.rm){
      posNA <- which(is.na(dur))
      dur <- dur[-c(posNA)]
    }

    #### Return differences
    return(dur)
  }


#### End of code.
#############################################
#############################################
