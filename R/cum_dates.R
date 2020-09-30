#' @title The total number of events through time (in days)
#' @description Calculate the total number of events by each date in a series of dates (i.e., a cumulative frequency distribution).
#'
#' @param data A vector of dates.
#' @param ... Other arguments passed to \code{\link[base]{as.Date}}.
#'
#' @return The function returns a dataframe with three columns: (1) 'date', the date; (2) 'freq', the number of observations on each unique date; and (3) 'cumul', the total number of events by each date (i.e. the cumulative frequency).
#'
#' @examples
#'
#' #### Example (1): A simple sequence of dates
#' cum_dates(seq.Date(as.Date("2016-01-01"), as.Date("2016-01-10"), 1))
#'
#' #### Example (2): Multiple records of each date
#' set.seed(1)
#' dates <- seq.Date(as.Date("2016-01-01"), as.Date("2016-01-10"), 1)
#' dates <- sort(sample(dates, size = 1000, replace = TRUE))
#' cum_dates(dates)
#'
#' @author Edward Lavender
#' @export
#'

###################################################
###################################################
#### cum_dates()

cum_dates <- function(data, ...){
  # work out the frequency of events on each date and put this in a new dataframe
  df.new <- data.frame(table(data))
  colnames(df.new) <- c("date", "freq")
  # format date correctly and order dataframe by date
  df.new$date <- as.Date(df.new$date, ...)
  df.new <- df.new[order(df.new$date), ]
  # use the cumsum function to calculate the total number of events by that date
  df.new$cumul <- cumsum(df.new$freq)
  return(df.new)
}

###################################################
###################################################
