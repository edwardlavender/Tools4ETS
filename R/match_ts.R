###############################
###############################
#### match_ts_nearest()

#' @title Find the position in one vector that is nearest in time to a value in another dataframe
#' @import data.table
#'
#' @description This function is like \code{\link[base]{match}}, but the aim is, for a given sequence of times (\code{times}), to find the positions in another sequence of times (\code{lookup}) that are nearest in time to those in the first sequence. In other words, for every time inputted, \code{match_ts_nearest()} finds the position in another sequence of times which is nearest in time to that time. This is useful if, for example, you have an existing dataframe to which you want to add the observations, held in another dataframe, that are nearest in time to observations in the first dataframe (i.e., nearest neighbour interpolation). This function uses \code{\link[data.table]{data.table}} for fast matching, even with very large vectors.
#'
#' @param times A vector of timestamps for which you want to identify the position of the nearest timestamp in another vector (\code{lookup}).
#' @param lookup A vector of timestamps for which you will determine the position of the nearest timestamp to each time in \code{times}.
#'
#' @details If there are multiple matches, only the first is returned.
#'
#' @return For a sequence of times (\code{times}), the function returns a vector of the positions of the nearest times in another sequence (\code{lookup}).
#'
#' @examples
#'
#' #### Define example data (1)
#' # Define dataframe to which we want to add information
#' d1 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "hours"))
#' # Define dataframe in which information is contained
#' d2 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "mins"))
#' d2$vals <- runif(nrow(d2), 0, 50)
#'
#' #### Example (1): Given a sequence of times, identify the positions of the nearest
#' # ... corresponding observations in another sequence:
#' # Use match_ts_nearest() to add information to the first dataframe based on second dataframe
#' d1$position_in_d2 <- match_ts_nearest(times = d1$t, lookup = d2$t)
#' d1$vals <- d2$vals[d1$position_in_d2]
#' # Examine
#' head(cbind(d1, d2[d1$position_in_d2, ]))
#'
#' #### Example (2): Relative to the times in 'times', the nearest times in lookup may be
#' # ... before/after a given observation:
#' t1 <- as.POSIXct(c("2016-01-01 00:00:01", "2016-01-01 00:10:00",
#'                    "2016-01-01 00:20:01", "2016-01-01 00:30:01"))
#' t2 <- as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:11:00",
#'                     "2016-01-01 00:50:01", "2016-01-01 00:22:01"))
#' # The correct order here is as follows:
#' # The first observation in t1 is nearest (before) to t2[1]
#' # The second observation in t2 is nearest (after) t2[2]
#' # The third observation in t1 is nearest (before) t2[4]
#' # The fourth observation in t2 is nearest (before) t2[4]
#' # This is what is returned by match_ts_nearest():
#' match_ts_nearest(t1, t2)
#'
#' #### Example (3) Input observations ('times' or 'lookup') do not need to be ordered by time:
#' ## Example with 'times' unordered:
#' t1_unordered <- t1[c(2, 1, 4, 3)]
#' # Manual examination of nearest observations
#' t1_unordered; t2
#' # First observation in t1_unordered is nearest to t2[2]
#' # Second observation in t1_unordered is nearest to t2[1]
#' # Third observation in t1_unordered is nearest to t2[4]
#' # Fourth observation in t1_unordered is nearest to t2[4]
#' # Implement match_ts_nearest():
#' match_ts_nearest(t1_unordered, t2)
#' ## Example with 'lookup' unordered
#' t2_unordered <- t2[c(3, 1, 2, 4)]
#' t1; t2_unordered
#' # Correct order is 2, 3, 4, 4
#' match_ts_nearest(t1, t2_unordered)
#' ## Example with both 'times' and 'lookup' unordered
#' t1_unordered; t2_unordered
#' # correct output is: 3, 2, 4, 4
#' match_ts_nearest(t1_unordered, t2_unordered)
#'
#' @seealso \code{\link[Tools4ETS]{match_ts_nearest_by_key}} is an extension of \code{match_ts_nearest()} to account for different factor levels when these to be included in the matching process.
#'
#' @author Edward Lavender
#' @export
#'

match_ts_nearest <- function(times, lookup){
  # Define data.table objects, adding indices to both tables;
  # We will use the index to ensure that observations are returned in the correct order.
  # This also means that 'times' input does not need to be arranged by time.
  dt1 <- data.table::data.table(t = times, index_times = 1:length(times))
  dt2 <- data.table::data.table(t = lookup, index_lookup = 1:length(lookup))
  # Set the key for both tables, arranging by time
  data.table::setkey(dt1, t)
  data.table::setkey(dt2, t)
  # Join the data tables by the observations that are nearest in time
  # This adds the index_lookup column to dt1, adding the values in index_lookup
  # ... that are nearest in time to the times in dt1
  djoin <- dt2[dt1, roll = "nearest", mult = "first"]
  # Reorder djoin by index_times so that the vector of positions in lookup
  # ... returned is in the appropriate order given order of inputted times
  # NB set index times to NULL to avoid 'no visible binding for global variable ‘index_times’'
  index_times <- NULL
  data.table::setkey(djoin, index_times)
  return(djoin$index_lookup)
}


###############################
###############################
#### match_ts_nearest_by_key()

#' @title Match timeseries by key and time
#' @description For two dataframes, \code{d1} and \code{d2}, this function finds the positions in the second dataframe which, for each key (e.g., factor level, individual) in the first dataframe, are nearest in time (i.e., nearest neighbour interpolation accounting for observations from different factor levels).
#'
#' @param d1 A dataframe which includes a column that defines factor levels and a column that defines timestamps. The names of these columns need to match those in \code{d2}.
#' @param d2 A dataframe which includes a column that defines factor levels and a column that defines timestamps. The names of these columns need to match those in \code{d1}.
#' @param key_col A character that defines the column name in \code{d1} and \code{d2} that distinguishes factor levels.
#' @param time_col A character that defines the column name in \code{d1} and \code{d2} that defines timestamps.
#'
#' @details If there are multiple matches, only the first is returned.
#'
#' @return For a dataframe comprising observations from a series of factor levels (e.g., individuals) collected through time, the function returns a vector of positions in a second dataframe which, for the appropriate factor level, are nearest in time.
#'
#' @seealso This is an extension of \code{\link[Tools4ETS]{match_ts_nearest}} to account for different factor levels when these to be included in the matching process.
#'
#' @examples
#' #### Example (1)
#' # Imagine we have observations from two keys (e.g., individuals) in two dataframes
#' # We want to add observations from the second dataframe into the first dataframe.
#' # Accounting for keys, the observations nearest in time in d2 for each row in d1 are
#' # ... 1, 2, 4, 4
#' d1 <- data.frame(t = as.POSIXct(c("2016-01-01 12:00:00",
#'                                   "2016-01-01 15:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 16:00:00")),
#'                  key = c(1, 1, 2, 2))
#' d2 <- data.frame(t = as.POSIXct(c("2016-01-01 13:00:00",
#'                                   "2016-01-01 14:00:00",
#'                                   "2016-01-01 12:00:00",
#'                                   "2016-01-01 15:00:00")),
#'                  key = c(1, 1, 2, 2))
#' match_ts_nearest_by_key(d1, d2, key_col = "key", time_col = "t")
#'
#' #### Example (2)
#' # Define dataframes
#' d1 <- data.frame(t = as.POSIXct(c("2016-01-01 18:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 13:00:00",
#'                                   "2016-01-01 14:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 21:00:00")),
#'                  key = c(2, 2, 2, 1, 1, 3))
#' d2 <- data.frame(t = as.POSIXct(c("2016-01-01 21:00:00",
#'                                   "2016-01-01 14:00:00",
#'                                   "2016-01-01 18:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 22:00:00",
#'                                   "2016-01-01 20:00:00",
#'                                   "2016-01-01 13:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 16:00:00")),
#'                  key = c(2, 2, 2, 2, 2, 3, 3, 1, 1),
#'                  vals = stats::runif(9, 0, 1))
#' # Add the to the dataframe
#' d1$position_in_d2 <- match_ts_nearest_by_key(d1, d2, key_col = "key", time_col = "t")
#' # Show that the index adds the correct key
#' d1$key_in_d2 <- d2$key[d1$position_in_d2]
#' # Show that the index adds the correct timestamp for that key
#' d1$t_in_d2 <- d2$t[d1$position_in_d2]
#' # We can now safely add values from d2 to d1:
#' d1$val_in_d2 <- d2$vals[d1$position_in_d2]
#' # Examine d1 and d2;
#' d1; d2
#'
#' @author Edward Lavender
#' @export
#'

###############################
###############################
#### match_ts_nearest_by_key()

match_ts_nearest_by_key <- function(d1, d2, key_col, time_col){
  # Check dataframes contain required columns
  stopifnot(all(c(key_col, time_col) %in% colnames(d1)))
  stopifnot(all(c(key_col, time_col) %in% colnames(d2)))
  # Check that all keys in d1 are in d2 and, if not, return a warning
  if(!all(unique(d1[, key_col]) %in% unique(d2[, key_col]))){
    warning("Not all unique keys in d1 are found in d2.")
  }
  # Convert tibbles to dataframes: this is necessary to correctly define data.tables, below.
  if(inherits(d1, "tbl")) d1 <- data.frame(d1)
  if(inherits(d2, "tbl")) d2 <- data.frame(d2)
  # Define datatables
  dt1 <- data.table::data.table(ky = d1[, key_col], t = d1[, time_col],  d1_index = 1:nrow(d1))
  dt2 <- data.table::data.table(ky = d2[, key_col], t = d2[, time_col],  d2_index = 1:nrow(d2))
  # Set the key for both tables, arranging by key then time
  ky <- NULL; t <- NULL;
  data.table::setkey(dt1, ky, t)
  data.table::setkey(dt2, ky, t)
  # Join the data tables by the observations by key and nearest in time
  djoin <- dt2[dt1, roll = "nearest", mult = "first"]
  # Reorder djoin by d1_index to match input order
  d1_index <- NULL
  data.table::setkey(djoin, d1_index)
  return(djoin$d2_index)
}


###############################
###############################
#### pair_ts()

#' @title Pair timeseries
#' @description This function adds observations from one timeseries to another timeseries using a matching process (e.g., nearest neighbour interpolation). This is useful when you have a main dataframe to which you need to add observations (e.g., those occurring closest in time) from another dataframe.
#'
#' @param d1 A dataframe that contains, at a minimum, a vector of timestamps, to which observations are added from \code{d2}.
#' @param d2 A dataframe that contains, at a minimum, a vector of timestamps and associated observations, to be added to \code{d1}.
#' @param time_col A character that defines the name of the column that contains timestamps in \code{d1} and \code{d2}.
#' @param key_col (optional) A character that defines the name of the column that contains keys in \code{d1} and \code{d2}. This is required for \code{method = "match_ts_nearest_by_key"} (see below).
#' @param val_col A character that defines the name of the column that contains observations in \code{d2}.
#' @param method A character that defines the matching method. The options currently implemented are \code{"match_ts_nearest"}, which implements \code{\link[Tools4ETS]{match_ts_nearest}} and \code{"match_ts_nearest_by_key"} which implements \code{\link[Tools4ETS]{match_ts_nearest_by_key}}.
#' @param min_gap (optional) A number that defines the minimum time gap (in user-defined units, see \code{units}, below) between times in \code{d1} and the times of observations that are added to \code{d1} from \code{d2}. This is useful if, for instance, some of the nearest observations in \code{d2} occurred long before the nearest observations in \code{d1}. If provided, the function counts the number of observations which do not meet this requirement and, if requested via \code{control_beyond_gap}, removes these from the returned dataframe or sets them to NA (see below).
#' @param max_gap As above, for \code{min_gap}, but the maximum time gap.
#' @param units A character that defines the units of the inputted \code{min_gap} or \code{max_gap}. This is passed to \code{\link[base]{difftime}}.
#' @param control_beyond_gap A character that defines whether or not to rows from \code{d1} that contain observations from \code{d2} that exceed \code{min_gap} or \code{max_gap} to NA (\code{"NA"}) or to remove those rows (\code{"remove"}).
#'
#' @return The function returns a dataframe, \code{d1}, as inputted, with an added column (whose name is given by \code{val_col}), comprising values added from another dataframe, \code{d2}. Any observations in \code{d1} for which there are not observations in \code{d2} occurring within some time window (defined by \code{min_gap} and \code{max_gap}), if specified, are counted and, if requested, removed from the returned dataframe.
#'
#' @examples
#' #### Example (1) Using method = "match_nearest_ts()"
#' # Define dataframe to which we want to add information
#' d1 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "hours"))
#' # Define dataframe in which information is contained
#' d2 <- data.frame(t = seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "mins"))
#' d2$vals <- runif(nrow(d2), 0, 50)
#' pair_ts(d1, d2, time_col = "t", val_col = "vals", method = "match_ts_nearest")
#'
#' #### Example (2) Using method = "match_nearest_ts_by_key()"
#' # Define dataframes
#' d1 <- data.frame(t = as.POSIXct(c("2016-01-01 18:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 13:00:00",
#'                                   "2016-01-01 14:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 21:00:00")),
#'                  key = c(2, 2, 2, 1, 1, 3))
#' d2 <- data.frame(t = as.POSIXct(c("2016-01-01 21:00:00",
#'                                   "2016-01-01 14:00:00",
#'                                   "2016-01-01 18:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 22:00:00",
#'                                   "2016-01-01 20:00:00",
#'                                   "2016-01-01 13:00:00",
#'                                   "2016-01-01 17:00:00",
#'                                   "2016-01-01 16:00:00")),
#'                  key = c(2, 2, 2, 2, 2, 3, 3, 1, 1),
#'                  vals = stats::runif(9, 0, 1))
#' pair_ts(d1, d2,
#'         time_col = "t", key_col = "key", val_col = "vals",
#'         method = "match_ts_nearest_by_key")
#'
#' #### Example (3) Flag observations that exceed a min/max gap
#' pair_ts(d1, d2,
#'         time_col = "t", key_col = "key", val_col = "vals",
#'         method = "match_ts_nearest_by_key",
#'         min_gap = 0,
#'         max_gap = 1,
#'         control_beyond_gap = "remove")
#'
#' @author Edward Lavender
#' @export

pair_ts <- function(d1,
                    d2,
                    time_col,
                    key_col = NULL,
                    val_col,
                    method = "match_ts_nearest",
                    min_gap = NULL,
                    max_gap = min_gap,
                    units = "mins",
                    control_beyond_gap = NULL){

  #### Identify current columns in d1
  cols_in_d1 <- colnames(d1)

  #### Checks
  stopifnot(time_col %in% colnames(d1) & time_col %in% colnames(d2))
  stopifnot(val_col %in% colnames(d2))
  check_value(arg = "method", input = method, supp = c("match_ts_nearest", "match_ts_nearest_by_key"))
  if(!is.null(control_beyond_gap))
    check_value(arg = "control_beyond_gap", input = control_beyond_gap, supp = c("NA", "remove"))

  #### Implement match_ts method
  if(method == "match_ts_nearest"){
    d1$position_in_d2 <- match_ts_nearest(d1[, time_col], d2[, time_col])
  } else if(method == "match_ts_nearest_by_key"){
    if(is.null(key_col)){
      stop("key_col must be specified for method = 'match_ts_nearest_by_key'")
    } else{
      stopifnot(key_col %in% colnames(d2))
    }
    d1$position_in_d2 <- match_ts_nearest_by_key(d1, d2, key_col = key_col, time_col = time_col)
  }

  #### Add values to d1 from d2
  d1[, val_col] <- d2[d1$position_in_d2, val_col]

  #### Check whether min or max gap have been exceeded, if requested
  if(!is.null(min_gap) | !is.null(max_gap)){

    ## Add times in d2 to d1
    d1$time_in_d2 <- d2[d1$position_in_d2, time_col]
    # Compute difference in time using specified units
    # Use drop = TRUE in case a tibble has been provided.
    d1$difftime <- as.numeric(difftime(d1[, "time_in_d2", drop = TRUE], d1[, time_col, drop = TRUE], units = units))

    ## Check whether the min_gap was exceeded
    min_gap_exceeded <- any(d1$difftime < min_gap, na.rm = TRUE)
    l_min_gap_exceeded <- length(which(min_gap_exceeded))
    if(min_gap_exceeded){
      warning(paste0(l_min_gap_exceeded, " observations exceeded min_gap."))
      if(!is.null(control_beyond_gap)){
        if(control_beyond_gap == "remove"){
          d1 <- d1[which(d1$difftime >= min_gap), ]
        } else if(control_beyond_gap == "NA"){
          d1[which(d1$difftime < min_gap), val_col] <- NA
        }
      }
    }

    ## Check whether the max_gap was exceeded
    max_gap_exceeded <- any(d1$difftime > max_gap, na.rm = TRUE)
    l_max_gap_exceeded <- length(which(max_gap_exceeded))
    if(max_gap_exceeded){
      warning(paste0(l_max_gap_exceeded, " observations exceeded max_gap."))
      if(!is.null(control_beyond_gap)){
        if(control_beyond_gap == "remove"){
          d1 <- d1[which(d1$difftime <= max_gap), ]
        } else if(control_beyond_gap == "NA"){
          d1[which(d1$difftime > max_gap), val_col] <- NA
        }
      }
    }

  } else{
    if(!is.null(control_beyond_gap)) warning("control_beyond_gap is ignored: both min_gap and max_gap are NULL.")
  }

  #### Return dataframe
  d1_to_return <- d1[, c(cols_in_d1, val_col)]
  if(nrow(d1_to_return) == 0){
    warning("No observations remain in d1; NULL returned.")
    return(NULL)
  } else return(d1_to_return)
}



#### End of code.
###############################
###############################
