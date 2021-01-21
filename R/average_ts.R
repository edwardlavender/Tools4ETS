#' @title Summarise a time series in bins
#' @description This function averages a time series (i.e., all of the values of a response variable collected through time) in each of a series of user-specified bins. The function can handle independent time series (e.g., for multiple individuals), average numeric explanatory variables alongside the response, incorporate other 'static' variables (e.g., the sex of each individual) and implement different types of averages.
#' @param dat A dataframe which includes observations collected over time.
#' @param split (optional) A character that defines the name of the column in \code{dat} that distinguishes independent time series.
#' @param timestamp A character that defines the name of the column in \code{dat} that contains time stamps, to be grouped into bins.
#' @param response A character that defines the name of the column in \code{dat} that contains the values of the response to be averaged in each bin.
#' @param breaks A number or vector which defines the number of unique cut points or the unique cut points themselves at which each independent time series is cut into bins. This is passed to \code{\link[base]{cut}}.
#' @param fun A function used to calculate the average response and the average value of any other other numeric variable specified in \code{average} in each bin.
#' @param first (optional) A character vector that defines the name(s) of the column(s) in \code{dat} for which the first observation for each independent time series should be retained in the averaged dataframe. This is useful for 'static' variables which do not change through time (e.g., the sex of each individual).
#' @param average (optional) A character vector that defines the name(s) of any other column(s) in \code{dat} that need to be averaged in each bin, like \code{response}.
#' @param as_time A function that converts a character vector of breaks into a vector of times. The default function is \code{as.POSIXct}.
#' @param order A character vector of column names that defines the order of desired columns in the returned dataframe.
#' @param verbose A logical input that defines whether or not to return messages to the console to monitor function progress.
#' @param ... Additional arguments passed to \code{\link[base]{cut}}.
#' @return The function returns a dataframe in which time stamps have been aggregated into a sequence of bins and the value of the response and any other numeric columns requested have been averaged over all the data in each bin. For static variables (e.g., the sex of an individual), the first observation for each independent time series may also be included.
#' @examples
#' dat_flapper_av <- average_ts(dat = dat_flapper,
#'                              split = "id",
#'                              timestamp = "timestamp",
#'                              response = "depth",
#'                              breaks = "hours",
#'                              first = "id",
#'                              average = "temp"
#'                              )
#' utils::head(dat_flapper_av)
#' @seealso \code{\link[prettyGraphics]{summarise_in_bins}}
#' @author Edward Lavender
#' @export
#'

average_ts <- function(dat,
                       split = NULL,
                       timestamp,
                       response,
                       breaks,
                       fun = mean,
                       first = NULL,
                       average = NULL,
                       as_time = as.POSIXct,
                       order = c(timestamp, response, first, average),
                       verbose = TRUE,...){

  #### Checks
  cols <- list(split, timestamp, response, first, average)
  names(cols) <- c("split", "timestamp", "response", "first", "average")
  lapply(1:5, function(i){
    if(any(!(cols[[i]] %in% colnames(dat)))){
      stop(paste0("Column name(s) inputted via '", names(cols)[i], "' are not included in 'dat': ",
                  paste0("'", cols[[i]][!(cols[[i]] %in% colnames(dat))], collapse = "',"), "'."))
    }
  })

  #### Split time series into independent sections
  if(!is.null(split)){
    if(verbose) cat("Step 1: Splitting time series into independent segments...\n")
    dat_ls <- split(dat, dat[, split])
  } else{
    dat_ls <- list(dat_ls)
  }

  #### Average each independent time series
  ## Loop over each independent time series...
  if(verbose) cat("Step 2: Averaging each independent time series...\n")
  dat_ls <- pbapply::pblapply(dat_ls, function(dat){

    ## Cut time series into breaks
    # if(verbose) cat(".. Step A: Cutting time series into breaks...\n")
    if(any("bin" %in% colnames(dat))) warning("dat$bin column is being overwritten...\n")
    dat$bin <- cut(x = dat[, timestamp], breaks = breaks,...)

    ## Average response for each break
    # if(verbose) cat("... Step B: Averaging response over each break...\n")
    dat_av <- tapply(dat[, response], dat[, "bin"], fun)
    dat_av <- data.frame(timestamp = names(dat_av), response = as.numeric(dat_av))

    ## Rename columns
    if(response != "response"){
      dat_av[, response] <- dat_av$response
      dat_av$response  <- NULL
    }
    if(timestamp != "timestamp"){
      dat_av[, timestamp] <- dat_av$timestamp
      dat_av$timestamp  <- NULL
    }

    ## Re-convert breaks to times...
    # if(verbose) cat("... Step C: Converting breaks to time stamps via as_time...\n")
    dat_av[, timestamp] <- as_time(dat_av[, timestamp])

    ## Add back the first record of any columns specified in 'first':
    if(!is.null(first)){
      # if(verbose) cat("... Step D: Retaining first record for each 'first' column...\n")
      for(fst in first){
        dat_av[, fst] <- dat[1, fst]
      }
    }
    ## Add back averages for each bin for any record specified in 'average':
    if(!is.null(average)){
      # if(verbose) cat("... Step E: Averaging records for each break in each 'average' column...\n")
      for(av in average){
        dat_av[, av] <- tapply(dat[, av], dat[, "bin"], fun)
      }
    }

    ## Return dat_av
    return(dat_av)
  })

  #### Join all independent time series
  if(verbose) cat("Step 3: Joining all independent time series...\n")
  dat_av <- dplyr::bind_rows(dat_ls)

  #### Order columns
  dat_av <- dat_av[, order]

  #### Print any unretained columns
  dat_cols <- colnames(dat)
  if(any(!(dat_cols %in% colnames(dat_av)))){
    dat_cols_dropped <- dat_cols[!(dat_cols %in% colnames(dat_av))]
    message(paste0("The following column(s) have been dropped from dat: ",
                  paste0("'", dat_cols_dropped, collapse = "', "),
                  "'."))
  }

  #### Return dataframe
  if(verbose) cat("Step 4: Returning dataframe...finished.\n")
  return(dat_av)

}
