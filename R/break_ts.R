#' @title Break a timeseries and introduce gaps
#' @description This function 'breaks' a timeseries by introducing gaps either around (a) \code{n} randomly chosen positions or (b) user-defined positions. The width of each gap is determined by user-supplied functions which define the amount of time around each position to be removed. Datasets comprising multiple, independent timeseries can be broken by different amounts and/or at different positions. This can be useful to explore the effects of gaps in real-world data for model inferences (e.g. a model's ability to correctly infer the effects of covariates acting at different scales for different individuals with different amounts of missing data).
#'
#' @param dat A dataframe to be broken. As a minimum, the dataframe should include a column which defines timestamps (see below).
#' @param timestamp A character string defining the column name in \code{dat} which contains times. \code{timestamp} can be an object of class numeric, integer or \code{\link[base]{DateTimeClasses}}.
#' @param ind A character string defining the column name in \code{dat} which distinguishes unique timeseries (see also \code{\link[Tools4ETS]{flag_ts}} and the code{flag} argument).
#' @param n A number or a numeric vector defining the number of breaks in each unique timeseries. Alternatively, \code{pos} can be provided (see below).
#' @param pos A list of positions at which to break each unique timeseries. This can be provided instead of \code{n} for more control.
#' @param before A list containing a function/functions which define(s) the number of timesteps before each break position to be included in the break.
#' @param after As above, but after the break position.
#' @param output A numeric value defining the output type. If \code{output = 1}, the function returns a named list. This comprises: (1) 'breaks', a vector of TRUE/FALSE statements indicating the positions in \code{dat} that lie within break windows; (2) 'pos_breaks', a numeric vector of positions at which \code{dat} is broken; and (3) 'dat_broken', the dataframe in which these positions have been removed. If \code{output = 2}, only 'dat_broken' is returned.
#'
#' @return The function returns a list or a dataframe, depending on the input to \code{output} (see above).
#'
#' @examples
#'
#' #### Simulate some data
#' # First, we will consider a single, continuous, regular timeseries:
#' # Define a sequence of timestamps
#' t <- seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-10"), by = "2 mins")
#' # Define a dataframe
#' dat <- data.frame(timestamp = t)
#'
#' #### Example (1): Break a timeseries at one position using default options
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            n = 1)
#' # By default, the function returns a list:
#' utils::str(break_ts_ls)
#' # We have broken the timeseries around n = 1 randomly chosen position(s),
#' # ... removing 721 rows
#' # ... based on the default before() and after() functions
#' length(which(break_ts_ls$breaks))
#' length(break_ts_ls$pos_breaks)
#' nrow(dat) - length(break_ts_ls$dat_broken)
#'
#' #### Example (2) Break a timeseries at multiple randomly chosen positions
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            n = 2)
#' utils::str(break_ts_ls)
#'
#' #### Example (3) The function can handle dat comprising multiple independent timeseries
#' # Simulate data and a unique flag for each independent timeseries
#' dat <- data.frame(timestamp = rep(t, 2))
#' dat$ind <- c(rep(1, length(t)), rep(2, length(t)))
#' # Break each independent timeseries around n = 1 randomly chosen position(s)
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            ind = "ind",
#'            n = 1)
#' utils::str(break_ts_ls)
#'
#' #### Example (4) Break each timeseries by different amounts by supplying a vector to n
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            ind = "ind",
#'            n = c(1, 20))
#' utils::str(break_ts_ls)
#'
#' #### Example (5) Break each timeseries at custom positions by supplying a list of positions
#' # In this case, we'll break the timeseries at the same positions
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            ind = "ind",
#'            pos = lapply(1:length(unique(dat$ind)), function(i){ c(50, 100, 1000) }))
#' utils::str(break_ts_ls)
#'
#' #### Example (6) Break each timeseries by different amounts
#' before_funs <- list(function(break_t){ break_t - 10 }, function(break_t){ break_t - 100 })
#' after_funs <- list(function(break_t){ break_t +10 }, function(break_t){ break_t +100 })
#' break_ts_ls <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            ind = "ind",
#'            n = 1,
#'            before = before_funs,
#'            after = after_funs)
#' utils::str(break_ts_ls)
#'
#' #### Example (7) Change output format to return the broken dataframe
#' dat_broken <-
#'   break_ts(dat = dat,
#'            timestamp = "timestamp",
#'            ind = "ind",
#'            n = 2,
#'            output = 2)
#' utils::str(dat_broken)
#'
#' @author Edward Lavender
#' @export
#'

##########################################
##########################################
#### break_ts()

break_ts <-
  function(dat,
           timestamp,
           ind = NULL,
           n = 1,
           pos = list(),
           before = list(function(break_t){ break_t - 43200 }),
           after = list(function(break_t){ break_t + 43200 }),
           output = 1){

    #### Define a list, one element for each unique timeseries
    if(is.null(ind)){
      dat_ls <- list(dat)
    } else{
      dat_ls <- split(dat, f = dat[, ind])
    }

    #### Define positions at which to break the timeseries, if these have not been provided
    if(length(pos) == 0){
      pos <-
        # Loop over the list of timeseries and the positions provided in n
        # (if n is a single number, we'll recycle this number and sample the same number
        # ... from each timeseries)
        mapply(dat_ls, n, FUN = function(dind, nsel){
          # sample nsel positions from each timeseries
          nn <- nrow(dind)
          psel <- sample(x = 1:nn, size = nsel, replace = FALSE)
          return(psel)
      }, SIMPLIFY = FALSE)
    }

    #### Define a list, with one element for each timeseries, defining where we'll break each timeseries
    dat_ls_breaks <-
      # Loop over each unique timeseries, the pos list, and the before/after functions
      mapply(dat_ls, pos, before, after, FUN = function(dind, psel, bf, af){
        #dind <- dat_ls[[1]]
        #psel <- pos[[1]]
        #bf <- before[[1]]
        #af <- after[[1]]
        # Define a new column, pbreak, in which we'll specify FALSE/TRUE to make break positions
        dind$pbreak <- FALSE
        # If there are positions at which we'll break the timeseries
        if(length(psel) > 0){
          # For each position
          for(p in psel){
            # Extract the value of the timestamp column at that break position
            pbreak_timestamp <- dind[p, timestamp]
            # Define all the positions around (and including) this position that we'll remove
            paround <- which(dind[, timestamp] >= bf(pbreak_timestamp) & dind[, timestamp] <= af(pbreak_timestamp))
            # Define true at these positions
            dind$pbreak[paround] <- TRUE
          }
        }
        # Return our vector of TRUE/FALSE for each timeseries
        return(dind$pbreak)
      }, SIMPLIFY = FALSE)

    #### Define the positions at which the timeseries will be broken
    pbreaks_logic <- as.vector(unlist(dat_ls_breaks))
    pbreaks <- which(pbreaks_logic)

    #### Break the timeseries
    dat_broken <- dat[-c(pbreaks), ]

    #### Return the desired output
    if(output == 1){
      return(list(breaks = pbreaks_logic, pos_breaks = pbreaks, dat_broken = dat_broken))
    } else if(output == 2){
      return(dat_broken)
    } else{
      warning("Output type not supported; output == 2 assumed.")
      return(dat_broken)
    }

  } # close function


#### End of code.
##########################################
##########################################
