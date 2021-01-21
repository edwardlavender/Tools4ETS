#' @title Thin a time series by selecting every nth observation
#' @importFrom magrittr %>%
#'
#' @description This function thins a time series by selecting every nth observation. The function provides flexibility to (a) distinguish among independent time series (e.g. time series for different individuals) via a user-supplied factor that can be determined \code{\link[Tools4ETS]{flag_ts}} and (b) return multiple thinned datasets for which thinning is started at different locations. The latter is useful for models of thinned time series because it is important to check that model inferences are not sensitive to the particular subset of data chosen.
#'
#' @param dat A dataframe to be thinned.
#' @param ind A character input which defines the column name \code{dat} which uniquely distinguishes each independent time series (i.e., \code{flag3} reported by \code{\link[Tools4ETS]{flag_ts}}).
#' @param flag1 A character input which defines the column name in \code{dat} which contains a logical vector with \code{TRUE} marking the first observation in each independent segment of time series (i.e. \code{flag1} reported by \code{\link[Tools4ETS]{flag_ts}}).
#' @param first A number or numeric vector which defines the position(s) at which thinning is initiated for each independent time series. If a single number is supplied, the function returns a thinned dataframe. If a vector of numbers is supplied, the function returns a list, of the same length, in which each element is a thinned dataframe comprising a different thinned dataframe - one with the same degree of thinning but in which thinning was initiated at a different position. The order of elements in the resultant list is the same as the order of elements in \code{first}.
#' @param nth A number which defines the degree of thinning (i.e. the selection of every \code{nth} row).
#'
#' @return The function returns a list or dataframe, depending on the input to \code{first} (see above).
#'
#' @examples
#' #### Simulate a dataframe to be thinned
#' # Define timestamps
#' t <- c(seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-02"), by = "6 hours"),
#'        seq.POSIXt(as.POSIXct("2016-01-02 18:00:00"), as.POSIXct("2016-01-04"), by = "6 hours")
#' )
#' # Apply flag_ts() function to flag independent time series
#' dat <- cbind(t, flag_ts(t, duration_threshold = 6*60, flag = 1:3))
#' nrow(dat)
#'
#' #### Example (1): Thin a single time series by selecting every nth position
#' # Thin the time series
#' dat_thin <- thin_ts(dat = dat,
#'                     nth = 2,
#'                     flag1 = "flag1"
#' )
#' # Examine the rows retained:
#' dat$row_retained <- dat$t %in% dat_thin$t
#' dat
#'
#' #### Example (2): Thin multiple independent time series via the'ind' argument
#' # Here, we now account for the fact that the data consists of multiple (two) independent time series
#' # ... as identified by flag_ts(), and the selection of positions is identical for both time series
#' # ... (i.e. the first observation in each time series)
#' dat$row_retained <- NULL
#' dat_thin <- thin_ts(dat = dat,
#'                     nth = 2,
#'                     flag1 = "flag1",
#'                     ind = "flag3")
#' dat$row_retained <- dat$t %in% dat_thin$t
#' dat
#'
#' #### Example (3): Multiple thinned datasets can be produced by supplying multiple values to 'first'
#' # This is useful because it is important to check that the exact thinned sample does not affect
#' # ... results (e.g. if thinned data are used in modelling)
#' dat$row_retained <- NULL
#' dat_thin_ls <- thin_ts(dat = dat,
#'                        nth = 2,
#'                        flag1 = "flag1",
#'                        ind = "flag3",
#'                        first = c(1, 2))
#' # With multiple 'first' values, the function returns a list, with a thinned dataframe for each
#' # ... first value:
#' utils::str(dat_thin_ls)
#' # Examine the difference:
#' dat$row_retained1 <- dat$t %in% dat_thin_ls[[1]]$t
#' dat$row_retained2 <- dat$t %in% dat_thin_ls[[2]]$t
#' dat
#'
#' @author Edward Lavender
#' @export
#'

###########################################
###########################################
#### thin_ts()

thin_ts <-
  function(dat,
           ind = NULL,
           flag1,
           first = 1,
           nth
           ){

    #### Split the dataframe into independent time series (if present)
    if(is.null(ind)){
      dat_ls <- list(dat)
    } else{
      stopifnot(is.character(ind))
      # Check for any unused factor levels and drop these, to avoid looping over empty levels.
      if(inherits(dat[, ind], "factor")){
        if(!(all(levels(dat[, ind]) %in% unique(dat[, ind])))){
          warning(paste0("Dropping empty factor levels in dat[,'", ind, "']."))
          dat[, ind] <- droplevels(dat[, ind])
        }
      }
      dat_ls <- split(dat, f = dat[, ind])
    }
    stopifnot(is.character(flag1))

    #### Define thin_ls
    # Create a list of with an element for each independent time series:
    # ... In turn, each element is a list with each element corresponding to
    # ... a dataframe for a specific first value
    thin_ls <-
      # loop over every independent time series
      lapply(dat_ls, function(ind_df){
        # ind_df <- dat_ls[[1]]
        id_ls <-
          # Loop over every supplied starting position:
          lapply(first, function(first_val){
            # first_val <- first[1]
            # determine the rows we want to keep for the time series in question
            # ...within the current time series:
            row_retain <- seq(first_val, nrow(ind_df), by = nth)
            # create a dataframe
            thin_from_specific_first <- data.frame(ind_df[row_retain, ])
            # Ensure that the first row is recognised as the start of an independent time series
            # Using flag1 (start_event) column:
            thin_from_specific_first[1, flag1] <- TRUE
            # return thin_from_specific_first
            return(thin_from_specific_first)
            })
        return(id_ls)
      }) # close function and lapply(unique(dat[, ind_df])


    #### Process the list to be more intuitive so we have one element
    # ... for each first value that is a dataframe containing all the data
    # ... across all unique time series
    thin <-
      # Loop over every first value
      lapply(1:length(first), function(first_val){
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

    #### Convert thin to a dataframe if only one value is given to first
    if(length(first) == 1){
      thin <- thin[[1]]
    }

    #### Return thin
    return(thin)
  }



#### close function.
###########################################
###########################################
