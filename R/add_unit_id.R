#' @title Add unique unit IDs to time-series
#' @description This function extracts unique unit (e.g., electronic tag) IDs from a dataframe containing unit attributes, termed \code{dat_units}, that correspond to units listed in a time-series, termed \code{dat_ts}. This is useful when the same unit (e.g., an electronic tag) is deployed multiple times so that each 'unit' code (i.e., the code of a particular tag) does not correspond to a unique deployment. In this scenario, both the unit code and the time of deployment need to be included in the matching procedure to add unique unit IDs to the time-series.
#'
#' @param dat_ts A dataframe comprising time-series collected from 'units'. This must contain two named columns: 'timestamp', a  vector which defines the time of each observation (in \code{\link[base]{Date}} or \code{\link[base]{DateTimeClasses}} format); and 'unit', a vector which defines the unit code (which may be non unique). The column 'unit' should also be found in \code{dat_units} (see below).
#' @param dat_units A dataframe which contains unit metadata. This must contain four named columns: 'unit', as above for \code{dat_ts}; 'unit_id', a unique identifier for unit; 'start_time', a vector which defines the time of each unit's deployment; and 'end_time', a vector which defines the end of each unit's deployment. 'start_time' and 'end_time' should be of the same object type, and the same type as 'timestamp' in \code{dat_ts}.
#'
#' @return The function returns a vector of unit IDs, as defined in the \code{dat_units$unit_id} column, which correspond to each observation in the \code{dat_ts} dataframe.
#'
#' @examples
#' #### Define example data
#' # In this example, we have two units, but one has been re-deployed:
#' dat_units <- data.frame(unit = c(1, 1, 2),
#'                         unit_id = c(1, 2, 3),
#'                         start_time = as.POSIXct(c("2016-01-01", "2016-01-02", "2016-01-01")),
#'                         end_time = as.POSIXct(c("2016-01-02", "2016-01-03", "2016-01-02"))
#'                         )
#' # Our observational dataframe contains units but not unique unit IDs:
#' dat_ts <- data.frame(unit = c(1, 1, 2),
#'                      timestamp = as.POSIXct(c("2016-01-01 00:30:00",
#'                      "2016-01-02 00:30:00",
#'                      "2016-01-01 00:30:00"))
#'                      )
#'
#' #### Example (1): Add unit IDs to the observational dataframe
#' # The first observation corresponds to unit_id 1;
#' # The second observation corresponds to the same unit
#' # ... but a different deployment, and has unit_id = 2
#' # The third observation corresponds to unit id 3;
#' dat_ts$unit_id <- add_unit_id(dat_ts, dat_units)
#' dat_ts
#'
#' @author Edward Lavender
#' @export
#'


#############################################
#############################################
#### add_receiver_id()

add_unit_id <-
  function(dat_ts, dat_units){

    #### Checks
    # Check that dat_ts and dat_units contain required columns
    if(!all(c("unit_id", "unit", "start_time", "end_time") %in% colnames(dat_units))) stop("dat_units does not contain all required column names.")
    if(!all(c("unit", "timestamp") %in% colnames(dat_ts))) stop("dat_ts does not contain all required column names.")

    #### If some units were deployed, we need to
    # ... account for the unit code and the time of deployment when we add units to dat_ts
    if(any(duplicated(dat_ts$unit))){

      #### Check dat_ts/dat_units dataframes contain required information in correct format
      if(!all(class(dat_units$start_time) %in% class(dat_units$end_time))){
        stop("class(dat_units$start_time) and class(dat_units$end_time) must be equal.")
      }
      if(!any(class(dat_units$start_time) %in% class(dat_ts$timestamp))){
        stop("class(dat_units$start_time) must be the same as class(dat_ts$timestamp)")
      }
      if(class(dat_units$unit)[1] != class(dat_ts$unit)[1]){
        warning("class(dat_units$unit)[1] != class(dat_ts$unit)[1]; both coerced to character vectors.")
        dat_units$unit <- as.character(dat_units$unit)
        dat_ts$unit <- as.character(dat_ts$unit)
      }

      #### Define data.tables
      dat_ts$start_time <- dat_ts$timestamp
      dat_ts$end_time   <- dat_ts$timestamp
      dat_ts <- data.table::data.table(dat_ts)
      dat_units <- data.table::data.table(dat_units)
      unit <- NULL; start_time <- NULL; end_time <- NULL
      data.table::setkey(dat_units, unit, start_time, end_time)

      #### Implement data.table::foverlaps()
      order <- data.table::foverlaps(dat_ts, dat_units, type = "within", nomatch = NA, which = TRUE)

      #### Define dat_ts$receiver_id
      dat_ts$unit_id <- dat_units$unit_id[order$yid]

      #### If there are no redeployed units, we can simply use match()
      # ... to obtain the unit IDs for the dat_ts dataframe.
    } else{
      dat_ts$unit_id <- dat_units$unit_id[match(dat_ts$unit, dat_units$unit)]
    }

    #### Return the unit IDs
    lna <- length(which(is.na(dat_ts$unit_id)))
    if(lna > 0) warning(paste("unit IDs returned contain,", lna, "NAs (e.g. possibly due to incorrect start/end times)."))
    return(dat_ts$unit_id)
  }



#### End of code.
#############################################
#############################################
