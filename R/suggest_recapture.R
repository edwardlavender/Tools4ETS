#' @title Explore putative, unrecorded recapture events within depth time series
#' @description For animals tagged with archival tags, this function highlights putative, unrecorded recapture events (when tagged individuals may have been captured by recreational anglers). To do this, the function bins each individual's time series into a series of time windows. In each window, the function identifies all of the time steps when the individual's depth was shallower than a specified \code{threshold}: these are `putative capture events'. Windows that contain putative capture event(s) are termed capture windows. For each capture window, the depth time series can be inspected visually to clarify whether or not movement into shallow water is likely to reflect capture (e.g., if putative capture events occurred at a reasonable time of day and the shape of changes in depth are consistent with that expected to be caused by capture). The function returns a list, with one element for each capture window, that contains the depth time series within that window.
#'
#' @param data A dataframe of depth time series. This must contain a POSIXct vector of time stamps (`timestamp') and a numeric vector of absolute depths (`depth'). A column can be included to distinguish levels of a grouping factor (e.g., individuals) (see \code{fct}).
#' @param fct (optional) A character that defines the name of a column in \code{data} that distinguishes levels of a grouping factor (e.g., individuals).
#' @param threshold A number that defines the depth threshold; depth(s) shallower than \code{threshold} are flagged as putative capture event(s).
#' @param window A number that defines the duration (s) of capture windows.
#' @param plot A logical input that defines whether or not to plot, for each capture window, depth (which is internally negated for plotting) against time, with putative capture events flagged.
#' @param pretty_axis_args If \code{plot = TRUE}, \code{pretty_axis_args} is named list, passed to \code{\link[prettyGraphics]{pretty_axis}}, to control plot axes.
#' @param buffer A number, or a vector of two numbers, used to adjust the time (s) of the first and last putative capture events in each capture window to define x axis limits. For example, \code{buffer = c(60, 90)} minuses 60 s from the time of the first capture event in a window and adds 90 s to the time of the last capture event in the window and these times are taken to define the x limits of the plot.
#' @param add_threshold_markers (optional) If \code{plot = TRUE}, \code{add_threshold_markers} is a named list, passed to \code{\link[graphics]{lines}}, used to flag the time steps when the depth was shallower than the \code{threshold} on the plot using vertical lines. \code{add_threshold_markers = list()} implements default graphical options. \code{add_threshold_markers = NULL} suppresses this option.
#' @param add_additional (optional) If \code{plot = TRUE}, \code{add_additional} is function used to add additional elements to each plot. This must accept two arguments, even if they are ignored: an index (the capture window number) and the data within the capture window that is plotted.
#' @param prompt If \code{plot = TRUE}, \code{prompt} is a logical input that defines whether or not to pause function execution between sequential plots.
#' @param prompt_warning If \code{plot = TRUE} and \code{prompt = TRUE}, \code{prompt_warning} is a number that defines the number of capture windows with putative capture events above which the function will warn the user to reconsider \code{prompt = TRUE}.
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{pretty_plot}}, excluding \code{xlim}, which is controlled via \code{window} and \code{buffer}.
#'
#' @details This function was motivated by the need to identify putative, unrecorded recreational angling events for tagged flapper skate (\emph{Dipturus intermedius}) during individuals' time at liberty. The skate are typically targeted by recreational anglers over areas of deep (> 100 m) water, so angling events are visible as dramatic spikes in the depth time series caused by ascent into shallow water.
#'
#' For each individual's time series, the function groups observations into a series of \code{window}s and identifies all of the time steps within each window when the depth was shallower than the \code{threshold}. For each capture window (a window that contains putative capture event(s)), a dataframe of the depth time series/putative capture events within that window is returned. If \code{plot = TRUE}, for each window, a plot of the depth time series also returned with all moments when the depth was shallower than the threshold flagged for visible inspection. Putative capture events can then be evaluated according to the time of day when they occurred, the shape of the depth time series or other relevant factors.
#'
#' @return The function returns a list, with one element for each capture window, that contains the depth time series within that window. If \code{plot = TRUE}, for each window, a plot of the depth time series also returned.
#'
#' @examples
#' #### Example (1): Implement function with default options
#' # One potential capture event identified
#' events <- suggest_recapture(data = dat_flapper, fct = "id", prompt = FALSE)
#' utils::str(events)
#'
#' #### Example (2): Adjust capture threshold
#' events <- suggest_recapture(data = dat_flapper,
#'                             fct = "id",
#'                             threshold = 10,
#'                             prompt = FALSE)
#'
#' #### Example (3) Adjust capture window size
#' # Adjust capture window
#' events <- suggest_recapture(data = dat_flapper,
#'                             fct = "id",
#'                             threshold = 20,
#'                             window = 60*60*24*7,
#'                             prompt = FALSE)
#' # Adjust plot x limits via buffer
#' events <- suggest_recapture(data = dat_flapper,
#'                             fct = "id",
#'                             threshold = 20,
#'                             window = 60*60*24*7,
#'                             buffer = 60*60*24*2,
#'                             prompt = FALSE)
#'
#' #### Example (4): Customise plot
#'
#' ## Adjust vertical flags via add_threshold_markers
#' events <- suggest_recapture(data = dat_flapper,
#'                             add_threshold_markers = list(col = "red"),
#'                             fct = "id",
#'                             prompt = FALSE)
#'
#' ## Use add_additional e.g, to provide useful plot titles
#' # Define a function, which must depend on an index and the data, to add plot titles
#' # These will depend on the capture window index and
#' # ... we will add a * to flag putative events that occurred during the day
#' # ... (which are more likely to reflect actual capture events).
#' add_main <- function(index, data){
#'   # Define title
#'   title <- paste0("[", index, "]")
#'   # Identify events during the day and add this to title
#'   day <- data[data$depth <= 5, ]
#'   if(nrow(day) > 0){
#'     day$hour <- hour_dbl(day$timestamp)
#'     day <- day[day$hour >= 8 & day$hour <= 20, ]
#'   }
#'   if(nrow(day) > 0){
#'     title <- paste0(title, "*")
#'   }
#'   # Add plot title to plot
#'   mtext(side = 3, text = title, line = 1.25, font = 2)
#' }
#' # Implement function
#' events <- suggest_recapture(data = dat_flapper,
#'                             threshold = 10,
#'                             fct = "id",
#'                             add_additional = add_main,
#'                             prompt = FALSE)
#'
#' ## Pass additional arguments via ...
#' events <- suggest_recapture(data = dat_flapper,
#'                             fct = "id",
#'                             prompt = FALSE,
#'                             type = "l")
#'
#' @seealso \code{\link[Tools4ETS]{define_recapture}} defines the precise time of known capture events.
#' @author Edward Lavender
#' @export

suggest_recapture <- function(data,
                              fct = NULL,
                              threshold = 1,
                              window = 60 * 60 * 12,
                              plot = TRUE,
                              pretty_axis_args = list(side = 3:2),
                              buffer = 60*60,
                              add_threshold_markers = list(col = "blue", lty = 3),
                              add_additional = NULL,
                              prompt = TRUE,
                              prompt_warning = 100,
                              ...){

  #### Check inputs
  check_names(input = data, req = c("timestamp", "depth", fct))
  check...("xlim",...)

  #### Isolate potential capture events based on depth threshold
  # This returns a dataframe with the times of all moments when depth <= threshold
  if(!is.null(fct)) data$fct <- data[, fct] else data$fct <- 1L
  events <- data
  events$capture <- events$depth <= threshold
  events <- events[events$capture, ]

  #### If there are no possible events, return a message
  if(nrow(events) < 1) {
    message("No possible capture events identified.")
    return(invisible())
  }

  #### Define a list of capture windows
  # This is used to isolate the depth time series for each bin that contains capture event(s)
  events$bin <- as.character(cut(events$timestamp, paste0(window, " sec")))
  events$key <- paste0(events$fct, "-", events$bin)
  capture_windows <- split(events, events$key)

  #### Check plotting prompt
  if(plot){
    if(prompt & !is.null(prompt_warning)){
      if(length(capture_windows) >= prompt_warning){
        adj_prompt <-
          utils::askYesNo(paste0("The number of capture window(s) is ",
                                 length(capture_windows),
                                 ". Do you want to change prompt = TRUE to prompt = FALSE?"))
        if(adj_prompt) prompt <- FALSE
      }
    }
  }

  #### Define a list of depth time series within each capture window
  # We can plot events too.
  out <- lapply(1:length(capture_windows), function(i){

    #### Isolate capture_window
    capture_window <- capture_windows[[i]]

    #### Define xlim (for data subsetting and plotting)
    xlim <- range(capture_window$timestamp)
    if(length(buffer) == 1) buffer <- rep(buffer, 2)
    xlim[1] <- xlim[1] - buffer[1]
    xlim[2] <- xlim[2] + buffer[2]

    #### Get data around capture events in capture window
    data_for_window <- data[data$fct == capture_window$fct[1] &
                             data$timestamp >= xlim[1] &
                             data$timestamp <= xlim[2], ]

    #### Make plot of putative capture
    if(plot){

      ## Blank plot
      axis_ls <- prettyGraphics::pretty_plot(data_for_window$timestamp, abs(data_for_window$depth)*-1,
                                             xlim = xlim,
                                             pretty_axis_args = pretty_axis_args,...)

      ## Add lines demarking each recapture event
      if(!is.null(add_threshold_markers)){
        lapply(split(capture_window, 1:nrow(capture_window)), function(e){
          add_threshold_markers$x <- rep(e$timestamp, 2)
          add_threshold_markers$y <- axis_ls[[2]]$lim
          do.call(graphics::lines, add_threshold_markers)
        })
      }

      ## Add additional elements to plot according to the index and the data
      if(!is.null(add_additional)) add_additional(i, data_for_window)
      if(prompt & i < length(capture_windows)) readline(prompt = "Press [enter] to move on to the interval with possible recapture event(s) or [Esc] to exit...")
    }

    ##### Process and return data
    data_for_window$fct <- NULL
    return(data_for_window)
  })

  #### Return list of depth time series around each capture event window
  return(out)
}
