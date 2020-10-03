#' @title Tools4ETS: Tools for Ecological Timeseries.
#' @author Edward Lavender
#'
#' @description Tools for Ecological Timeseries (Tools4ETS) is package designed to facilitate the simulation, processing, exploration, visualisation and modelling of (primarily ecological) timeseries data (simply defined as data collected through time). This package has been particularly inspired by high-resolution depth timeseries data, for example, of the kind collected by archival/data storage tags.
#'
#' @section Data Visualisation:
#' The prettyGraphics package was designed to support functions in Tools4ETS. This package includes a set of functions for visualising timeseries data, across different factor levels, in relation to covariates and at different scales. This includes an interactive Shiny-Dashboard interface for exploring timeseries.
#'
#' @section Data Processing:
#' A set of functions is used for processing timeseries data. This includes defining time categories and rates of change, matching/pairing timeseries, flagging independent sections of timeseries, thinning timeseries and breaking timeseries. A few functions are designed to deal with depth timeseries collected from archival tags specifically, including for the identification and description of recapture events.
#'
#' @section Simple Statistics:
#' Some functions facilitate the computation of simple statistics from timeseries data or models, such as cumulative frequencies and autocorrelation parameters.
#'
#' @section Simulations:
#' A set of functions is used to simulate timeseries, either from de novo timeseries or generalised additive models. This includes an interactive Shiny-Dashboard interface for simulating and modelling depth timeseries using generalised additive models under different conditions.
#'
#' @docType package
#' @name Tools4ETS
NULL
