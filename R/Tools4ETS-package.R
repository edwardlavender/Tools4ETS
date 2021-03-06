#' @title Tools4ETS: Tools for Ecological Time Series.
#' @author Edward Lavender
#'
#' @description Tools for Ecological Time Series (Tools4ETS) is package designed to facilitate the simulation, processing, exploration, visualisation and modelling of (primarily ecological) time series (simply defined as data collected through time). This package has been particularly inspired by high-resolution depth time series, for example, of the kind collected by archival/data storage tags.
#'
#' @section Data Visualisation:
#' The prettyGraphics package was designed to support functions in Tools4ETS. This package includes a set of functions for visualising time series, across different factor levels, in relation to covariates and at different scales. This includes an interactive Shiny-Dashboard interface for exploring time series.
#'
#' @section Data Processing:
#' A set of functions is used for processing time series. This includes defining time categories and rates of change, matching/pairing time series, flagging independent sections of time series, thinning time series and breaking time series. A few functions are designed to deal with depth time series collected from archival tags specifically, including for the identification and description of recapture events.
#'
#' @section Simple Statistics:
#' Some functions facilitate the computation of simple statistics from time series or models, such as cumulative frequencies and autocorrelation parameters.
#'
#' @section Simulations:
#' A set of functions is used to simulate time series, either from de novo time series or generalised additive models. This includes an interactive Shiny-Dashboard interface for simulating and modelling depth time series using generalised additive models under different conditions.
#'
#' @docType package
#' @name Tools4ETS
NULL
