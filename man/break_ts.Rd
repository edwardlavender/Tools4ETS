% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/break_ts.R
\name{break_ts}
\alias{break_ts}
\title{Break a time series and introduce gaps}
\usage{
break_ts(
  dat,
  timestamp,
  ind = NULL,
  n = 1,
  pos = list(),
  before = list(function(break_t) {
     break_t - 43200
 }),
  after = list(function(break_t) {
     break_t + 43200
 }),
  output = 1
)
}
\arguments{
\item{dat}{A dataframe to be broken. As a minimum, the dataframe should include a column which defines time stamps (see below).}

\item{timestamp}{A character string defining the column name in \code{dat} which contains times. \code{timestamp} can be an object of class numeric, integer or \code{\link[base]{DateTimeClasses}}.}

\item{ind}{A character string defining the column name in \code{dat} which distinguishes unique time series (see also \code{\link[Tools4ETS]{flag_ts}} and the code{flag} argument).}

\item{n}{A number or a numeric vector defining the number of breaks in each unique time series. Alternatively, \code{pos} can be provided (see below).}

\item{pos}{A list of positions at which to break each unique time series. This can be provided instead of \code{n} for more control.}

\item{before}{A list containing a function/functions which define(s) the number of time steps before each break position to be included in the break.}

\item{after}{As above, but after the break position.}

\item{output}{A numeric value defining the output type. If \code{output = 1}, the function returns a named list. This comprises: (1) 'breaks', a vector of TRUE/FALSE statements indicating the positions in \code{dat} that lie within break windows; (2) 'pos_breaks', a numeric vector of positions at which \code{dat} is broken; and (3) 'dat_broken', the dataframe in which these positions have been removed. If \code{output = 2}, only 'dat_broken' is returned.}
}
\value{
The function returns a list or a dataframe, depending on the input to \code{output} (see above).
}
\description{
This function 'breaks' a time series by introducing gaps either around (a) \code{n} randomly chosen positions or (b) user-defined positions. The width of each gap is determined by user-supplied functions which define the amount of time around each position to be removed. Datasets comprising multiple, independent time series can be broken by different amounts and/or at different positions. This can be useful to explore the effects of gaps in real-world data for model inferences (e.g. a model's ability to correctly infer the effects of covariates acting at different scales for different individuals with different amounts of missing data).
}
\examples{

#### Simulate some data
# First, we will consider a single, continuous, regular time series:
# Define a sequence of time stamps
t <- seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-01-10"), by = "2 mins")
# Define a dataframe
dat <- data.frame(timestamp = t)

#### Example (1): Break a time series at one position using default options
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           n = 1)
# By default, the function returns a list:
utils::str(break_ts_ls)
# We have broken the time series around n = 1 randomly chosen position(s),
# ... removing 721 rows
# ... based on the default before() and after() functions
length(which(break_ts_ls$breaks))
length(break_ts_ls$pos_breaks)
nrow(dat) - length(break_ts_ls$dat_broken)

#### Example (2) Break a time series at multiple randomly chosen positions
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           n = 2)
utils::str(break_ts_ls)

#### Example (3) The function can handle dat comprising multiple independent time series
# Simulate data and a unique flag for each independent time series
dat <- data.frame(timestamp = rep(t, 2))
dat$ind <- c(rep(1, length(t)), rep(2, length(t)))
# Break each independent time series around n = 1 randomly chosen position(s)
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           ind = "ind",
           n = 1)
utils::str(break_ts_ls)

#### Example (4) Break each time series by different amounts by supplying a vector to n
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           ind = "ind",
           n = c(1, 20))
utils::str(break_ts_ls)

#### Example (5) Break each time series at custom positions by supplying a list of positions
# In this case, we'll break the time series at the same positions
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           ind = "ind",
           pos = lapply(1:length(unique(dat$ind)), function(i){ c(50, 100, 1000) }))
utils::str(break_ts_ls)

#### Example (6) Break each time series by different amounts
before_funs <- list(function(break_t){ break_t - 10 }, function(break_t){ break_t - 100 })
after_funs <- list(function(break_t){ break_t +10 }, function(break_t){ break_t +100 })
break_ts_ls <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           ind = "ind",
           n = 1,
           before = before_funs,
           after = after_funs)
utils::str(break_ts_ls)

#### Example (7) Change output format to return the broken dataframe
dat_broken <-
  break_ts(dat = dat,
           timestamp = "timestamp",
           ind = "ind",
           n = 2,
           output = 2)
utils::str(dat_broken)

}
\author{
Edward Lavender
}
