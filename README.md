
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Tools4ETS

<!-- badges: start -->

<!-- badges: end -->

`Tools4ETS` is an R package which provides tools for ecological
timeseries. This includes (1) data exploration, (2) timeseries
processing and (3) simulations to guide model-based ecological
inference. This functionality is provided by functions and two
interactive R Shiny applications. `Tools4ETS` was motivated by analyses
of new new, high resolution depth timeseries for a Critically Endangered
elasmobranch.

## Installation

You can install the released version of Tools4ETS from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Install from CRAN 
install.packages("Tools4ETS")

# Or, install the development version from github:
devtools::install_github("edwardlavender/Tools4ETS")
```

## Data Visualisation

`Tools4ETS` facilitates rapid timeseries visualisation in relation to
covariates, across factor levels (e.g. individuals) and timescales with
`plot_ts()` (a function based on the complementary `plot.pretty`
package) and an interactive R Shiny application (`visTS()`).

## Data processing

`Tools4ETS` streamlines common data processing operations for ecological
timeseries. These include:

  - **Flagging timeseries.** `flag_ts()` can be used to flag independent
    sections of timeseries in a dataset due to the presence of different
    factor levels (e.g. individuals) and/or breaks in timeseries.
    Different flag typescan be added, reflecting the requirements of
    different modelling approaches.  
  - **Thinning timeseries.** `thin_ts()` can be used to thin a dataset,
    accounting for independent timeseries. `thin_ts_iter()` implements
    `thin_ts()` iteratively to explore the change in autocorrelation and
    data volume with thinning.
  - **Breaking timeseries.** `break_ts()` can be used to induce breaks
    in timeseries.

`Tools4ETS` includes additional functions designed specifically for
processing depth timeseries, including the identification and
description of recapture events (`suggest_recapture()` and
`define_recapture()`) and the definition common covariates (e.g. via
`define_photoperiod()` and `define_time_blocks()`).

## Simulations

`Tools4ETS` advocates simulation-informed model-based inference; i.e,
the use of simulations to explore the consequences of data structure,
processing and modelling decisions for model performance and ecological
inferences.

This includes functions which facilitate posterior simulation from
generalised additive models (GAMs), a widely used modelling approach in
ecology; namely:

  - **Posterior simulation of expected values.**
    `simulate_posterior_mu()` can be used to simulate from the posterior
    of a GAM to compute ecologically-meaningful metrics, with confidence
    intervals, which are not directly estimated by a model.
  - \*\*Posterior simulation of new ‘observed values.\*\*
    `simulate_posterior_obs()` can be used to simulate new
    ’observations’ from a model, accounting for uncertainty in
    fitted coefficients, expected values, predictions and
    autocorrelation.
  - **Summarising the posterior distribution.** `summarise_posterior()`
    can be used to summarise posterior matrices (e.g. so that that they
    can be plotted with `plot.pretty::add_model_predictions()`.)

This also includes function for simulating *de novo* timeseries with
known properties; namely:

  - **Dataframe assembly.** `assemble_ts()` can be used to simulate
    timeseries dataframes with timestamps for multiple factor levels (if
    applicable), possibly at different resolutions, over different
    durations and/or with breaks.
  - **Define parameters.** `parameterise_smooth()` can be used to define
    functions/parameters which relate covariates to a response and to
    compare simulated smooths to those inferred by GAMs.  
  - **Response simulation.** `sim_ts()` can be used to simulate the
    values of a response variable given a user-defined model. For
    simulations including residual autocorrelation, `sigma_arima()`
    facilitates the simulate autocorrelated observations with known
    variance.

`Tools4ETS` also includes an interactive application designed to explore
the use of GAMs as a tool for modelling depth timeseries (`GAMS4DTS()`)
via the comparison of simulated datasets and model inferences under
different conditions.

## Interactive applications

`Tools4ETS` includes two interactive applications for (a) interactive
timeseries visualisation and (b) simulating/modelling depth timeseries
using GAMs. These can be launched using `launch_shiny()`.
