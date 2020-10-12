#' @title Explore the effects of basis dimension on the residual autocorrelation in a GAM
#' @description For a generalised additive model of a response as a smooth function of some variable (e.g., time), this function evaluates how the residual autocorrelation changes with the basis dimension. Specifically, for each basis dimension in a sequence of basis dimensions, a user-defined model is evaluated and the autoregressive order 1 (AR1) parameter is calculated from the model. For large vectors or basis dimensions, models can be evaluated in parallel. The function then returns a list of models and a dataframe/plot which shows the relationship between the AR1 parameter and the basis dimension.
#' @param mod A function used to evaluate a \code{\link[mgcv]{gam}} model of a given basis dimension. The function should take a single argument, the basis dimension, and return an evaluated model.
#' @param ks A numeric vector of basis dimensions. The model is evaluated for each element in this vector.
#' @param est_AR1 A function which calculates the AR1 parameter from a model. The only input to this function should be the model returned by the evaluation of \code{mod}.
#' @param cl A cluster object created by \code{\link[parallel]{makeCluster}}. This is required if you want to fit models in parallel.
#' @param varlist A character vector containing the names of exported objects. This may be required if \code{cl} is supplied. This is passed to the \code{varlist} argument of \code{\link[parallel]{clusterExport}}. Exported objects must be located in the global environment.
#' @param plot A logical input which defines whether or not to produce a plot of the basis dimension against the AR1 parameter values.
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{pretty_plot}} to customise the plot.
#' @return The function returns a list with two elements: 'list_by_model' and 'dat'. 'list_by_model' is a list with two elements for each model: 'model', the evaluated model; and 'ar1', the estimated AR1 parameter. 'dat' is a dataframe which includes each basis dimension ('k') and the corresponding AR1 value ('ar1'). The function also returns a plot of the basis dimension against the AR1 parameter if \code{plot = TRUE}.
#'
#' @examples
#' #### Define example data
#' # For an example individual flapper skate, we will model depth ~ s(timestamp)
#' # For speed, we'll focus on a sample of available data
#' dat <- dat_flapper[dat_flapper$id == "A", ]
#' dat$timestamp <- as.numeric(dat$timestamp)
#' dat <- dat[1:500, ]
#'
#' #### Define wrapper functions
#' eval_mod <- function(k) mgcv::gam(depth ~ s(timestamp, k = k), data = dat)
#' eval_AR1 <- function(mod) Tools4ETS::estimate_AR1(stats::resid(mod))
#'
#' #### Example (1) Implement approach using default options
#' ls <- estimate_AR1_with_k(mod = eval_mod,
#'                          ks = c(5, 10),
#'                          est_AR1 = eval_AR1)
#' # The function returns a list that includes all fitted models and some output data
#' names(ls)
#' # In the list of outputs for each model, each element contains the model and the estimated ar1:
#' ls$list_by_model[[1]]
#' # ar1 values can be extracted as follows:
#' sapply(ls$list_by_model, function(elm) elm$ar1)
#' # The dat element provides a dataframe with knots and ar1 values:
#' ls$dat
#'
#' #### Example (2): Customise the plot
#' ls <- estimate_AR1_with_k(mod = eval_mod,
#'                          ks = c(5, 10),
#'                          est_AR1 = eval_AR1,
#'                          type = "b",
#'                          xlab = "basis dimension",
#'                          ylab = "AR1")
#'
#' #### Example (3): Implement model fitting in parallel
#' ls <- estimate_AR1_with_k(mod = eval_mod,
#'                          ks = c(5, 10),
#'                          est_AR1 = eval_AR1,
#'                          type = "b",
#'                          xlab = "basis dimension",
#'                          ylab = "AR1",
#'                          cl = parallel::makeCluster(2L),
#'                          varlist = c("eval_mod", "dat", "eval_AR1"))
#' sapply(ls$list_by_model, function(elm) elm$ar1)
#'
#' #### Examine ACF plots for each model
#' pp <- par(mfrow = c(1, 2))
#' acfs <-
#'   lapply(ls$list_by_model, function(mod_ls)
#'     stats::acf(stats::resid(mod_ls$model)))
#' par(pp)
#'
#' #### Visualise predictions for each model
#' pp <- par(mfrow = c(1, 2))
#' preds <-
#'   pbapply::pblapply(ls$list_by_model, function(mod_ls){
#'     mod <- mod_ls$model
#'     prettyGraphics::pretty_plot(dat$timestamp, dat$depth*-1,
#'                                 xlab = "Time",
#'                                 ylab = "Depth (m)",
#'                                 lwd = 4,
#'                                 type = "l")
#'     pred <- mgcv::predict.gam(mod)*-1
#'     lines(dat$timestamp, pred, col = "red")
#'   })
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

estimate_AR1_with_k <-
  function(mod,
           ks = c(5, 10, 15),
           est_AR1,
           cl = NULL,
           varlist = NULL,
           plot = TRUE,...
  ){

    #### Cluster set up, if required
    if(!is.null(cl) & !is.null(varlist)) parallel::clusterExport(cl = cl, varlist = varlist)

    #### Define a list of models and AR1 values
    # Loop over every basis dimension value (in parallel if requested)...
    list_by_model <-
      pbapply::pblapply(ks, cl = cl, function(k){
        # Fit the model given k
        m <- mod(k)
        # Calc ar1 given user-inputted function
        ar1 <- est_AR1(m)
        # Return a list containing the model and the ar1 value
        out <- list(model = m, ar1 = ar1)
        return(out)
      })
    if(!is.null(cl)) parallel::stopCluster(cl)

    #### Define a dataframe containing the basis dimension and each ar1 value
    # Extract ar1s
    ar1s <- sapply(list_by_model, function(elm) elm$ar1)
    # Define dataframe
    dat  <- data.frame(k = ks, ar1 = ar1s)

    #### Plot ar1 ~ k if requested
    if(plot){
      prettyGraphics::pretty_plot(dat$k, dat$ar1,...)
    }

    #### Return outputs
    out <- list(list_by_model = list_by_model, dat = dat)
    return(out)
  }
