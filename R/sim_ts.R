#' @title Simulate the values of a response variable
#'
#' @importFrom magrittr %>%
#'
#' @description This function is used to simulate values of a response according to (a) user-defined functions for the effects of covariates and (b) a user-defined function for the simulation of 'observed' values of a response from the linear predictor. The function can incorporate linear and smooth functions of factors and continuous explanatory variables. The function can incorporate a random intercept term. For factors, the user needs to specify dummy variables in the formula (see below).
#'
#' @param alpha A numeric value which defines the model intercept.
#' @param alpha_sigma_random A numeric value which defines the random variation in intercept among factor levels (see below).
#' @param compute_lp A named list which is used to compute the linear predictor. The name of each element should correspond to the name of a variable which is found in \code{dat}. Each element should consist of (a) a function which takes in 'x', the values of that variable in \code{dat} and any other parameters, and (b) a named list of parameters and parameter values.
#' @param sim_obs A function which takes in the values of the linear predictor for each factor level and simulates the values of the response from the linear predictor.
#' @param dat A dataframe which contains variables used to predict the values of a response.
#' @param fct (optional) A character input which defines the name of a column in \code{dat} which distinguishes different time-series (i.e., those for different individuals).
#' @param seed (optional) A number passed to \code{\link[base]{set.seed}} which ensures simulated observations are reproducible.
#'
#' @return A numeric vector of the simulated response variable
#'
#' @examples
#'
#' #### Simulate a dataframe which we will use to simulate the depth of a
#' # ... hypothetical animal through time.
#' # We will simulate a small time-series (2 days in length) for two individuals,
#' # ... one male and one female.
#' # We will include sex, length, sun_angle, lunar_phase and julian_day in the dataframe:
#' set.seed(2)
#' dat <-
#'   assemble_ts(start_date = "2016-01-01",
#'                     start_date_variable = FALSE,
#'                     max_duration_days = 2,
#'                     duration_days_variable = FALSE,
#'                     resolution_minutes = 120,
#'                     n_individuals = 2,
#'                     longitude = 5,
#'                     latitude = 65,
#'                     tz = "UTC",
#'                     covariates = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
#'                     parameters = list(sex = list(Pf = 0.5, replace = FALSE),
#'                                       length = list(shape = 25, scale = 4,
#'                                                     plot_density_curve = FALSE))
#'   )
#'
#' #### Simulate 'observed' depths
#' # We will simulate depths in the scenario that depth is affected by
#' # ... linear functions of sex and length
#' # ... and smooth functions of sun_angle, lunar_phase and julian_day.
#' ## (a) Record sex in terms of 0s/1s
#' dat$sex <- (dat$sex == "M") + 0
#' # (b) Define a named list that we'll use to compute the values of the linear predictor
#' # For each variable we'll specify a function and a named list of parameters
#' # ... to be passed to that function.
#' compute_lp <- list(sex = list(f = linear,
#'                               param = list(a = 0, b = 25)
#' ),
#' length = list(f = linear,
#'               param = list(a = 0, b = 5)
#' ),
#' sun_angle = list(f = sigmoid,
#'                  param = list(x0 = 0, L = 100, k = 0.2)
#' ),
#' lunar_phase = list(f = quadratic,
#'                    param = list(a = -5, b = 1, h = 3, k = 25)
#' ),
#' julian_day = list(f = quadratic,
#'                   param = list(a = -0.001, b = 1, h = 183, k = 15)
#' )
#' )
#' ## (b) Simulate observations:
#' dat$depth <-
#'   sim_ts(
#'     alpha = 0,
#'     alpha_sigma_random = 0,
#'     compute_lp = compute_lp,
#'     sim_obs = function(lpi){
#'     lpi + stats::arima.sim(list(order = c(1,0,0), ar = 0.95), n = length(lpi), sd = 50) },
#'     dat = dat,
#'     fct = "individual",
#'     seed = 1)
#'
#' # Visualise simulated depth time-series
#' plot(dat$timestamp, dat$depth, type = "n")
#' lines(dat$timestamp[dat$individual == 1], dat$depth[dat$individual == 1])
#' lines(dat$timestamp[dat$individual == 2], dat$depth[dat$individual == 2])
#'
#' @author Edward Lavender
#' @export
#'
#####################################
#####################################
#### sim_ts()

# factors (e.g. sex must be coded as a number)

sim_ts <-
  function(
    alpha = 0,
    alpha_sigma_random = 0,
    compute_lp = list(),
    sim_obs = function(lpi){
      lpi + stats::arima.sim(list(order = c(1,0,0), ar = 0.95), n = length(lpi), sd = 50) },
    dat,
    fct = NULL,
    seed = NULL
    ){

    #### Set seed
    if(!is.null(seed)){
      set.seed(seed)
    }

    #### Define intercept
    dat$mu <- alpha

    #### Define random intercept terms based on fct
    if(alpha_sigma_random > 0){
      stopifnot(!is.null(fct))
      dat_ls <- split(dat, f = dat[, fct])
      dat$delta <-
        lapply(dat_ls, function(df){
          delta <- stats::rnorm(n = nrow(df), mean = 0, sd = alpha_sigma_random)
          return(delta)
          }) %>% unlist() %>% as.vector()
      dat$mu <- dat$mu + dat$delta
    }

    #### Calculate the value of mu (i.e. linear predictor) based on inputs
    vars <- names(compute_lp)
    computed_lp_ls <-
      mapply(compute_lp, vars, FUN = function(var_ls, var){
        f <- var_ls$f
        param <- var_ls$param
        param$x <- as.numeric(as.character(dat[, var])) # ensure can manage factors
        y <- do.call(f, param)
        return(y)
        }, SIMPLIFY = FALSE)
    computed_lp_dat <- do.call(cbind, computed_lp_ls)
    computed_lp_sum <- rowSums(computed_lp_dat)
    dat$mu <- dat$mu + computed_lp_sum

    #### Simulate observations
    if(!is.null(fct)){
      dat_ls <- split(dat, f = dat[, fct])
    } else{
      dat_ls <- list(dat)
    }
    obs <- lapply(dat_ls, function(df){
      sim_obs(df$mu)
    }) %>% unlist() %>% as.vector()

    return(obs)

  } # close function



#### End of code.
#####################################
#####################################
