#' @title Create a barplot of the number of individuals of each sex in timeseries
#' @description This function produces a barplot showing the number of individuals of each sex in a timeseries. To do this, the function takes in a dataframe which contains a column distinguishing individuals and a column defining their sex, which are specified as character strings. The function then identifies the sex of each unique individual, sums the number of individuals of each sex and produces a barplot. This function is primarily intended for use in \code{\link[Tools4ETS]{GAMS4DTS}}.
#'
#' @param dat A dataframe which contains a column which defines unique individuals and a column which defines their sex. Multiple observations from the same individual are supported because the function obtains the sex of each unique individual once.
#' @param id_column A character input which defines the name of the column in \code{dat} which distinguishes individuals.
#' @param f_column A character input which defines the name of the column in \code{dat} which defines the sex of each individual.
#' @param cex.axis A number which defines the font size for axis tick labels.
#' @param cex A number which defines the font sizes for axis labels.
#'
#' @return The function returns a pre-customised barplot showing the number of individuals of each sex in a dataframe.
#'
#' @examples
#'
#' #### Example (1): A simple example
#' # Define a dataframe
#' set.seed(1)
#' dat <- data.frame(individual = 1:10)
#' dat$sex <- sample(c("M", "F"), size = nrow(dat), replace = TRUE)
#' # Define barplot
#' barplot_ts_sex(dat = dat,
#'                id_column = "individual",
#'                f_column = "sex")
#'
#' #### Example (2) The function can support multiple observations for each individual
#' # Replicate dataframe 10 times reflecting 10 observations from each individual
#' dat_ls <- lapply(1:10, function(x){ dat$time <- x; return(dat) })
#' dat <- do.call(rbind, dat_ls)
#' dat <- dplyr::arrange(dat, dat$individual)
#' # Define barplot
#' barplot_ts_sex(dat = dat,
#'                id_column = "individual",
#'                f_column = "sex")
#'
#'
#' @author Edward Lavender
#' @export
#'

#########################################
#########################################
#### barplot_ts_sex()

barplot_ts_sex <-
  function(
    dat,
    id_column = "individual",
    f_column = "sex",
    cex.axis = 1,
    cex = 1
){

  #### create a new dataframe, containing the sex of each id_column
  sc <- data.frame(individual = unique(dat[, id_column]))
  sc[, f_column] <- factor(dat[, f_column][match(sc[, id_column], dat[, id_column])])

  #### count the number of individuals
  n_individuals <- length(unique(dat[, id_column]))
  # count the number of females and males
  nfemales <- length(which(sc[, f_column] == as.character(levels(sc[, f_column])[1])))
  nmales <- n_individuals - nfemales

  #### Define y axis and limits for plot
  yat <- pretty(c(0, n_individuals), 5)
  ylims <- c(0, max(yat))

  #### create barplot
  graphics::barplot(c(nfemales, nmales),
                    space = 0,
                    axes = F,
                    ylim = ylims)

  #### Add axes
  graphics::axis(side = 1, at = c(0.5, 1.5), labels = c("Female", "Male"), pos = 0, cex.axis = cex.axis)
  graphics::axis(side = 2, at = yat, pos = 0, las = 2, cex.axis = cex.axis)

  #### Add axes labels
  graphics::mtext(side = 1, "Sex", line = 2.5, cex = cex)
  graphics::mtext(side = 2, "Count", line = 2.5, cex = cex)

  # close function
}

#### End of code
#########################################
#########################################
