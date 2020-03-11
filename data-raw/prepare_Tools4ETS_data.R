##########################################
##########################################
#### prepare_Tools4ETS_data.R

#### Read in raw data
dat_flapper <- readRDS("data-raw/dat_flapper_raw.rds")


##########################################
#### dat_flapper
# A sample of flapper skate data

#### Examine structure
str(dat_flapper)
# 'data.frame':	20161 obs. of  4 variables:
#  $ timestamp: POSIXct, format: "2016-05-08 23:00:00" "2016-05-08 23:02:00" "2016-05-08 23:04:00" "2016-05-08 23:06:00" ...
#  $ temp     : num  8.56 8.56 8.59 8.56 8.56 8.56 8.56 8.59 8.56 8.56 ...
#  $ depth    : num  108 107 107 106 104 ...
#  $ id       : Factor w/ 2 levels "A","B": 1 1 1 1 1 1 1 1 1 1 ...



##########################################
#### dat_ts_axis
# A list of custom axis limits/axis positions/labels for ts variables important for DTS
# ... (i.e., sun angle, lunar phase and julian day) for pretty plots, including in GAMS4DTS.

# Define a sequence of days (the year is irrelevant)
date_seq <- seq.Date(as.Date("2016-01-01"), as.Date("2016-12-01"), by = "months")
# determine the julian day at the start of each month
ydays <- lubridate::yday(date_seq)
# define month names
months <- month.abb[lubridate::month(date_seq)]

dat_ts_axis <-
  list(sun_angle =
         list(lim = c(-60, 60),
              axis = list(at = seq(-60, 60, by = 10),
                          labels = seq(-60, 60, by = 10)
              )

         ),
       lunar_phase =
         list(lim = c(0, 2*pi),
              axis = list(at = c(0, pi/2, pi, 3*pi/2, 2*pi),
                          labels = c(0,
                                     expression(pi/2),
                                     expression(pi),
                                     expression(3 * pi/2),
                                     expression(2 * pi)
                          )
              )
         ),
       julian_day =
         list(lim = c(1, 366),
              axis = list(at = ydays,
                          labels = paste0(months, " (", ydays, ")")
              )
         )
  )



#### Add prepared data to the package
# ... (use usethis::use_data())
usethis::use_data(dat_ts_axis, internal = TRUE, overwrite = TRUE)
usethis::use_data(dat_flapper, overwrite = TRUE)


#### End of code.
##########################################
##########################################
