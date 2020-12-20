#' @title GAMS4DTS
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#'
#' @description An interactive Shiny-Dashboard interface for exploring generalised additive models (GAMs) as a tool for modelling depth time-series (DTS). Within this application, depth time-series can be simulated, post-processed and modelled under different conditions. Comparisons of the simulated and inferred relationships under different conditions can be informative about the impacts of impacts of data structure, post-processing decisions and model structure for ecological inferences. A specific vignette provides further information for this function.
#'
#' @examples
#' \dontrun{
#' GAMS4DTS()
#' }
#'
#' @author Edward Lavender
#' @export
#'

################################################
################################################
#### GAMS4DTS()

GAMS4DTS <- function(){


################################################
################################################
#### Global parameters


cex.lab <- 1.75
cex.axis <- cex.lab - 0.2


################################################
################################################
#### User Interface

# Define the user interface
ui <-

  # Open up a dashboardPage
  dashboardPage(

    dashboardHeader(
      title = "Exploring Depth Trends",
      titleWidth = 250
      ),



    ################################################
    #### Define sidebar

    dashboardSidebar(
      width = 250,
      sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("angle-right")),
      # menuItem("")
      menuItem("Dataframe Assembly", tabName = "dat_assembly", icon = icon("angle-right")),
      menuItem("Parameterisations", tabName = "dat_smooths", icon = icon("angle-right")),
      menuItem("Simulate Depth", tabName = "dat_depth", icon = icon("angle-right")),
      menuItem("Thin Depth Time-series", tabName = "dat_thin", icon = icon("angle-right")),
      menuItem("Visualise Simulated Data", tabName = "vis_raw", icon = icon("angle-right")),
      menuItem("Define Model", tabName = "model_define", icon = icon("angle-right")),
      menuItem("Model Summary", tabName = "model_summary", icon = icon("angle-right")),
      menuItem("Model Covariate Plots", tabName = "model_covariates", icon = icon("angle-right")),
      menuItem("Model Predictions", tabName = "model_preds", icon = icon("angle-right")),
      menuItem("Model Diagnostics", tabName = "model_diag", icon = icon("angle-right"))
      )
    ),



    ################################################
    #### Define dashbar body for each table in the sidebar
    dashboardBody(

      tabItems(


        ################################################
        ################################################
        #### Introduction ("introduction")

        tabItem(tabName = "introduction",

                # Define a title for the page
                h2("A dynamic simulation and modelling environment for exploring generalised additive models as a tool for modelling depth time-series"),

                box(width = 12,
                    h2("Abstract"),

                    p("Depth-specific periodic behaviours are common in marine ecosystems. However, our understanding the drivers of these trends remains limited, in part, by available analytical techniques, which are either principally qualitative (e.g. wavelet analysis) or else require knowledge that lies outside of many ecologists' training (e.g. Hidden Markov Models). Generalised Additive Models (GAMs) strike a balance along this double-edged sword, being both a powerful modelling framework and a tool with which many ecologists are familiar, but their successful implementation requires correctly interpreting model fits and outputs, which remains challenging. Here, I present a interactive tool for the simulation and modelling of depth time-series using GAMs. The tool allows the user to simulate complex depth time-series according to a user-specified data-generating process that can incorporate variables commonly associated with depth in natural ecosystems (i.e., sex, body size, light levels, lunar phase, season). Simulated time-series can then be modelled using GAMs to investigate (a) the extent to which models recover simulated parameters and/or functions under different simulation/model scenarios; and (b) the consequences of mis-specifying of various aspects of the simulated data-generating process within models for model outputs. This tool will facilitate ecologists to build and interpret GAMs of time-series in cases where the true data-generating processes are unknown.",
                      style = "font-size: 25px; font-family: Times New Roman; text-align: justify;"
                    ) # close paragraph
                ) # close box


                # close tabItem()
                ),



        ################################################
        ################################################
        #### Dataframe Assembly ("dat_assembly")

        tabItem(tabName = "dat_assembly",
                h1("Dataframe Assembly"),
                h4("Assemble and examine a basic dataframe, like that which you would obtain from an depth-recording tag (but without depth at this stage)."),
                fluidRow(


                  ################################################
                  #### Add a box with options for data frame assembly

                  box(
                    title = "Assemble a basic dataframe structure",
                    width = 3,
                    numericInput(inputId = "seed",
                                 label = strong("Set the seed so that results are reproducible."),
                                 value = 1),
                    dateInput(inputId = "start_date",
                              label = strong("Select a start date."),
                              value = "2016-03-01"),
                    numericInput(inputId = "duration_days",
                                 label = strong("Define the number of days for which you will simulate data."),
                                 value = 10),
                    numericInput(inputId = "resolution_minutes",
                                 label = strong("Define the resolution, in minutes, at which to simulate depths."),
                                 value = 60),
                    numericInput(inputId = "n_individuals",
                                 label = strong("Define the number of individuals for which you will simulate data."),
                                 value = 10),
                    numericInput(inputId = "long_dst",
                                 label = strong("Define the longitude (DD) of the simulated tagging location."),
                                 value = 5.469723),
                    numericInput(inputId = "lat_dst",
                                 label = strong("Define the latitude (DD) of the simulated tagging location."),
                                 value = 56.41041),
                    numericInput(inputId = "sex_Pf",
                                 label = strong("Define the probability of sampling a female."),
                                 value = 0.5,
                                 min = 0,
                                 max = 1
                                 ),
                    numericRangeInput(inputId = "length_density_curve",
                                 label = strong("Define the shape and scale parameters of the Gamma distribution from which lengths are simulated."),
                                 value = c(10, 4),
                                 separator = "and") ,
                    plotOutput("length_density_curve")
                    # close box()
                    ),



                  ################################################
                  #### Add a box that displays the assembled dataframe

                  box(title = "Examine the dataframe that you have assembled",
                      width = 9,
                      DT::dataTableOutput("rendered_dat"))

                # close fluidRow()
                )
           # close tabItem
           ),



        ################################################
        #### Parameterisations ("dat_smooths")

        tabItem(tabName = "dat_smooths",
                h1("Parameterisations"),
                h4("Define the covariates that you would like to drive depth and associated parameters and/or functions."),



                ################################################
                #### Define the covariates which you would like to

                box(title = "Select the covariates you wish to drive depth in the simulation.",
                    width = 13,
                    pickerInput(inputId = "covariates",
                                label = strong("Select the covariates you wish to drive depth in the simulation."),
                                choices = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
                                selected = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
                                options = list(`actions-box` = TRUE,
                                               size = 10,
                                               `selected-text-format` = "count > 3"),
                                multiple = TRUE)

                ),



                ################################################
                #### sex (if inputted)

                conditionalPanel(condition = "input.covariates .includes('sex')",
                                 # create a new row
                                 fluidRow(

                                   # create a box for specifying the function and its parameters:
                                   box(title = "Define the parameters for sex",
                                       width = 3,

                                       sliderInput(inputId = "sex_contrast",
                                                   label = "Define parameter delta",
                                                   value = 0,
                                                   min = -250,
                                                   max = 250)

                                       # close box(
                                   ),

                                   # Create a box with visualises the contrast for sexc:
                                   box(title = "Visualisation of the contrast for sex",
                                       width = 9,
                                       plotOutput("sex_contrast")
                                       # close box which visualises the sun angle smooth:
                                   )

                                   # close fluidRow
                                 )

                                 # close conditionalPanel(condition = "input.covariates .includes('sex')
                ),



                ################################################
                #### length

                conditionalPanel(condition = "input.covariates .includes('length')",
                                 # create a new row
                                 fluidRow(

                                   # create a box for specifying the function and its parameters:
                                   box(title = "Define the parameters for length",
                                       width = 3,

                                       # sliderInput for beta term
                                       sliderInput(inputId = "length_beta",
                                                   label = "Define parameter beta",
                                                   value = 0,
                                                   min = -1.5,
                                                   step = 0.25,
                                                   max = 1.5)


                                       # close box(
                                   ),

                                   # Create a box with visualises the contrast for sexc:
                                   box(title = "Visualisation of the effect of length",
                                       width = 9,
                                       plotOutput("length_smooth")
                                       # close box which visualises the sun angle smooth:
                                   )

                                   # close fluidRow
                                 )

                                 # close conditionalPanel(condition = "input.covariates .includes('sex')
                ),



                ################################################
                #### Sun angle (if inputted)

                conditionalPanel(condition = "input.covariates .includes('sun_angle')",
                                 # create a new row
                                 fluidRow(

                                   # create a box for specifying the function and its parameters:
                                   box(title = "Define the smooth function and its parameters for sun angle",
                                       width = 3,

                                       radioButtons(inputId = "sun_angle_type",
                                                    label = "Choose the type of smooth function",
                                                    choices = c("quadratic", "sigmoidal"),
                                                    selected = "sigmoidal",
                                                    inline = T),



                                       ################################################
                                       #### sigmoidal option

                                       conditionalPanel(condition = "input.sun_angle_type == 'sigmoidal'",
                                                        sliderInput(inputId = "sun_angle_x0",
                                                                    label = "Define parameter x0",
                                                                     min = -60,
                                                                     max = 60,
                                                                     value = 0,
                                                                     step = 0.1),
                                                        sliderInput(inputId = "sun_angle_L",
                                                                    label = "Define parameter L",
                                                                    min = -600,
                                                                    max = 600,
                                                                    value = 100,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "sun_angle_K",
                                                                    label = "Define parameter K",
                                                                    min = -5,
                                                                    max = 5,
                                                                    value = 0.15,
                                                                    step = 0.1)
                                                        # close conditionalPanel(condition = "input.sun_angle_type == 'sigmoidal'"
                                                        ),



                                       ################################################
                                       #### quadratic option

                                       conditionalPanel(condition = "input.sun_angle_type == 'quadratic'",
                                                        sliderInput(inputId = "sun_angle_a",
                                                                    label = "Define parameter a",
                                                                    min = -0.5,
                                                                    max = 0.5,
                                                                    value = -0.1),
                                                        sliderInput(inputId = "sun_angle_b",
                                                                    label = "Define parameter b",
                                                                    min = -1,
                                                                    max = 1,
                                                                    value = 1,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "sun_angle_h",
                                                                    label = "Define parameter h",
                                                                    min = -60,
                                                                    max = 60,
                                                                    value = 0,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "sun_angle_k",
                                                                    label = "Define parameter k",
                                                                    min = -250,
                                                                    max = 250,
                                                                    value = 50,
                                                                    step = 1)
                                                        # close conditionalPanel(condition = "input.sun_angle_type == 'sigmoidal'"
                                       )

                                       # close box for specifying the function and its parameters:
                                      ),

                                   # Create a box with visualises the sun angle smooth:
                                   box(title = "Visualisation of the sun angle smooth function",
                                       width = 9,
                                       plotOutput("sun_angle_smooth")
                                       # close box which visualises the sun angle smooth:
                                       )
                                   # close fluidRow()
                                   )
                                 # close conditionalPanel(condition = "input.covariates .includes('sun_angle')"
                                 ),



                ################################################
                #### lunar_phase

                conditionalPanel(condition = "input.covariates .includes('lunar_phase')",
                                 # create a new row
                                 fluidRow(

                                   # create a box for specifying the function and its parameters:
                                   box(title = "Define the smooth function and its parameters for lunar phase",
                                       width = 3,

                                       radioButtons(inputId = "lunar_phase_type",
                                                    label = "Choose the type of smooth function",
                                                    choices = c("quadratic", "sigmoidal"),
                                                    selected = "quadratic",
                                                    inline = T),



                                       ################################################
                                       #### quadratic option (default)

                                       conditionalPanel(condition = "input.lunar_phase_type == 'quadratic'",
                                                        sliderInput(inputId = "lunar_phase_a",
                                                                    label = "Define parameter a",
                                                                    min = -30,
                                                                    max = 30,
                                                                    value = -5),
                                                        sliderInput(inputId = "lunar_phase_b",
                                                                    label = "Define parameter b",
                                                                    min = -1.5,
                                                                    max = 1.5,
                                                                    value = 1,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "lunar_phase_h",
                                                                    label = "Define parameter h",
                                                                    min = 0,
                                                                    max = 2*pi,
                                                                    value = pi,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "lunar_phase_k",
                                                                    label = "Define parameter k",
                                                                    min = -250,
                                                                    max = 250,
                                                                    value = 25,
                                                                    step = 0.1)
                                                        # close conditionalPanel(condition = "input.lunar_phase_type == 'quadratic'"
                                       ),



                                       ################################################
                                       #### sigmoidal option (default)

                                       conditionalPanel(condition = "input.lunar_phase_type == 'sigmoidal'",
                                                        sliderInput(inputId = "lunar_phase_x0",
                                                                    label = "Define parameter x0",
                                                                    min = -60,
                                                                    max = 60,
                                                                    value = 0,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "lunar_phase_L",
                                                                    label = "Define parameter L",
                                                                    min = -600,
                                                                    max = 600,
                                                                    value = 100,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "lunar_phase_K",
                                                                    label = "Define parameter K",
                                                                    min = -5,
                                                                    max = 5,
                                                                    value = 0.15,
                                                                    step = 0.1)
                                                        # close conditionalPanel(condition = "input.lunar_phase_type == 'sigmoidal'"
                                       )

                                       # close box for specifying the function and its parameters:
                                   ),

                                   # Create a box with visualises the sun angle smooth:
                                   box(title = "Visualisation of the lunar phase smooth function",
                                       width = 9,
                                       plotOutput("lunar_phase_smooth")
                                       # close box which visualises the sun angle smooth:
                                   )
                                   # close fluidRow()
                                 )
                                 # close conditionalPanel(condition = "input.covariates .includes('lunar_phase')"
                ),



                ################################################
                #### julian_day

                conditionalPanel(condition = "input.covariates .includes('julian_day')",
                                 # create a new row
                                 fluidRow(

                                   # create a box for specifying the function and its parameters:
                                   box(title = "Define the smooth function and its parameters for Julian day",
                                       width = 3,

                                       radioButtons(inputId = "julian_day_type",
                                                    label = "Choose the type of smooth function",
                                                    choices = c("quadratic", "sigmoidal"),
                                                    selected = "quadratic",
                                                    inline = T),



                                       ################################################
                                       #### quadratic option (default)

                                       conditionalPanel(condition = "input.julian_day_type == 'quadratic'",
                                                        sliderInput(inputId = "julian_day_a",
                                                                    label = "Define parameter a",
                                                                    min = -1,
                                                                    max = 1,
                                                                    step = 0.001,
                                                                    value = -0.001),
                                                        sliderInput(inputId = "julian_day_b",
                                                                    label = "Define parameter b",
                                                                    min = -10,
                                                                    max = 10,
                                                                    value = 1,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "julian_day_h",
                                                                    label = "Define parameter h",
                                                                    value = stats::median(1:365),
                                                                    min = -400,
                                                                    max = 400,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "julian_day_k",
                                                                    label = "Define parameter k",
                                                                    min = -250,
                                                                    max = 250,
                                                                    value = 15,
                                                                    step = 0.1)
                                                        # close conditionalPanel(condition = "input.julian_day_type == 'quadratic'"
                                       ),



                                       ################################################
                                       #### sigmoidal option (default)

                                       conditionalPanel(condition = "input.julian_day_type == 'sigmoidal'",
                                                        sliderInput(inputId = "julian_day_x0",
                                                                    label = "Define parameter x0",
                                                                    min = -60,
                                                                    max = 60,
                                                                    value = 0,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "julian_day_L",
                                                                    label = "Define parameter L",
                                                                    min = -600,
                                                                    max = 600,
                                                                    value = 100,
                                                                    step = 0.1),
                                                        sliderInput(inputId = "julian_day_K",
                                                                    label = "Define parameter K",
                                                                    min = -5,
                                                                    max = 5,
                                                                    value = 0.15,
                                                                    step = 0.1)
                                                        # close conditionalPanel(condition = "input.julian_day_type == 'sigmoidal'"
                                       )

                                       # close box for specifying the function and its parameters:
                                   ),

                                   # Create a box with visualises the sun angle smooth:
                                   box(title = "Visualisation of the Julian day smooth function",
                                       width = 9,
                                       plotOutput("julian_day_smooth")
                                       # close box which visualises the sun angle smooth:
                                   )
                                   # close fluidRow()
                                 )
                                 # close conditionalPanel(condition = "input.covariates .includes('julian_day')"
                )

                # close the tabItem for Third tab: Parameterisations
                ),



        ################################################
        ################################################
        #### Simulate Depth ("dat_depth")

        # Define additional details required to simulate depth s
        tabItem(tabName = "dat_depth",
                h1("Simulate Depths"),
                h4("Define additional parameters and/or options required to simulate depths."),
                # Open box
                box(title = "Select additional options to simulate depths",
                    width = 2,
                    radioButtons(inputId = "distribution",
                                 label = "Select the likelihood distribution.",
                                 choices = c("gaussian"),
                                 selected = "gaussian",
                                 inline = T),
                    radioButtons(inputId = "link",
                                 label = "Select a link function.",
                                 choices = c("identity"),
                                 selected = "identity",
                                 inline = T),
                    # define distribution parameters
                    conditionalPanel(condition = "input.distribution == 'gaussian'",
                                     numericInput(inputId = "sigma_global",
                                                  label = "Define the sigma parameter of the gaussian distribution.",
                                                  min = 0,
                                                  value = 5)
                                     ),
                    numericInput(inputId = "alpha_global",
                                 label = "Define the model intercept term, alpha.",
                                 value = 100),

                    conditionalPanel(condition = "input.n_individuals > 1",
                                     numericInput(inputId = "alpha_sigma_random",
                                                  label = "Define the standard deviation in intercept among individuals i.e. the random intercept term.",
                                                  value = 0
                                                  )
                                     ),
                    sliderInput(inputId = "rho",
                                label = "Define the serial autocorrelation (rho) parameter.",
                                min = 0,
                                max = 0.99999,
                                value = 0)

                    # close box
                    ),

                # Open new box to view the simulated time-series
                box(title = "Examine the full dataframe that you have assembled",
                width = 10,
                DT::dataTableOutput("rendered_dat_full"))

        # close tabItem() dat_depth
        ),



        ################################################
        ################################################
        #### Thin Depth time-series ("dat_thin")

        tabItem(tabName = "dat_thin",
                h1("Thin Depth Time-series"),
                h4("Choose whether or not you would like to thin the depth time-series and, if so, your method of choice and by how much."),
                # open box:
                box(title = "Options for thinning the depth time-series",
                    width = 2,
                    radioButtons(inputId = "thin_yn",
                                 label = "Would you like to thin the depth time-series?",
                                 choices = c("Yes", "No"),
                                 selected = "No",
                                 inline = T),
                    conditionalPanel(condition = "input.thin_yn == 'Yes'",
                                     # select the method by which you would like to thin the time-series:
                                     radioButtons(inputId = "thin_method",
                                                  label = "Select the method used to thin the time-series",
                                                  choiceValues = c("swa", "sma", "sps"),
                                                  choiceNames = c("Static window average",
                                                                  "Simple moving average",
                                                                  "Systematic point selection")
                                                  ),
                                     # If the input method is swa, display options to define necessary parameters:
                                     conditionalPanel(condition = "input.thin_method == 'swa'",
                                                      # add numeric output
                                                      # where the minimum cannot be lower than input$resolution_minutes (specified previously)
                                                      uiOutput("time_mins_opts")
                                                      ), # close conditionalPanel(condition = "input.thin_method == 'swa'",
                                     # If the input method is sps, , display options to define necessary parameters:
                                     conditionalPanel(condition = "input.thin_method == 'sps'",
                                                    numericInput(inputId = "thin_nth",
                                                                 label = "Selected every nth value",
                                                                 min = 2,
                                                                 step = 1,
                                                                 value = 10)
                                                    ), # close conditionalPanel(condition = "input.thin_method == 'sps'",,
                                     # If the input method is sma, display options to define necessary parameters:
                                     conditionalPanel(condition = "input.thin_method == 'sma'",
                                                      p("You have selected to calculate state-space simple moving averages. This may take a few minutes...")
                                                      # No necessary parameters to set at this stage
                                     ) # close conditionalPanel(condition = "input.thin_method == 'sma'",
                    ) # close conditionalPanel(condition = "input.thin_yn == 'Yes'"

                # close box
                ),

                # Open new box to view the simulated time-series
                conditionalPanel(condition = "input.thin_yn == 'Yes'",
                                 box(title = "Examine the full dataframe that you have assembled",
                                     width = 10,
                                     DT::dataTableOutput("rendered_dat_thin"))
                                 # close conditionalPanel(condition = "input.thin_yn == 'Yes'"
                                 )

          # close tabItem
        ),



        ################################################
        ################################################
        #### Visualise Simulated Data ("vis_raw")

        tabItem(tabName = "vis_raw",
                # Define title:
                h1("Visualise Simulated Data"),



                ################################################
                ################################################
                #### Histograms

                fluidRow(

                h3("Histograms"),

                # A) Barplot of the frequency of each sex
                conditionalPanel(condition = "input.covariates .includes('sex')",
                                 box(title = "Counts of simulated males and females",
                                     width = 6,
                                     plotOutput("sex_barplot")
                                     ) # close box(title = "Counts of simulated males and females"
                ), # close conditionalPanel(condition = "input.covariates .includes('sex')",

                # B) Histogram of simulated depths
                box(title = "The distribution of simulated depths",
                    width = 6,
                    plotOutput("depth_hist")
                    )

                ), # close fluidRow for "Histograms"



                ################################################
                ################################################
                #### Relationships among variables

                fluidRow(
                  h3("Relationships among variables"),
                  plotOutput("pairs_plot", height = "800px")

                ),



                ################################################
                ################################################
                #### time-series plots: depth and covariates:

                # Define a new row:
                fluidRow(

                # Title of this section:
                h3("Time-series Plots"),



                ################################################
                #### Define the options for visualising time-series

                # Define a box to display the options for visualising simulated time-series:
                box(title = "Select the options for visualising the time-series you have simulated.",
                    width = 6,

                    # Select the individual for which data will be plotted
                    uiOutput("select_an_individual")

                    ) # close box for for selecting options to define depth time-series
                ), # close fluidRow for inputs for visualising time-series



                ################################################
                #### Visualise depth time-series

                # Define a new row
                fluidRow(
                  box(title = "Depth Time-series",
                      width = 12,
                      plotOutput("depth_timeseries")
                  ) # close box for "Depth timeseries"
                ), # close fluidRow for "Depth timeseries"



                ################################################
                #### Visualise sun angle time-series

                # Define new row
                fluidRow(
                  # Define a conditionPanel - only do this if "sun_angle" has been input as a covariate
                  conditionalPanel(condition = "input.covariates .includes('sun_angle')",
                                   box(title = "Sun Angle Time-series",
                                       width = 12,
                                       plotOutput("sun_angle_timeseries")
                                       ) # close box for sun_angle
                                 ) # close conditionalPanel(condition = "input.covariates .includes('sun_angle')
                ), # close fluidRow for sun angle



                ################################################
                #### Visualise lunar_phase time-series

                # Define new row
                fluidRow(
                  # Define a conditionPanel - only do this if "lunar_phase" has been input as a covariate
                  conditionalPanel(condition = "input.covariates .includes('lunar_phase')",
                                   box(title = "Lunar Phase Time-series",
                                       width = 12,
                                       plotOutput("lunar_phase_timeseries")
                                   ) # close box for lunar_phase
                  ) # close conditionalPanel(condition = "input.covariates .includes('lunar_phase')"
                ), # close fluidRow for lunar_phase




                ################################################
                #### Visualise julian_day time-series

                # Define new row
                fluidRow(
                  # Define a conditionPanel - only do this if "julian_day" has been input as a covariate
                  conditionalPanel(condition = "input.covariates .includes('julian_day')",
                                   box(title = "Julian Day Time-series",
                                       width = 12,
                                       plotOutput("julian_day_timeseries")
                                   ) # close box for julian_day
                  ) # close conditionalPanel(condition = "input.covariates .includes('julian_day')"
                ) # close fluidRow for julian_day



        # close tabItem(
        ),



        ################################################
        ################################################
        #### Define model ("model_define")

        tabItem(tabName = "model_define",
                h1("Define the model"),
                h4("You have now simulated a depth time-series dataset. The next step is to define a model for this time-series. At first, it is advised to keep the model similar to the truth (i.e. what you have simulated), to check whether you correctly recover what you expect. Then, you can start to make the data-generating process asssumed by the model different from the simulated data-generating process and explore the consequences. (Remember, in reality, the data-generating process is unknown, so this process will help you understand model performance when the true data-generating process is unknown.)"),

                box(title = "Define the inputs to your model",
                    width = 3,

                    # Define the likelihood distribution for the model
                    radioButtons(inputId = "distribution_model",
                                 label = "Select the likelihood distribution.",
                                 choices = c("gaussian"),
                                 selected = "gaussian",
                                 inline = T),

                    # Define the link function for the model
                    radioButtons(inputId = "link_model",
                                 label = "Select a link function.",
                                 choices = c("identity"),
                                 selected = "identity",
                                 inline = T),

                    # Define the covariates to be included in the model
                    pickerInput(inputId = "covariates_model",
                                label = strong("Select the covariates you wish to include in your model of depth."),
                                choices = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
                                selected = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
                                options = list(`actions-box` = TRUE,
                                                size = 10,
                                                `selected-text-format` = "count > 3"),
                                multiple = TRUE),

                    # Define the basis function for length
                    conditionalPanel(condition = "input.covariates_model .includes('length')",
                                     radioButtons(inputId = "length_bs",
                                                  label = "Choose the basis function for length",
                                                  choices = c("tp", "ts", "cr", "cs"),
                                                  selected = "ts",
                                                  inline = T)
                    ),

                    # Define the basis function for sun_angle:
                    conditionalPanel(condition = "input.covariates_model .includes('sun_angle')",
                                     radioButtons(inputId = "sun_angle_bs",
                                                  label = "Choose the basis function for sun_angle.",
                                                  choices = c("tp", "ts", "cr", "cs"),
                                                  selected = "ts",
                                                  inline = T)
                                     ),

                    #### Define knots
                    # length
                    conditionalPanel(condition = "input.covariates_model .includes('length')",
                                     numericInput(inputId = "length_nknots",
                                                  label = "Define the basis dimension for s(length).",
                                                  value = 3,
                                                  step = 1,
                                                  min = 3
                                                  )
                    ),
                    # sun angle
                    conditionalPanel(condition = "input.covariates_model .includes('sun_angle')",
                                     numericInput(inputId = "sun_angle_nknots",
                                                  label = "Define the basis dimension for s(sun_angle).",
                                                  value = 10,
                                                  step = 1,
                                                  min = 3
                                     )
                    ),
                    # lunar phase
                    conditionalPanel(condition = "input.covariates_model .includes('lunar_phase')",
                                     numericInput(inputId = "lunar_phase_nknots",
                                                  label = "Define the basis dimension for s(lunar_phase).",
                                                  value = 10,
                                                  step = 1,
                                                  min = 3
                                     )
                    ),
                    # julian day
                    uiOutput("julian_day_nknots"),

                    # Choose whether you would like to include individual as a random effect
                    radioButtons(inputId = "random_intercept_yn",
                                 label = "Choose whether or not you would like to include a random intercept term for individual.",
                                 choices = c("Yes", "No"),
                                 selected = "No",
                                 inline = T),

                    # Define the rho parameter for the model
                    sliderInput(inputId = "rho_model",
                                label = "Define the serial autocorrelation (rho) parameter",
                                min = 0,
                                max = 0.99999,
                                value = 0),

                    # Define the gamma parameter (prevent overfitting)
                    numericInput(inputId = "gamma_parameter",
                                label = strong("Define the value of the gamma parameter (increase gamma above 1 to force smoother fits)"),
                                value = 1,
                                min = 1,
                                max = 2)

                    # close box
                    ),

                box(title = "Examine the model that you have defined",
                    width = 9,
                    # print the model
                    # this includes the formula and other information and shows that the model has fit without errors.
                    verbatimTextOutput("dm"),
                    verbatimTextOutput("dm_start_event"),
                    verbatimTextOutput("knots"),
                    verbatimTextOutput("print_model")
                    # close box
                    )

                # close tablItem (model_define)
                ),



        ################################################
        ################################################
        #### Model Summary ("model_summary")

        tabItem(tabName = "model_summary",
                h1("Model Summary"),
                h3("Examine the summary output of your model."),
                h4("You will have already checked the model family, link function and formula from the previous tab. Here, first examine the estimates for the parametric coefficients. Have they been correctly estimated? How well has the model recovered these? Next, examine the approximate significance of smooth terms, the model deviance and other terms provided. Guidance is given in the Vignette (tab: Introduction) on what you should be looking for here."),

                # Print mean depth
                # verbatimTextOutput("mean_depth"),

                # Print the model summary
                verbatimTextOutput("model_summary")

                # close tabItem(
                ),



        ################################################
        ################################################
        #### Model Covariate Plots ("model_covariates")

        tabItem(tabName = "model_covariates",
                h1("Model Covariate Plots"),
                h4("Examine the simulated and recoved parameters and/or smooth functions relating depth to the covariates you specified should be included in the model. Simulated outcomes are shown in grey. Predicted relationships are shown in black. For covariates that were not included in simulations but were included in models, no simulated effects are included, but you should expect to see no relationship."),

                # Define display options
                box(title = "Display Options",
                    checkboxInput(inputId = "partial_residuals",
                                 label = "Select this options to display partial residuals on 1D smooth plots. This is advisable for small datasets. For large datasets, plots may take some time to appear.",
                                 value = FALSE)
                    # close box
                ),

                # show plots
                # sex:
                conditionalPanel(condition = "input.covariates_model .includes('sex')",
                                 box(plotOutput("sex_preds")
                                     )
                                 ),
                # length
                conditionalPanel(condition = "input.covariates_model .includes('length')",
                                 box(plotOutput("length_smooth_preds")
                                     )
                                 ),
                # sun_angle:
                conditionalPanel(condition = "input.covariates_model .includes('sun_angle')",
                                 box(plotOutput("sun_angle_smooth_preds")
                                   )
                                 ),
                # lunar_phase:
                conditionalPanel(condition = "input.covariates_model .includes('lunar_phase')",
                                 box(plotOutput("lunar_phase_smooth_preds")
                                     )
                                 ),
                # julian_day:
                conditionalPanel(condition = "input.covariates_model .includes('julian_day')",
                                 box(plotOutput("julian_day_smooth_preds")
                                     )
                                 )

          # close tabItem(
          ),


        ################################################
        ################################################
        #### Model Predictions ("model_preds")

        tabItem(tabName = "model_preds",
                # Define title:
                h1("Model Predictions"),


                ################################################
                #### Inputs

                # Defin a box for model inputs
                box(title = "Define inputs... ",
                    width = 3,
                    uiOutput("select_an_individual_preds")
                ),


                ################################################
                #### View Predictions

                # Define a box to visualise model predictions
                box(title = "Visualise model predictions",
                    width = 9,
                    plotOutput("depth_timeseries_preds")
                ) # close box(title = "Visualise model predictions"


        ), #  tabItem(tabName = "model_preds"



        ################################################
        ################################################
        #### Model Diagnostics ("model_diag")

        tabItem(tabName = "model_diag",
                h1("Model Diagnostics"),


                ################################################
                #### A) gam.check()

                fluidRow(
                  # title
                  h2("gam.check()"),
                  # output of gam_check:
                  verbatimTextOutput("gam_check")
                  ), # close fluidRow for gam_check


                ################################################
                #### B) Residual Plots

                fluidRow(
                  # title
                  h2("Residuals Diagnostic Plots"),
                  box(title = "Define options for residual plots",

                      # Define whether or not you would like to plot plots for all individuals:
                      checkboxInput(inputId = "residuals4id",
                                    label = "Residual plots are plotted by default for all individuals. Would you like to examine the plot for a specific individual?",
                                    value = FALSE),
                      # If the individual has selected to plot residual plots for only a specific individual...
                      conditionalPanel(condition = "input.residuals4id == true",
                                       uiOutput("select_an_individual_resids")
                                       ), # close conditionalPanel(condition = "input.residuals4id == true"
                      # Define whether or not you would like to plot a random subset of residuals:
                      checkboxInput(inputId = "yes_rand_pc",
                                    label = "Would you like to plot a random subset of residuals? This can make plots more intepretable if datasets are large.",
                                    value = FALSE),
                      # If the individual has selected to plot a random subset of individuals...
                      conditionalPanel(condition = "input.yes_rand_pc == true",
                                       # Define a slider input to choose the % of residuals to be randomly selected:
                                       sliderInput(inputId = "rand_pc",
                                                   label = "Define the percentage (%) of residuals to be randomly selected.",
                                                   min = 1,
                                                   max = 99,
                                                   value = 50
                                                   )
                                       ) # close conditionalPanel(condition = "input.yes_rand_pc == true",
                      ) # close box(title = "Define options for residual plots",
                ), # close fluidRow,

                fluidRow(
                  box(width = 12,
                      plotOutput("cresid_plots", width = "100%", height = "3500px")
                    )

                ) # close fluidROw

           # close tabItem(
           )



      # close tabItems()
      )
    # close dashBoardBody
    )

  # close dashboardPage()
  )



################################################
################################################
#### Server

server <- function(input, output) {


  ################################################
  ################################################
  #### Dataframe Assembly

  #### Set seed for reproducibility
  reactive(set.seed(input$seed))

  #### Define dataframe using assemble_ts()
  # Define the dataframe as a reactive object using user-inputted inputs
  # This way, it can be called elsewhere in this script using dat().
  # Note that input date must be a character or, for some reason (!), if its
  # ... inputted as.Date(), the calculation of lunar phase is incorrect.
  dat <-
    reactive({
      d <- assemble_ts(start_date = as.character(input$start_date),
                        start_date_variable = FALSE,
                        max_duration_days = input$duration_days,
                        duration_days_variable = FALSE,
                        resolution_minutes = input$resolution_minutes,
                        n_individuals = input$n_individuals,
                        longitude =  input$long_dst,
                        latitude =  input$lat_dst,
                        tz = "UTC",
                        covariates = c("sex", "length", "sun_angle", "lunar_phase", "julian_day"),
                        parameters =
                          list(sex = list(Pf = input$sex_Pf, replace = TRUE),
                               length = list(shape = input$length_density_curve[1],
                                             scale = input$length_density_curve[2],
                                             plot_density_curve = FALSE))
                        )
      d$sex <- (d$sex=="F")+0
      d$sex <- factor(d$sex)
      flag <- flag_ts(x = d$timestamp, fct = d$individual, duration_threshold = 9e999, flag = 1:3)
      d$start_event <- flag$flag1
      d$start_event_id <- flag$flag2
      return(d)
    })

  #### Create a rendered version of this dataframe for display
  # ... on the Dataframe Assembly tab
  # Note the () after dat, because it is a reactive object.
  output$rendered_dat <- DT::renderDataTable(DT::datatable(dat(), options = list(pageLength = 20)))

  #### Simulated depths
  output$length_density_curve <-
    renderPlot({
      pp <- graphics::par(oma = c(2, 3, 2, 2))
      prettyGraphics::pretty_curve(x = dat()$length,
                                   f = stats::dgamma,
                                   x_interp = FALSE, type = "p",
                                   xlab = "", ylab = "", main = "",
                                   param = list(shape = input$length_density_curve[1],
                                                scale = input$length_density_curve[2]),
                                   pretty_axis_args = list(side = 1:2, pretty = list(n = 5),
                                                           axis = list(cex.axis = cex.axis, las = TRUE)),
                                   mtext_args = list(list(side = 1, text = "Length (cm)", line = 2.5, cex = cex.lab),
                                                     list(side = 2, text = "Density", line = 5, cex = cex.lab)
                                   )
      )
      graphics::par(pp)

   })

 ################################################
 ################################################
 #### Parameterisations



 ################################################
 #### sex

 # Create a plot for sex using the sex.contrast function
 # which shows the extent of the contrast inputted by the user in the user interfacer
 output$sex_contrast <-
    renderPlot(
      parameterise_contrast_2l(x = factor(c(0, 1)),
                               param = input$sex_contrast,
                               plot_gam = FALSE,
                               cex.axis = cex.axis,
                               mtext_args = list(list(side = 1, text = "Sex (0, female; 1, male)", line = 2.5, cex = cex.lab),
                                                 list(side = 2, text = "Contrast (m)", line = 2.5, cex = cex.lab)
                               )
      )
    )



 ################################################
 #### length

  length_smooth <- reactive({
    if("length" %in% input$covariates){
      length_smooth <- parameterise_smooth(x = dat()$length,
                                              f = linear,
                                              param = list(a = 0, b = input$length_beta),
                                              plot = FALSE
      )
      return(length_smooth)
    }
  })

 output$length_smooth <-
    renderPlot({
      parameterise_smooth(x = seq(min(dat()$length), max(dat()$length), length.out = 100),
                          f = length_smooth()$f,
                          param = length_smooth()$param,
                          plot = TRUE,
                          pretty_axis_args = list(side = c(1, 2),
                                                  pretty = list(n = 5),
                                                  axis = list(cex.axis = cex.axis, las = TRUE)),
                          add_rug = TRUE,
                          term = "length",
                          dat = dat(),
                          mtext_args = list(list(side = 1, text = "Length (cm)", line = 2.5, cex = cex.lab),
                                            list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
                          )
    })


 ################################################
 #### sun angle smooth

  sun_angle_smooth <- reactive({
    if("sun_angle" %in% input$covariates){
           if(input$sun_angle_type == "sigmoidal"){
             sun_angle_pars <- list("x0" = input$sun_angle_x0, "L" = input$sun_angle_L, "k" = input$sun_angle_K)
             fsa <- sigmoid
           } else if(input$sun_angle_type == "quadratic"){
             sun_angle_pars <- list("a" = input$sun_angle_a, "b" = input$sun_angle_b, "h" = input$sun_angle_h, "k" = input$sun_angle_k)
             fsa <- quadratic
           }
      sun_angle_smooth <- parameterise_smooth(x = dat()$sun_angle,
                                              f = fsa,
                                              param = sun_angle_pars,
                                              plot = FALSE
                                              )
      return(sun_angle_smooth)
    }
  })

  output$sun_angle_smooth <-
    renderPlot(
      if("sun_angle" %in% input$covariates){
        parameterise_smooth(x = seq(-60, 60, length.out = 100),
                            f = sun_angle_smooth()$f,
                            param = sun_angle_smooth()$param,
                            plot = TRUE,
                            pretty_axis_args = list(side = c(1, 2), pretty = list(n = 5), axis = list(cex.axis = cex.axis, las = TRUE)),
                            add_rug = TRUE,
                            term = "sun_angle",
                            dat = dat(),
                            mtext_args = list(list(side = 1,
                                                   text = expression(paste("Sun Angle (", degree, ")")),
                                                   line = 2.5, cex = cex.lab),
                                              list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
        )
      }

    )



 ################################################
 #### lunar phase smooth

  lunar_phase_smooth <- reactive({
    if("sun_angle" %in% input$covariates){
      if(input$lunar_phase_type == "sigmoidal"){
        lunar_phase_pars <- list("x0" = input$lunar_phase_x0, "L" = input$lunar_phase_L, "k" = input$lunar_phase_K)
        fsa <- sigmoid
      } else if(input$lunar_phase_type == "quadratic"){
        lunar_phase_pars <- list("a" = input$lunar_phase_a,
                                 "b" = input$lunar_phase_b,
                                 "h" = input$lunar_phase_h,
                                 "k" = input$lunar_phase_k)
        fsa <- quadratic
      }
      lunar_phase_smooth <- parameterise_smooth(x = dat()$lunar_phase,
                                              f = fsa,
                                              param = lunar_phase_pars,
                                              plot = FALSE
      )
      return(lunar_phase_smooth)
    }
  })

  output$lunar_phase_smooth <-
    renderPlot({
      if("lunar_phase" %in% input$covariates){
        parameterise_smooth(x = seq(0, 2*pi, length.out = 100),
                            f = lunar_phase_smooth()$f,
                            param = lunar_phase_smooth()$param,
                            plot = TRUE,
                            pretty_axis_args = list(side = c(1, 2),
                                                    lim = list(x = dat_ts_axis$lunar_phase$lim, y = NULL),
                                                    pretty = list(n = 5),
                                                    axis =
                                                      list(
                                                        list(at = dat_ts_axis$lunar_phase$axis$at,
                                                             labels = dat_ts_axis$lunar_phase$axis$labels,
                                                             cex.axis = cex.axis
                                                             ),
                                                        list(las = TRUE, cex.axis = cex.axis)
                                                        )
                                                    ),
                            add_rug = TRUE,
                            term = "lunar_phase",
                            add_moons = TRUE,
                            add_moons_args = list(radius1 = 0.05),
                            dat = dat(),
                            mtext_args = list(list(side = 1, text = "Lunar Phase (rad)", line = 2.5, cex = cex.lab),
                                              list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
        )
      }
    })



 ################################################
 #### Julian day smooth

  julian_day_smooth <- reactive({
    if("sun_angle" %in% input$covariates){
      if(input$julian_day_type == "sigmoidal"){
        julian_day_pars <- list("x0" = input$julian_day_x0, "L" = input$julian_day_L, "k" = input$julian_day_K)
        fsa <- sigmoid
      } else if(input$julian_day_type == "quadratic"){
        julian_day_pars <- list("a" = input$julian_day_a,
                                 "b" = input$julian_day_b,
                                 "h" = input$julian_day_h,
                                 "k" = input$julian_day_k)
        fsa <- quadratic
      }
      julian_day_smooth <- parameterise_smooth(x = dat()$julian_day,
                                                f = fsa,
                                                param = julian_day_pars,
                                                plot = FALSE
      )
      return(julian_day_smooth)
    }
  })

  output$julian_day_smooth <-
    renderPlot({
      if("julian_day" %in% input$covariates){
        parameterise_smooth(x = seq(0, 366, length.out = 100),
                            f = julian_day_smooth()$f,
                            param = julian_day_smooth()$param,
                            plot = TRUE,
                            pretty_axis_args = list(side = c(1, 2),
                                                    lim = list(x = dat_ts_axis$julian_day$lim, y = NULL),
                                                    pretty = list(n = 5),
                                                    axis =
                                                      list(
                                                        list(at = dat_ts_axis$julian_day$axis$at,
                                                             labels = dat_ts_axis$julian_day$axis$labels,
                                                             cex.axis = cex.axis
                                                        ),
                                                        list(las = TRUE, cex.axis = cex.axis)
                                                      )
                            ),
                            add_rug = TRUE,
                            term = "julian_day",
                            dat = dat(),
                            mtext_args = list(list(side = 1, text = "Time (Months [Julian day])", line = 2.5, cex = cex.lab),
                                              list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
        )
      }
    })



 ################################################
 ################################################
 #### Simulate Depths



 ################################################
 #### Define reactive inputs required to simulate depth
 # ... (This simplifies the inputs to the dat.depth() function)

 #### Define compute_lp
 compute_lp <- reactive({
   #### Define a blank list
   lp_ls <- list()
   #### sex
   # Add the list of parameters for sex
   if("sex" %in% input$covariates){
     lp_ls <- append(lp_ls, list(sex = list(f = linear,
                                           param = list(a = 0, b = input$sex_contrast))
                                           ))}
   #### length
   if("length" %in% input$covariates){
     lp_ls <- append(lp_ls, list(length = list(f = length_smooth()$f,
                                               param = length_smooth()$param)
                                 ))}
   #### sun angle
   if("sun_angle" %in% input$covariates){
     lp_ls <- append(lp_ls, list(sun_angle = list(f = sun_angle_smooth()$f,
                                            param = sun_angle_smooth()$param)
     ))}

   #### lunar phase
   if("lunar_phase" %in% input$covariates){
     lp_ls <- append(lp_ls, list(lunar_phase = list(f = lunar_phase_smooth()$f,
                                              param = lunar_phase_smooth()$param)
                           ))}
   #### julian day
   if("julian_day" %in% input$covariates){
     lp_ls <- append(lp_ls, list(julian_day = list(f = julian_day_smooth()$f,
                                              param = julian_day_smooth()$param)
                           ))}
   # return the list
   return(lp_ls)
   # close reactive expression
   })

  #### Define sim_obs
  sim_obs <- reactive({
    if(input$rho == 0){
      return(function(lpi){ stats::rnorm(length(lpi), mean = lpi, sd = input$sigma_global) })
    } else{
      return(function(lpi) {lpi + stats::arima.sim(list(order = c(1, 0, 0), ar =
                                                          input$rho),
                                                   n = length(lpi),
                                                   sd = sigma_arima(input$rho, input$sigma_global))
        })
    }
  })

 ################################################
 #### Simulate depths using reactive inputs
 # Note the use of () where objects have been defined reactively

 # Define depth_sims as a reactive() object
 depth_sims <-
   reactive(
     sim_ts(alpha = input$alpha_global,
                  alpha_sigma_random = input$alpha_sigma_random,
                  compute_lp = compute_lp(),
                  sim_obs = sim_obs(),
                  dat = dat(),
                  fct = "individual",
                  seed = NULL
                  )
             )

 # create a new dataframe, that also contains depths
 dat_full <- reactive(cbind(dat(), depth = depth_sims()))

 # view the full dataframe; remember dat_full needs () because it is a reactive object
 output$rendered_dat_full <- DT::renderDataTable(dat_full())



 ################################################
 ################################################
 #### Thin Depth time-series

 # Define a reactive UI option for thinning the dataframe,
 # whereby the minimum value by which the dataframe can be thinned
 # must be greater than input$resolution_minutes (preventing careless errors)
 # this reactive UI is passed back to the UI with the uiOutput() function, above.
 output$time_mins_opts <- renderUI(
   numericInput(inputId = "time_mins",
                label = "Define the time, in minutes, between observations after thinning.",
                min = input$resolution_minutes,
                # By default, the value selected will be twice the input value:
                value = input$resolution_minutes * 2
                ))

 # Create a reactive, thinned dataframe, using the options displayed
 dat_thin_parameters <- reactive({

   # if method is swa...
   if(input$thin_method == "swa"){
     pl <- list(
       swa = list(time_mins = input$time_mins,
                   latitude = 56.41041,
                   longitude = 5.469723,
                   # define the covariates you want to include in the thinned dataframe
                   # (the function adds these back in as appropriate)
                   covariates = c("sex",
                                  "length",
                                  "sun_angle",
                                  "lunar_phase",
                                  "julian_day"),
                  # set independence_threshold at INF for now since gaps cannot be introduced for individuals
                  independence_threshold = 9e999)
     )
     return(pl)

   # close if(input$thin_method == "swa"){ and move on to next possibilty:
   } else if(input$thin_method == "sps"){
     pl <- list(sps = list(first = 1, nth = input$thin_nth))
     return(pl)
     } # close else if(input$thin_method == "sps"){ and move on to next possibility

   # sma
   # no parameters need to be defined (in current version)

 }) # close reactive function

 # Thin the dataframe:
 dat_thin <- reactive({
  if(input$thin_yn == "Yes"){
    gams4dts_thin_ts(dat = dat_full(),
                     id_column = "individual",
                     timestamp_column = "timestamp",
                     start_event_id_column = "start_event_id",
                     start_event_logic_column = "start_event",
                     method = input$thin_method,
                     parameters = dat_thin_parameters()
                     )
    } # close if(input$thin_yn == "Yes"){
 }) # close the reactive function


 # Create a rendered DataTable of dat_thin for display
 # Note the () after dat_thin because it is a reactive object.
 output$rendered_dat_thin <- DT::renderDataTable(dat_thin())

 # Create a new reactive dataframe: dat_model, that will be used for subsequent
 # models and plots of models.
 dat_model <- reactive({
   # If the user thats selected to thin the dataframe, then the thinned dataframe
   # defines dat_model
   if(input$thin_yn == "Yes"){
     return(dat_thin())
   # else if there is no thinned dataframe, then dat_model is defined by dat_full()
   } else{
     return(dat_full())
     }
 })



 ################################################
 ################################################
 #### Visualisations of the assembled dataframe



 ################################################
 #### Histograms

 # counts of sex:
 output$sex_barplot <-
   renderPlot(
     barplot_ts_sex(dat = dat_model(),
                    id_column = "individual",
                    f_column = "sex",
                    cex.axis = cex.axis,
                    cex = cex.lab)
   )

 # histogram of depths:
 output$depth_hist <- renderPlot(
   prettyGraphics::pretty_hist(x = dat_model()$depth,
                               freq = TRUE,
                               xaxis = list(cex.axis = cex.axis),
                               yaxis = list(las = TRUE, cex.axis = cex.axis),
                               xlab = "", ylab = "",
                               mtext_args = list(list(side = 1, text = "Depth (m)", line = 2.5, cex = cex.lab),
                                                 list(side = 2, text = "Frequency", line = 2.5, cex = cex.lab))
   )
   )



 ################################################
 #### Pairs plot

 # Define a pairs plot for looking at correlations between covariates
 output$pairs_plot <- renderPlot(graphics::pairs(dat_model()[, c("individual", input$covariates, "depth")],
                                                 upper.panel = NULL, cex.axis = 1.7))



 ################################################
 #### Time-series Plots

 #### Define reactive user interface options
 # Define a drop down menu for one of the simulated individuals to be selected:
 output$select_an_individual <-
   renderUI(selectInput(inputId = "selected_individual",
                        label = strong("Select an individual"), # strong() makes the label bold
                        choices = c(1:input$n_individuals),
                        selected = 1)
            )

 #### Plot depth_timeseries
 output$depth_timeseries <-
   renderPlot(
     prettyGraphics::pretty_ts(x = dat_model()$timestamp,
             y1 = dat_model()$depth*-1,
             fct = dat_model()$individual,
             fct_level = input$selected_individual,
             pretty_axis_args = list(side = c(3, 2),
                                     pretty = list(n = 5),
                                     axis = list(las = TRUE, cex.axis = cex.axis)
                                     ),
             mtext_args = list(list(side = 3, text = "Time", line = 2.5, cex = cex.lab),
                               list(side = 2, text = "Depth (m)", line = 2.5, cex = cex.lab)
                               )
             )
   )

 #### Sun angle time-series
 output$sun_angle_timeseries <-
   renderPlot(
     prettyGraphics::pretty_ts(x = dat_model()$timestamp,
             y1 = dat_model()$sun_angle,
             fct = dat_model()$individual,
             fct_level = input$selected_individual,
             pretty_axis_args = list(side = c(1, 2),
                                     pretty = list(n = 5),
                                     axis = list(las = TRUE, cex.axis = cex.axis)
             ),
             mtext_args = list(list(side = 1, text = "Time", line = 2.5, cex = cex.lab),
                               list(side = 2, text = "Sun Angle (degrees)", line = 2.5, cex = cex.lab)
             )
     )
   )

 #### Lunar phase
 output$lunar_phase_timeseries <-
   renderPlot(
     prettyGraphics::pretty_ts(x = dat_model()$timestamp,
             y1 = dat_model()$lunar_phase,
             fct = dat_model()$individual,
             fct_level = input$selected_individual,
             pretty_axis_args = list(side = c(1, 2),
                                     lim = list(NULL, dat_ts_axis$lunar_phase$lim),
                                     pretty = list(n = 5),
                                     axis = list(
                                       list(cex.axis = cex.axis),
                                       list(at = dat_ts_axis$lunar_phase$axis$at,
                                            labels = dat_ts_axis$lunar_phase$axis$labels,
                                            las = TRUE,
                                            cex.axis = cex.axis)
                                     )
             ),
             mtext_args = list(list(side = 1, text = "Time", line = 2.5, cex = cex.lab),
                               list(side = 2, text = "Lunar Phase (rad)", line = 2.5, cex = cex.lab)
             ),
             add_moons_args = list(side = 2, radius1 = 5000)
     )
   )

 #### julian_day
 output$julian_day_timeseries <-
   renderPlot(
     prettyGraphics::pretty_ts(x = dat_model()$timestamp,
             y1 = dat_model()$julian_day,
             fct = dat_model()$individual,
             fct_level = input$selected_individual,
             pretty_axis_args = list(side = c(1, 2),
                                     lim = list(NULL, dat_ts_axis$julian_day$lim),
                                     pretty = list(n = 5),
                                     axis = list(cex.axis = cex.axis, las = TRUE)
             ),
             mtext_args = list(list(side = 1, text = "Time", line = 2.5, cex = cex.lab),
                               list(side = 2, text = "Time (Months [Julian Day])", line = 2.5, cex = cex.lab)
             )
     )
   )




 ################################################
 ################################################
 #### Define Model

 #### Knots reactive interface
 # Define k interactively for Julian day because the default (k = 10)
 # ... will often be more than the number of unique data points for Julian day.
 output$julian_day_nknots <- renderUI({
   if("julian_day" %in% input$covariates_model){
     numericInput(inputId = "julian_day_nknots",
                  label = "Define the basis dimension for s(julian_day).",
                  value = min(c(5, length(unique(dat_model()$julian_day)))),
                  step = 1,
                  min = 3,
                  max = length(unique(dat_model()$julian_day))
     )
   }
 })


 # Define a vector of fixed covariates to be included in the model, as specified by the user:
 #fixed_covariates_bam <- reactive(
 # c("sex", "length")[which(c("sex", "length") %in% input$covariates_model)]
 # )
 fixed_covariates_bam <- reactive({
   if("sex" %in% input$covariates_model){
     return("sex")
   }
 })

 # Define a vector of smooth covariates to be included in the model, as specified by the user
 smooth_covariates_model <- reactive(
   c("length", "sun_angle", "lunar_phase", "julian_day")[which(c("length",
                                                                 "sun_angle",
                                                                 "lunar_phase",
                                                                 "julian_day") %in% input$covariates_model)]
   )

 # Adjust the list of smooth covariates for the bam function,
 # ... which needs to include individual as a smooth function if this has been specified:
 smooth_covariates_bam <- reactive({
   # If a random intercept term has been specified...
    if(input$random_intercept_yn == "Yes"){
      # then add "individual" to the vector of smooth covariates
      c(smooth_covariates_model(), "individual")
    } else{
      # otherwise, just leave them as they are.
        smooth_covariates_model()}
   })

 # Define smooth_covariates list as required for gams4dts_define_bam_formula
 l <- reactive({
   l <- list()
   if("length" %in% smooth_covariates_bam()){
     l[["length"]] = list(bs = input$length_bs, k = input$length_nknots)
   }
   if("sun_angle" %in% smooth_covariates_bam()){
     l[["sun_angle"]] = list(bs = input$sun_angle_bs, k = input$sun_angle_nknots)
     }
   if("lunar_phase" %in% smooth_covariates_bam()){
     l[["lunar_phase"]] = list(bs = "cc", k = input$lunar_phase_nknots)
      }
   if("julian_day" %in% smooth_covariates_bam()){
     l[["julian_day"]] = list(bs = "cc", k = input$julian_day_nknots)
     }
   if("individual" %in% smooth_covariates_bam()){
     l[["individual"]] = list(bs = "re", k = input$n_individuals)
     }
   return(l)
  })

 # Define a model formula as a reactive object using the gams4dts_define_bam_formula function
 # ... and reactive inputs defined above
 # Note the () after reactive inputs in the function below
 f <- reactive(
   gams4dts_define_bam_formula(response = "depth",
                      fixed_covariates = sapply(fixed_covariates_bam(), list),
                      smooth_covariates = l(),
                      dat = dat_model())
   # close reactive function
   )

 # Define knots reactively
 kn <- list(lunar_phase = c(0, 2*pi), julian_day = c(0, 365))
 knots <- reactive({
   if("lunar_phase" %in% input$covariates_model & "julian_day" %in% input$covariates_model){
     kn
   } else if("lunar_phase" %in% input$covariates_model & !("julian_day" %in% input$covariates_model)){
     kn[1]
   } else if( !("lunar_phase" %in% input$covariates_model) & "julian_day" %in% input$covariates_model){
     kn[2]
   } else{
     NULL
   }
 })

 # Define the model as a reactive object, using the formula above and other inputs
 # NB: Do not define AR.start here;
 # The is dealt with within the custom functions automatically (see gams4dts_define_bam_formula and gams4dts_bam_model())
 m1 <- reactive({
   gams4dts_bam_model(f(),
             likelihood = list(distribution = input$distribution_model,
                               link = input$link_model),
             rho = input$rho_model,
             knots = knots(),
             gamma = input$gamma_parameter,
             dat = dat_model())
   })

 # Create a printed version of the model to display on screen
 # This shows valuable information, e.g. the model formula, to the user
 # ... and helps diagnose errors too (e.g. if this doesn't show up!)
 output$print_model <- renderPrint(print(m1()))

 # Additional error checking prints:
 # output$dm <- renderPrint(head(dat_model()))
 # output$dm_start_event <- renderPrint(head(dat_model()$start_event))
 # output$knots <- renderPrint(knots())



 ################################################
 ################################################
 #### Model Summary

 # Create an output object that is the model summary
 # Note the () after m1 (it is a reactive object)
 output$model_summary <- renderPrint(print(summary(m1())))

 # print the arithmetic mean depth: this is what bam() returns as the model intercept
 # output$mean_depth <- renderPrint(print(mean(dat_model()$depth, na.rm = T)))


 ################################################
 ################################################
 #### Model Covariate Plots



 ################################################
 #### fixed effects: sex.contrast and linear functions

 #### sex:
 output$sex_preds <-
   renderPlot(
     parameterise_contrast_2l(x = factor(c("F", "M")),
                              param = input$sex_contrast,
                              plot_gam = TRUE,
                              dat = dat_model(),
                              model = m1(),
                              term = "sex",
                              cex.axis = cex.axis,
                              mtext_args = list(list(side = 1, text = "Sex (F, female; M, male)", line = 2.5, cex = cex.lab),
                                                list(side = 2, text = "Contrast (m)", line = 2.5, cex = cex.lab)
                                                )
                              )
   )


 ################################################
 #### smooth functions:

 #### create plot.gam object
 plot_gam_ls <- reactive(plot.gam(m1(), residuals = TRUE))

 #### common arguments
 add_residuals_args <- list(cex = 0.5, col = "black")

 #### length
 output$length_smooth_preds <-
   renderPlot(
     parameterise_smooth(x = seq(min(dat()$length), max(dat()$length), length.out = 100),
                         f = length_smooth()$f,
                         param = length_smooth()$param,
                         plot = TRUE,
                         plot_gam = TRUE,
                         dat = dat_model(),
                         model = m1(),
                         term = "length",
                         plot_gam_ls = plot_gam_ls(),
                         residuals = input$partial_residuals,
                         add_residuals_args = add_residuals_args,
                         shift_truth = -mean(length_smooth()$y),
                         shift_predictions = 0,
                         add_rug = TRUE,
                         add_rug_args = list(),
                         add_moons = FALSE,
                         pretty_axis_args = list(side = c(1, 2),
                                                 pretty = list(n = 10),
                                                 axis = list(cex.axis = cex.axis, las = TRUE)
                         ),
                         mtext_args = list(list(side = 1,
                                                text = "Length (cm)",
                                                line = 2.5, cex = cex.lab),
                                           list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
     )
   )

 #### sun_angle
 output$sun_angle_smooth_preds <-
   renderPlot(
     parameterise_smooth(x = seq(-60, 60, length.out = 100),
                         f = sun_angle_smooth()$f,
                         param = sun_angle_smooth()$param,
                         plot = TRUE,
                         plot_gam = TRUE,
                         dat = dat_model(),
                         model = m1(),
                         term = "sun_angle",
                         plot_gam_ls = plot_gam_ls(),
                         residuals = input$partial_residuals,
                         add_residuals_args = add_residuals_args,
                         shift_truth = -mean(sun_angle_smooth()$y),
                         shift_predictions = 0,
                         add_rug = TRUE,
                         add_rug_args = list(),
                         add_moons = FALSE,
                         pretty_axis_args = list(side = c(1, 2),
                                                 pretty = list(n = 10),
                                                 axis = list(cex.axis = cex.axis, las = TRUE)
                                                 ),
                         mtext_args = list(list(side = 1,
                                                text = expression(paste("Sun Angle (", degree, ")")),
                                                line = 2.5, cex = cex.lab),
                                           list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
                         )
   )

 #### lunar_phase
 output$lunar_phase_smooth_preds <-
   renderPlot(
     parameterise_smooth(x = seq(0, 2*pi, length.out = 100),
                         f = lunar_phase_smooth()$f,
                         param = lunar_phase_smooth()$param,,
                         plot = TRUE,
                         plot_gam = TRUE,
                         dat = dat_model(),
                         model = m1(),
                         term = "lunar_phase",
                         plot_gam_ls = plot_gam_ls(),
                         residuals = input$partial_residuals,
                         add_residuals_args = add_residuals_args,
                         shift_truth = -mean(lunar_phase_smooth()$y),
                         shift_predictions = 0,
                         add_rug = TRUE,
                         add_moons = TRUE,
                         add_moons_args = list(radius1 = 0.05),
                         pretty_axis_args = list(side = c(1, 2),
                                                 lim = list(x = dat_ts_axis$lunar_phase$lim, y = NULL),
                                                 pretty = list(n = 5),
                                                 axis =
                                                   list(
                                                     list(at = dat_ts_axis$lunar_phase$axis$at,
                                                          labels = dat_ts_axis$lunar_phase$axis$labels,
                                                          cex.axis = cex.axis
                                                     ),
                                                     list(las = TRUE, cex.axis = cex.axis)
                                                   )
                         ),
                         mtext_args = list(list(side = 1,
                                                text = "Lunar Phase (rad)",
                                                line = 2.5, cex = cex.lab),
                                           list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab))
     )
   )


 #### julian_day
 output$julian_day_smooth_preds <-
   renderPlot(
     parameterise_smooth(x = seq(0, 366, length.out = 100),
                         f = julian_day_smooth()$f,
                         param = julian_day_smooth()$param,
                         plot = TRUE,
                         plot_gam = TRUE,
                         dat = dat_model(),
                         model = m1(),
                         term = "julian_day",
                         plot_gam_ls = plot_gam_ls(),
                         residuals = input$partial_residuals,
                         add_residuals_args = add_residuals_args,
                         shift_truth = -mean(julian_day_smooth()$y),
                         shift_predictions = 0,
                         add_rug = TRUE,
                         add_rug_args = list(),
                         add_moons = FALSE,
                         pretty_axis_args = list(side = c(1, 2),
                                                 lim = list(x = dat_ts_axis$julian_day$lim, y = NULL),
                                                 pretty = list(n = 10),
                                                 axis = list(cex.axis = cex.axis, las = TRUE)
                         ),
                         mtext_args = list(list(side = 1,
                                                text = "Time (months [Julian day])",
                                                line = 2.5, cex = cex.lab),
                                           list(side = 2, text = "Change in depth (m)", line = 2.5, cex = cex.lab)
                         )
     )
   )



 ################################################
 ################################################
 #### Model predictions

 #### Define a drop down menu for one of the simulated individuals to be selected:
 output$select_an_individual_preds <-
   renderUI(selectInput(inputId = "selected_individual_preds",
                        label = strong("Select an individual"), # strong() makes the label bold
                        choices = c(1:input$n_individuals),
                        selected = 1)
   )

 #### Define predictions
 pred <- reactive({
   nd <- dat_model()[which(dat_model()$individual == input$selected_individual_preds), ]
   p <- stats::predict(m1(), nd, se.fit = TRUE)
   return(p)
 })

 #### Define a plot which shows predictions
 output$depth_timeseries_preds <-
   renderPlot(
     prettyGraphics::pretty_ts(x = dat_model()$timestamp,
             y1 = dat_model()$depth *-1,
             fct = dat_model()$individual,
             fct_level = input$selected_individual_preds,
             list_CIs_args = list(fadj = function(x){ x*-1}, pred = pred()),
             pretty_axis_args = list(side = c(3, 2),
                                     pretty = list(n = 5),
                                     axis = list(las = TRUE, cex.axis = cex.axis)
             ),
             mtext_args = list(list(side = 3, text = "Time", line = 2.5, cex = cex.lab),
                               list(side = 2, text = "Depth (m)", line = 2.5, cex = cex.lab)
             )
     )
   )


 ################################################
 ################################################
 #### Model Diagnostics



 ################################################
 #### A) gam.check

 #### Create an object of gam.check()
 output$gam_check <- renderPrint(print(gam.check(m1())))



 ################################################
 #### B) Predictions on the simulated time-series

 ####
 # sim_ts_bam


 ################################################
 #### C) Residual plots (uncorrected and corrected residuals)

 #### Define rand_pc
 rand_pc <- reactive({
   if(input$yes_rand_pc){
     return(input$rand_pc)
   } else {return(NULL)}
 })

 #### Define individual to be plotted
 id_level <- reactive({
   if(input$residuals4id){
     return(input$id_level)
   } else {return(NULL)}
 })

 #### Define a drop down menu for one of the simulated individuals to be selected:
 output$select_an_individual_resids <-
   renderUI(radioButtons(inputId = "id_level",
                         label = strong("Select an individual"),
                         choices = c(1:input$n_individuals),
                         selected = 1,
                         inline = TRUE)
   )

 #### Define inputs for plots
 correction <- reactive({
   if(input$rho_model > 0){
     return(c("uncorrected", "corrected"))
   }
   else {
     return("uncorrected")
   }
 })

 #### define parameters for graphics::par(mfcol...)
 par_mfcol <- reactive({
   # There are 9 possible plots
   # although only 7 are activated at present
   # one plot depends on the number of covariates
   # so the total number of plots is:
   # 6 plots + 1 * length(input$covariates_model) * 2 if we're inputted unstandardised and standardised residuals
   npr <- 6 + length(input$covariates_model)
   if(length(correction()) == 2){
     mfcol <- c(ceiling(npr * 2/2), 2)
   } else {
     mfcol <- c(ceiling(npr/2), 2)
   }
   return(mfcol)
 })

 #### Residuals plots based on uncorrelated residuals
# output$uresid_plots <- renderPlot({
#   #if(input$rho_model == 0 | input$rho > 0 & input$make_uresid_plots){
 #     pp <- graphics::par(mfcol = par_mfcol(), oma = c(2, 2, 2, 2))
 #     prettyGraphics::pretty_residuals(residuals = stats::resid(m1()),
 #                    fv = stats::fitted(m1()),
 #                    lp = stats::fitted(m1()),
 #                    vars = unique(c(input$covariates, input$covariates_model)),
 #                    timestamp = "timestamp",
 #                    dat = dat_model(),
 #                    plot = 1:7,
 #                    rand_pc = rand_pc(),
 #                    plot_rand_pc = c(3, 4)
 #                    )
 #     graphics::par(pp)
 #   #}
 # })

 #### Residual plots based on correlated residuals
 output$cresid_plots <- renderPlot({

   pp <- graphics::par(mfcol = par_mfcol())

   prettyGraphics::pretty_residuals(residuals = stats::resid(m1()),
                                 fv = stats::fitted(m1()),
                                 lp = stats::fitted(m1()),
                                 vars = unique(c(input$covariates, input$covariates_model)),
                                 timestamp = "timestamp",
                                 timestamp_fct = "individual",
                                 dat = dat_model(),
                                 plot = 1:7,
                                 rand_pc = rand_pc(),
                                 plot_rand_pc = 1:7
   )

   if(input$rho_model > 0){
     prettyGraphics::pretty_residuals(residuals = m1()$std.rsd,
                                   fv = stats::fitted(m1()),
                                   lp = stats::fitted(m1()),
                                   vars = unique(c(input$covariates, input$covariates_model)),
                                   timestamp = "timestamp",
                                   timestamp_fct = "individual",
                                   dat = dat_model(),
                                   plot = 1:7,
                                   rand_pc = rand_pc(),
                                   plot_rand_pc = 1:7
     )
   }

   graphics::par(pp)
 })



 ################################################
 ################################################
 #### Close server

 # close shiny server function
 }



################################################
################################################
#### Run App

shinyApp(ui, server)



} #### Close GAMS4DTS function


#### End of code.
################################################
################################################
