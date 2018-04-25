library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(

  #  Application title
  headerPanel("Image Distortion Calibration Survey"),

  sidebarPanel(
      # This is intentionally an empty object.

      h6(textOutput("save.results")),
      h5("Survey by G. Z. Thompson"),
      h5("gzt@iastate.edu"),
      h5("Adapted from Survey Tool created by:"),
            tags$a("Econometrics by Simulation",
                   href="http://www.econometricsbysimulation.com"),
            h5("Github Repository:"),
            tags$a("Survey-Tool",
                   href=paste0("https://github.com/EconometricsBySimulation/",
                   "Shiny-Demos/tree/master/Survey")),
            # Display the page counter text.
            h5(textOutput("counter"))
      ),


  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("MainAction"),
    # This displays the action putton Next.
    actionButton("Click.Counter", "Next")
    )
))
