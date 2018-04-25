library(shiny)
library(rdrop2)
library(digest)
token <- readRDS("token.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)

# generate data from of possible images
imagelist = data.frame(expand.grid(1:15,1:12))
imagelist$imageone = paste0("image-",imagelist[,1],".png")
imagelist$imagetwo = paste0("image-",imagelist[,1],"-",imagelist[,2],".png")
#
# imagelisttwo = data.frame(image = 1:12,k=sample(c(8,16,32),12,replace=T),result=0)
# imagelisttwo$imageone = paste0("color-",imagelisttwo$image,"-",imagelisttwo$k,"-hcl.jpg")
# imagelisttwo$imagetwo = paste0("color-",imagelisttwo$image,"-",imagelisttwo$k,"-luv.jpg")
# imagelisttwo$imagethree = paste0("color-",imagelisttwo$image,"-",imagelisttwo$k,"-rgb.jpg")
# imagelisttwo$imagefour = paste0("color-",imagelisttwo$image,"-",imagelisttwo$k,"-xyz.jpg")
# imagelisttwo = imagelisttwo[sample(1:12,6)]
#responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
# Qlist <- read.csv("Qlist.csv")
# Qlist <- Qlist[1,]
outputDir <- "imagesurvey/responses"

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}
#
# saveDataTwo <- function(data) {
#   # Create a unique file name
#   fileName <- sprintf("second_%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
#   # Write the data to a temporary file locally
#   filePath <- file.path(tempdir(), fileName)
#   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
#   # Upload the file to Dropbox
#   drop_upload(filePath, path = outputDir)
# }

shinyServer(function(input, output) {
  # nq = number of questions to ask
  nq <<- 30
  #nqtwo <<-12
  # generate list to ask about
  Qlist <<- imagelist[sample(nrow(imagelist),nq),]
  Qlist$result = 0
  # Create an empty vector to hold survey results
  results <<- rep("", nq)
  # Name each element of the vector based on the
  # fourth column of the Qlist
  #names(results)  <<- Qlist[,4]

  # Hit counter
  output$counter <-
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1

      save(counter, file="counter.Rdata")
      paste0("Hits: ", counter)
    })

  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })

  # Dynamic UI is the interface which changes as the survey
  # progresses.
  dynamicUi <- reactive({
    # Initially it shows a welcome message.
    if (input$Click.Counter==0)
      return(
        list(
          h4("Image Quality Calibration"),
          h5("by Geoffrey Thompson"),
          tags$p("I'm making a metric for comparing binary and multiclass images to each other and need to calibrate against human perceptions. In this survey, you will be shown a series of pairs of images. The image on the left will be the original image and the image on the right will be a distorted version. "),
          tags$p("There is a slider bar beneath the images with a scale from 0, for a poor similarity to the original, to 100, for a perfect similarity to the original image. Please assess the distortion of each image."),
          #tags$p("In the second part of the survey, you will be presented with four images and asked to choose which has the looks the best."),
          tags$p("If you have any questions or concerns about the study, please let me know at gzt@iastate.edu")
        )
      )

    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist)){
      firstimage = Qlist$imageone[input$Click.Counter]
      secondimage = Qlist$imagetwo[input$Click.Counter]
      return(
        list(
          h5(textOutput("question")),
          tags$h6("Scale:"),
          tags$p("0-20: Poor"),
          tags$p("20-40: Bad"),
          tags$p("40-60: Fair"),
          tags$p("60-80: Good"),
          tags$p("80-100: Excellent"),
          ## insert images
          tags$table( border=5,
            tags$tr(
              tags$td(cellpadding=2,
                img(src=firstimage, alt="Undistorted Image", style="width:256px")
              ) ,
              tags$td(p(".")),
              tags$td(
                img(src=secondimage, alt="Distorted Image", style="width:256px")
              )
            )
          ) ,
          # tags$img(src=firstimage),
          # tags$p(renderText(paste0(secondimage))),
          sliderInput("survey",
                      "Similarity:",
                      min = 0,
                      max = 100,
                      value = 50)
          )
        )
    }

    # if (input$Click.Counter>nrow(Qlist) & input$Click.Counter<=nrow(Qlist)+nrow(imagelisttwo)){
    #   firstimage = imagelisttwo$imageone[input$Click.Counter-nrow(Qlist)]
    #   secondimage = imagelisttwo$imagetwo[input$Click.Counter-nrow(Qlist)]
    #   thirdimage = imagelisttwo$imagethree[input$Click.Counter-nrow(Qlist)]
    #   fourthimage = imagelisttwo$imagefour[input$Click.Counter-nrow(Qlist)]
    #   return(
    #     list(
    #       h5("Please indicate which of these four images has the best quality.",
    #       ## insert images
    #       tags$table( border=1,
    #                   tags$tr(
    #                     tags$td(tags$h6("1")),
    #                     tags$td(
    #                       img(src=firstimage, alt="Undistorted Image")
    #                     )
    #                     ),
    #                   tags$tr(
    #                     tags$td(tags$h6("2")),
    #                     tags$td(
    #                       img(src=secondimage, alt="Undistorted Image")
    #                     )
    #                   ),tags$tr(
    #                     tags$td(tags$h6("3")),
    #                     tags$td(
    #                       img(src=thirdimage, alt="Undistorted Image")
    #                     )
    #                   ),tags$tr(
    #                     tags$td(tags$h6("4")),
    #                     tags$td(
    #                       img(src=fourthimage, alt="Undistorted Image")
    #                     )
    #                   )
    #                   )
    #       ) ,
    #       # tags$img(src=firstimage),
    #       # tags$p(renderText(paste0(secondimage))),
    #       radioButtons("survey", "Please Select:",
    #                    c(1:4))
    #       )
    #     )
    #
    # }

    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter>nrow(Qlist))
      return(
        list(
          #h4("View aggregate results"),
          #tableOutput("surveyresults"),
          h4("Thanks for taking the survey!"),
          #downloadButton('downloadData', 'Download Individual Results'),
          br(),
          h6("Sorry, haven't figured out how to get rid of the last 'next' button.")
          )
        )
  })

  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter>0)&(input$Click.Counter<=nrow(Qlist))){
      try(results[input$Click.Counter] <<- input$survey)
      try(Qlist$result[input$Click.Counter] <<- results[input$Click.Counter])
    }
    # if ((input$Click.Counter>nrow(Qlist))&(input$Click.Counter<=nrow(Qlist)+nrow(imagelisttwo))){
    #   try(results[input$Click.Counter] <<- input$survey)
    #   try(imagelisttwo$result[input$Click.Counter-nrow(Qlist)] <<- results[input$Click.Counter])
    # }
      # try is used because there is a brief moment in which
      # the if condition is true but input$survey = NULL

    # If the user has clicked through all of the survey questions
    # then R saves the results to the survey file.
    #if (input$Click.Counter == nrow(Qlist)+1) {
    #  saveData(Qlist)
    #}
    if (input$Click.Counter == nrow(Qlist)+1) {
      #saveDataTwo(imagelisttwo)
      saveData(Qlist)
    }
    # Because there has to be a UI object to call this
    # function I set up render text that distplays the content
    # of this funciton.
    ""
  })

  # This function renders the table of results from the
  # survey.
  output$surveyresults <- renderTable({
    (summary(presults))
  })

  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )

  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:4]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix.
    as.matrix(qlist[qlist!=""])
  })

  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Q ", input$Click.Counter,"/30 : ",
      "Please rate the similarity of the image on the right to the image on the left, with 0 being poor similarity and 100 being a perfect match."
    )
  })

})


