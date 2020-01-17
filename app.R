#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(keras)
library(stringr)
library(ggplot2)
library(forcats)
library(shinycssloaders)
library(shinyjs)
library(reticulate)
library(tensorflow)

if ("r-tensorflow" %in% virtualenv_list()) {
  use_virtualenv("r-tensorflow", required = TRUE)
} else {
  cat("r-tensorflow not found. Installing...")
  virtualenv_create(envname = "r-tensorflow")
  install_tensorflow(envname = "r-tensorflow")
  virtualenv_install("r-tensorflow", packages = c('keras','numpy','scipy','pillow'))
}

load_data <- function() {
  model <- application_vgg16(weights = 'imagenet', include_top = TRUE)
  hide("loading_page")
  show("main_content")
  return(model)
}

ui <- fluidPage(
  includeCSS("www/mycss.css"),
  useShinyjs(),
  
  div(
    id = "loading_page",
    h2("Loading VGG-16 Model..."),
    withSpinner(textOutput("dummy"), type = 5, color = "#27A822")
  ),
  
  hidden(
    div(
      id = "main_content",
      
      # Application title
      titlePanel("Shiny-Hosted Keras Model"),
      
      fluidRow(
        column(4,
               wellPanel(
                 radioButtons("inputType", "Input Type", choices = c("Upload Image", "Capture Webcam")),
                 uiOutput("ui")   
               )
        ),
        
        # Display uploaded image
        column(4,
               conditionalPanel(
                 condition = "input.inputType == 'Upload Image'",
                 imageOutput("img")
               ),
               conditionalPanel(
                 condition = "input.inputType == 'Capture Webcam'",
                 tags$img(id='screenshot-img', width='100%', height='100%')
               )
        ),
        column(4,
               conditionalPanel(
                 condition = "input.inputType == 'Upload Image'",
                 plotOutput("upload_prob")
               ),
               conditionalPanel(
                 condition = "input.inputType == 'Capture Webcam'",
                 plotOutput("screenshot_prob")
               )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  model <- load_data()
  
  output$dummy <- renderText({ "temp" })
  
  output$ui <- renderUI({
    
    if (input$inputType == 'Upload Image') {
      fileInput(inputId = "myfile", 
                label = "Upload a photo",
                accept = c('image/png', 'image/jpeg','image/jpg')
      )
    } else {
      tagList(
        tags$p(align='center',
               tags$button("Capture Video ", id='capture-button', class='webcam-button', icon("video")),
               tags$button("Take Screenshot ", id='screenshot-button', class='webcam-button', disabled=NA, icon("camera"))),
        tags$video(class='videostream', width='100%', height='100%', autoplay=NA),
        tags$script(src="webcam.js")
      )
    }
  })
  
  path <- eventReactive(input$myfile, {
    str_replace_all(input$myfile$datapath, "\\\\", "/")
  })
  
  output$img <- renderImage({
    list(src = path(), width = "100%", height = "100%")
  }, deleteFile = FALSE)
  
  screenshot <- eventReactive(input$screenshot, {
    input$img_src %>% 
      str_remove('data:image/png;base64,') %>%
      str_replace(' ', '+') %>%
      base64enc::base64decode() %>%
      png::readPNG() %>% 
      .[,,-4]
  })
  
  output$screenshot_prob <- renderPlot({
    x <- screenshot() %>%
      image_to_array() %>% 
      image_array_resize(224, 224) * 255
    x %>%
      array_reshape(c(1, dim(.))) %>% 
      imagenet_preprocess_input() %>% 
      predict(model, .) %>% 
      imagenet_decode_predictions(top = 5) %>% 
      .[[1]] %>% 
      ggplot(aes(x = fct_reorder(
        tools::toTitleCase(
          str_replace_all(class_description, "_", " ")), score),
        y = score)) + 
      labs(y = NULL,
           x = NULL,
           title = "VGG-16 Predictions") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      geom_col(fill = '#27A822') +
      coord_flip() +
      theme_minimal()
  })
  
  output$upload_prob <- renderPlot({
    path() %>%
      image_load(., target_size = c(224,224)) %>%
      image_to_array() %>%
      array_reshape(c(1, dim(.))) %>%
      imagenet_preprocess_input() %>%
      predict(model, .) %>%
      imagenet_decode_predictions(top = 5) %>%
      .[[1]] %>%
      ggplot(aes(x = fct_reorder(
        tools::toTitleCase(
          str_replace_all(class_description, "_", " ")), score),
        y = score)) +
      labs(y = NULL,
           x = NULL,
           title = "VGG-16 Predictions") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      geom_col(fill = '#27A822') +
      coord_flip() +
      theme_minimal()
  })
  
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

