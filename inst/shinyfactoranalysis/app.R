#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(reactable)
library(latex2exp)
library(ggalt)
library(MATH5793BANH)
library(dplyr)
library(kableExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Shiny App"),
    
    theme = shinytheme("united"),
    # Application title
    #titlePanel("Old Faithful Geyser Data"),
    
    navbarPage(
        "Factor Analysis",
        tabPanel("",
                 # Sidebar with a slider input for number of bins
                 sidebarPanel(
                     fileInput("file1", "Select a .csv data file",
                               accept = c("text/csv", ".csv")),
                     tags$hr(),
                     sliderInput("m",
                                 label = "Select the number of factors in model",
                                 min = 0,
                                 max = 0,
                                 value = 0,
                                 step = 0),
                     selectInput("R", 
                                 label = "Select structure",
                                 choices = c("Covariance", "Correlation"), selected = "Correlation"),
                     numericInput("alpha",
                                 label = "Select significance level", 
                                 min = 0,
                                 max = 1,
                                 value = .05,
                                 step = .01)
                     
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     tabsetPanel(type = "pills",
                                 tabPanel("Tables",
                                          h3("Loadings and variances"),
                                          reactableOutput("load"),
                                          
                                          h3("Residual matrix"),
                                          reactableOutput("residual"),
                                          
                                          h3("Test of Factor Model Adequacy"),
                                          reactableOutput("factortest"),
                                          
                                          h3("Table with Rotated Loadings"),
                                          reactableOutput("rotated")
                                 ),
                                 tabPanel("Plots",
                                          h3("Plots"),
                                          plotOutput("communality"),
                                          plotOutput("variance"),
                                          plotOutput("pairs"),
                                          plotOutput("varimax"),
                                          plotOutput("promax")
                                          )
                                 
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    # Read in data set
    data <- eventReactive(input$file1,{
        read.csv(input$file1$datapath)
    })
    
    
    # Update select x and y variable dropdown
    observe({
        if(is.null(input$file1)){
            return()
        }
        else{
            updateSliderInput(session = session, inputId = "m", min = 2, max = dim(data())[2], value = 2, step = 1)
        }
    })
    
    
    
    
    output$load <- renderReactable({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        react <- reactable(data.frame(unclass(obj$table)), highlight = TRUE,
                           theme = reactableTheme(
                               color = "black",
                               backgroundColor = "peachpuff"
                           ))
        print(react)
    })
    
    
    output$residual <- renderReactable({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        react <- reactable(data.frame(unclass(obj$residual.mat)), highlight = TRUE,
                           theme = reactableTheme(
                               color = "black",
                               backgroundColor = "peachpuff"
                           ))
    })
    
    output$factortest <- renderReactable({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        react <- reactable(factorModelTest(obj, input$alpha), highlight = TRUE,
                           theme = reactableTheme(
                               color = "black",
                               backgroundColor = "peachpuff"
                           ))
        print(react)
    })
    
    
    output$rotated <- renderReactable({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        rot.loadings <- plot(obj)$rotated.table
        react <- reactable(data.frame(unclass(rot.loadings)), highlight = TRUE,
                           theme = reactableTheme(
                               color = "black",
                               backgroundColor = "peachpuff"
                           ))
    })
    
    
    output$communality <- renderPlot({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        p <- plot(obj)
        print(p[[4]])
    })
    output$variance <- renderPlot({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        p <- plot(obj)
        print(p[[5]])
    })
    output$pairs <- renderPlot({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        p <- plot(obj)
        print(p[[6]])
    })
    output$varimax <- renderPlot({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        p <- plot(obj)
        print(p[[7]])
    })
    output$promax <- renderPlot({
        if(input$R == "Covariance"){
            obj <- pcfacta(data(),input$m, R = FALSE)
        }
        else{
            obj <- pcfacta(data(), input$m)
        }
        p <- plot(obj)
        print(p[[8]])
    })
    
    
    
    
    
    
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
