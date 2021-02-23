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
library(grid)
library(formattable)
library(corrplot)
library(colourpicker)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Shiny App"),

   theme = shinytheme("united"),
    # Application title
    #titlePanel("Old Faithful Geyser Data"),

    navbarPage(
        "Tasks",
        tabPanel("Task 1",
            # Sidebar with a slider input for number of bins
            sidebarPanel(
                fileInput("file1", "Select a .csv data file",
                    accept = c("text/csv", ".csv")),
                tags$hr(),
                selectInput("xvar",
                            label = "Select an x variable to plot",
                            choices = "No choices yet"),
                selectInput("yvar",
                            label = "Select a y variable to plot",
                            choices = "No choices yet"),
                selectInput("type",
                            label = "Select the type of plot",
                            choices = c("Lines", "Points", "Smooth", "Quantile", "Jitter")),
                colourInput("col", "Select a color for the plot", "orangered"),
                sliderInput("theta",
                            label = "Enter degree of which axes are rotated",
                            min = -2*3.14,
                            max = 2*3.14,
                            value = 0),
                sliderInput("init",
                            label = "Enter an initial value to calculate first quadrant solution",
                            min = 0,
                            max = 10,
                            value = 1)

            ),

            # Show a plot of the generated distribution
            mainPanel(
                tabsetPanel(type = "pills",
                    tabPanel("Tables and Plots",
                        h3("First 6 rows of data set"),
                        tableOutput("data"),

                        h3("Plot"),
                        plotOutput("ggplot", click = "plot_click"),
                        verbatimTextOutput("info"),

                        h3("Drop one correlation values and plot"),
                        plotOutput("corr"),
                        verbatimTextOutput("corrVal")
                    ),
                    tabPanel("Plot with rotated axes",
                        h3("Plot with rotated axes"),
                        plotOutput("rotatedPlot", click = "plot_click2"),
                        verbatimTextOutput("info2"),

                        h3("First quadrant solution"),
                        verbatimTextOutput("solution")
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

    # Print out first 6 rows of data set
    output$data <- renderTable({
        head(data())
    })

    # Update select x and y variable dropdown
    observe({
        if(is.null(input$file1)){
            return()
        }
        else{
            updateSelectInput(session = session, inputId = "xvar", choices = colnames(data()))
            updateSelectInput(session = session, inputId = "yvar", choices = colnames(data()))
        }
    })


    # Plot ggplot
    output$ggplot <- renderPlot({
        dat <- data()
        type = input$type
        suppressWarnings({
        if(type == "Lines"){
            g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_line(col = input$col) +
                xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                theme(plot.title = element_text(hjust = .5))
        }

        if(type == "Points"){
            g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_point(col = input$col) +
                xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                theme(plot.title = element_text(hjust = .5))
        }

        if(type == "Smooth"){
            g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_smooth(col = input$col) +
                xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                theme(plot.title = element_text(hjust = .5))
        }

        if(type == "Jitter"){
            g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_jitter(col = input$col) +
                xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                theme(plot.title = element_text(hjust = .5))
        }

        if(type == "Quantile"){
            g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_quantile(col = input$col) +
                xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                theme(plot.title = element_text(hjust = .5))
        }

        print(g)
        })

    })

    # Add pointers
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })

    # Output correlations plot
    output$corr <- renderPlot({
        x <- cor(data()[1:ncol(data())])
        corrplot(x, type = "upper", order = "hclust")
    })

    # Output numerical correlations
    output$corrVal <- renderText({
        cor(data())
    })

    transformed.x <- c() # Empty vector used to store transformed x values
    # Rotated plot
    output$rotatedPlot <- renderPlot({
        dat <- data() # Vector holding data
        x.v <- dat[[input$xvar]] # Vector holding input for x
        y.v <- dat[[input$yvar]] # Vector holding input for y

        transformed.y <- c() # Empty vector used to store transformed y values
        rows = nrow(dat) # Number of rows of data matrix

        # Function to rotate the points
        rotate <- function(x,y,theta){
            rotate <- matrix(c(cos(theta), -sin(theta),
                               sin(theta), cos(theta)), nrow = 2, byrow = TRUE)
            point <- matrix(c(x,
                              y), nrow = 2, ncol = 1, byrow = TRUE)
            rotate%*%point
        }

        for(i in 1:30){
            transformed.x[i] <- rotate(x.v[i],y.v[i],input$theta)[1,]
            transformed.y[i] <- rotate(x.v[i],y.v[i],input$theta)[2,]
        }

        trans.val <- data.frame(transformed.x = transformed.x, transformed.y = transformed.y)

        type = input$type
        max.x = max(x.v)
        max.y = max(y.v)
        g.transf <- ggplot(trans.val, aes(x = transformed.x, y = transformed.y)) + geom_blank()
        suppressWarnings({
            if(type == "Lines"){
                g.transf <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_line(col = input$col) +
                    xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                    theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = 0, slope = tan(-input$theta), color = "red") +
                    geom_abline(intercept = 0, slope = tan(-input$theta + pi/2), color = "red") +
                    xlim(-max.x, max.x) + ylim(-max.y, max.y) + annotate("text", x = 0, y = max.y/2, label = paste0("Correlation: ", round(cor(trans.val)[1,2],4)), alpha = 2)
            }

            if(type == "Points"){
                g.transf <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]]))+ geom_point(col = input$col) +
                    xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                    theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = 0, slope = tan(-input$theta), color = "red") +
                    geom_abline(intercept = 0, slope = tan(-input$theta + pi/2), color = "red") +
                    xlim(-max.x, max.x) + ylim(-max.y, max.y) + annotate("text", x = 0, y = max.y/2, label = paste0("Correlation: ", round(cor(trans.val)[1,2],4)), alpha = 2)
            }

            if(type == "Smooth"){
                g.transf <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_smooth(col = input$col, method = "lm", formula = y~x) +
                    xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                    theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = 0, slope = tan(-input$theta), color = "red") +
                    geom_abline(intercept = 0, slope = tan(-input$theta + pi/2), color = "red") +
                    xlim(-max.x, max.x) + ylim(-max.y, max.y) + annotate("text", x = 0, y = max.y/2, label = paste0("Correlation: ", round(cor(trans.val)[1,2],4)), alpha = 2)
            }

            if(type == "Jitter"){
                g.transf <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_line(col = input$col) +
                    xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                    theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = 0, slope = tan(-input$theta), color = "red") +
                    geom_abline(intercept = 0, slope = tan(-input$theta + pi/2), color = "red") +
                    xlim(-max.x, max.x) + ylim(-max.y, max.y) + annotate("text", x = 0, y = max.y/2, label = paste0("Correlation: ", round(cor(trans.val)[1,2],4)), alpha = 2)
            }

            if(type == "Quantile"){
                g.transf <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]])) + geom_quantile(col = input$col) +
                    xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
                    theme(plot.title = element_text(hjust = .5)) + geom_abline(intercept = 0, slope = tan(-input$theta), color = "red") +
                    geom_abline(intercept = 0, slope = tan(-input$theta + pi/2), color = "red") +
                    xlim(-max.x, max.x) + ylim(-max.y, max.y) + annotate("text", x = 0, y = max.y/2, label = paste0("Correlation: ", round(cor(trans.val)[1,2],4)), alpha = 2)
            }
        })


        print(g.transf)
    })

    output$info2 <- renderText({
        paste0("x=", input$plot_click2$x, "\ny=", input$plot_click2$y)
        paste0(transformed.x)
    })

    output$solution <- renderText({
        # Calculate s11, s22, s12 tilde
        dat <- data()
        nr = dim(dat)[1]
        data.mat <- matrix(c(dat[[input$xvar]],
                             dat[[input$yvar]]), nrow = nr, ncol = 2, byrow = FALSE)

        # Find mean corrected matrix
        vec.1 <- rep(1, nr)
        mean.corrected <- data.mat - vec.1%*%t(colMeans(data.mat))

        # Unbiased covariance matrix
        s <- cov(data.mat, mean.corrected)

        # Obtain covariance values
        s11 = s[1,1]
        s22 = s[2,2]
        s12 = s[1,2]

        # The function below is what we want to find the zero of
        myfun=function(x){
            s11 <<- s11
            s22 <<- s22
            s12 <<- s12
            (s11-s22)*cos(x)*sin(x) - s12*cos(x)^2 + s12*sin(x)^2
        }


        # The Newton-Raphson algorithm function
        my.newt=function(x0,f=myfun,delta=0.0001,epsilon=1e-12){
            # x0 initial value
            #f the function to be zeroed
            #delta is the increment in the derivative
            #epsilon is how close our approximation is to zero
            fdash=function(x) (f(x+delta)-f(x))/delta

            d=1000 # initial values
            i=0
            x=c() # empty vector
            y=c()
            x[1]=x0 # assign initial guess
            y[1]=f(x[1]) # initial y value
            while(d > epsilon & i<100){ # ensures that it doesnt loop too much
                i=i+1
                x[i+1]= x[i]-f(x[i])/fdash(x[i])# NR step
                y[i+1]=f(x[i+1]) # update y value
                d=abs(y[i+1]) # update d
            }
            nn=length(x)
            list(x=x,y=y,d=d, root=x[nn])
        }

        paste0("d = ", my.newt(input$init, f = myfun)$d, "\nRoot = ",my.newt(input$init, f = myfun)$root)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
