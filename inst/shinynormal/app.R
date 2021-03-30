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
               selectInput("propvar",
                           label = "Select a variable to do proportion test of normality on",
                           choices = "No choices yet"),
               selectInput("xvar",
                           label = "Select an x variable to plot (marginal, bivariate plots)",
                           choices = "No choices yet"),
               selectInput("yvar",
                           label = "Select a y variable to plot (marginal, bivariate plots)",
                           choices = "No choices yet"),
               selectInput("type",
                           label = "Select the type of plot",
                           choices = c("Points", "Lines", "Smooth")),
               sliderInput("alpha",
                           label = "Select rejection level alpha",
                           min = 0,
                           max = 1.00,
                           value = .01),
               selectInput("dotvar",
                           label = "Select a variable to plot marginal dot plot",
                           choices = "No choices yet"),
               checkboxInput("showoutlier",
                             label = "Show outlier",
                             value = FALSE),
               numericInput("lambda",
                            label = "Select a choice of lambda for maximizing function",
                            value = 1),
               selectInput("boxcoxvar",
                           label = "Choose variable to transform",
                           choices = "No choices yet"),
               sliderInput("range",
                           label = "Select range for the box cox plot",
                           min = -5,
                           max = 5,
                           value = c(-3,3),
                           dragRange = TRUE)


             ),

             # Show a plot of the generated distribution
             mainPanel(
               tabsetPanel(type = "pills",
                           tabPanel("Univariate test and plots",
                                    h3("Proportion test of normality"),
                                    verbatimTextOutput("proportion"),

                                    h3("Proportion of data within 1-2 standard deviations of mean"),
                                    plotOutput("propplot"),

                                    h3("Univariate Q-Q plot"),
                                    plotOutput("univariateqq"),

                                    h3("Shapiro Wilk P-Value"),
                                    verbatimTextOutput("shapiro"),

                                    h3("Correlation coefficient"),
                                    reactableOutput("correlation")
                           ),
                           tabPanel("Bivariate test and plots",
                                    h3("Bivariate normality test"),
                                    verbatimTextOutput("bivariatetest"),

                                    h3("Confidence ellipse plot"),
                                    plotOutput("ellipse"),

                                    h3("Q-Q plot"),
                                    plotOutput("bivariateqq")
                           ),
                           tabPanel("Detecting outliers",
                                    h3("Marginal dot plots"),
                                    plotOutput("dot"),


                                    h3("Marginal plots"),
                                    plotOutput("ggplot"),

                                    h3("Standardized values"),
                                    reactableOutput("zval")


                           ),
                           tabPanel("Box cox transformation",
                                    h3("Box cox transformed data plot"),
                                    plotOutput("boxcoxplot"),

                                    h3("Shapiro-Wilk p-value based on chosen lambda"),
                                    verbatimTextOutput("shapirobox"),

                                    h3("Q-Q Plot of transformed data"),
                                    plotOutput("qqbox")
                           )

               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # Proportion test

  prop <- function(x){
    # x is a data vector
    n <- length(x)
    outliers = FALSE
    idx = 0

    totalp1 = 0
    for(i in 1:n){
      if((x[i] > (mean(x) - sd(x))) & (x[i] < (mean(x) + sd(x)))){
        totalp1 = totalp1 + 1
      }
    }

    p1 <- totalp1/n


    totalp2 = 0
    for(i in 1:n){
      if((x[i] > (mean(x) - 2*sd(x))) & (x[i] < (mean(x) + 2*sd(x)))){
        totalp2 = totalp2 + 1
      }
    }
    p2 <- totalp2/n

    boundary1 <- 1.396/sqrt(n)
    boundary2 <- .628/sqrt(n)
    if((abs(p1- .683) > boundary1)){
      outliers = TRUE
    }

    return(list(p1 = p1, p2 = p2, outliers.present = outliers))
  }


  # Function to check for bivariate norm
  bivariatenorm <- function(x1,x2){
    # x1 is a data vector
    # x2 is another data vector



    n <- length(x1)
    dat <- matrix(c(x1,x2), nrow = length(x1), ncol = 2, byrow = FALSE)
    xbar <- colMeans(dat)
    cov <- cov(dat)
    inv <- solve(cov)
    chisq <- qchisq(.5,2)

    # Counters to hold the number of values greater/less than chi-square value
    greater = 0
    less = 0
    norm = FALSE
    for(i in 1:n){
      mat <- matrix(c(x1[i] - xbar[1],
                      x2[i] - xbar[2]), ncol = 1)
      tsq <- t(mat)%*%inv%*%mat
      if(tsq <= chisq){
        less = less + 1
      }

      if(tsq > chisq){
        greater = greater + 1
      }
    }
    if(less/n >= .5){
      norm = TRUE
    }

    return(list(n=n, mean.vector=xbar,covariance.matrix=cov,chi.square.stat = chisq, number.greater = greater,
                number.less = less, normal = norm))
  }

  # Function that calculates correlation coefficient
  rq <- function(x){
    # x is a data vector

    x <- sort(x)
    sum_num = 0
    sum_denom1 = 0
    sum_denom2 = 0
    n = length(x)
    for(i in 1:n){
      quant <- qnorm((i-1/2)/n)
      sum_num = sum_num + ((x[i] - mean(x))*(quant))
      sum_denom1 = sum_denom1 + ((x[i] - mean(x))^2)
      sum_denom2 = sum_denom2 + quant^2
    }

    rq = sum_num/(sqrt(sum_denom1)*sqrt(sum_denom2))
    return(rq)
  }
  uniqq <- function(x){
    x.sort <- sort(x)
    chisq <- c()
    n <- length(x.sort)
    for(i in 1:n){
      chisq[i] = qchisq((i-1/2)/n, df = 1)
    }

    g <- ggplot(data = NULL, aes(x = chisq, y = x.sort, color = 1:n)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "black") +
      scale_color_gradient(low="blue", high="red") +
      xlab("chi-square") +
      ylab("Ordered observations") +
      ggtitle(paste0("Q-Q plot of ", input$xvar)) +
      theme(plot.title = element_text(hjust = .5)) +
      theme(legend.position = "none")
    return(g)
  }

  qqplot <- function(x1.v,x2.v){
    # x1 is a data vector
    # x2 is another data vector
    n <- length(x1.v)
    x1 <- sort(x1.v)
    x2 <- sort(x2.v)
    dat <- matrix(c(x1,x2), nrow = length(x1), ncol = 2, byrow = FALSE)
    xbar <- colMeans(dat)
    cov <- cov(dat)
    inv <- solve(cov)
    dsq <- c()


    chisq <- c()
    for(i in 1:n){
      mat <- matrix(c(x1[i] - xbar[1],
                      x2[i] - xbar[2]), ncol = 1)
      dsq[i] = t(mat)%*%inv%*%mat
      chisq[i] = qchisq((i-1/2)/n,2)
    }
    chisq[is.na(chisq)] <- 0

    qq <- ggplot(data = NULL, aes(x = chisq, y = sort(dsq), color = 1:n)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "black") +
      scale_color_gradient(low="blue", high="red") +
      xlab("chi-square") +
      ylab("Mahalanobis distance") +
      ggtitle(paste0("Q-Q plot of ", input$xvar, " and ", input$yvar)) +
      theme(plot.title = element_text(hjust = .5)) +
      theme(legend.position = "none")
    return(list(dsq = dsq, chisq = chisq, plot = qq))

  }
  boxcox.trans <- function(data,lambda){
    # Data is a vector
    n = length(data)

    xlambda.sum = 0
    xlambda = NULL
    for(i in 1:n){

      if(lambda != 0){
        xlambda <- (data[i]^lambda - 1)/lambda
        xlambda.sum = xlambda.sum + xlambda
      }
      else {
        xlambda <- log(data[i])
        xlambda.sum = xlambda.sum + xlambda
      }
    }

    xlambda.avg = xlambda.sum/n
    sum1 = 0
    sum2 = 0
    loglambda = 0
    xlambda.vec <- c()
    for(i in 1:n){
      if(lambda != 0){
        xlambda2 <- (data[i]^lambda - 1)/lambda
        xlambda.vec[i] = xlambda2
      }
      else {
        xlambda2 <- log(data[i])
        xlambda.vec[i] = xlambda2
      }
      sum1 = sum1 + ((1/n)*(xlambda2 - xlambda.avg)^2)
      sum2 = sum2 + log(data[i])
    }

    loglambda = (-n/2)*log(sum1) + (lambda - 1)*sum2

    x.no.inf <- xlambda.vec
    x.no.inf[sapply(x.no.inf,is.infinite)] <- NA

    shapiro.p <- shapiro.test(x.no.inf)$p.val


    return(list(loglambda = loglambda, xlambda = xlambda.vec, shapiro.p = shapiro.p))
  }

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
      updateSelectInput(session = session, inputId = "xvar", choices = colnames(data()))
      updateSelectInput(session = session, inputId = "yvar", choices = colnames(data()))
      updateSelectInput(session = session, inputId = "dotvar", choices = colnames(data()))
      updateSelectInput(session = session, inputId = "boxcoxvar", choices = colnames(data()))
      updateSelectInput(session = session, inputId = "propvar", choices = colnames(data()))
    }
  })

  output$shapiro <- renderText({
    dat <- data()
    result <- shapiro.test(dat[[input$xvar]])$p.val
    print(result)
  })
  output$propplot <- renderPlot({
    dat <- data()
    p <- ncol(dat)
    n <- nrow(dat)
    name <- names(dat)
    name.vec <- list()
    data.vec <- list()
    for(i in 1:p){
      name.vec[[i]] <- rep(name[i],n)
      data.vec[[i]] <- dat[,i]
    }
    name.as.vec <- unlist(name.vec)
    data.as.vec <- as.numeric(unlist(data.vec))
    g <- ggplot(NULL, aes(x = name.as.vec, y = data.as.vec, fill = factor(name.as.vec))) + geom_boxplot() +
      xlab("Variable") +
      ylab("Value")
    g
  })



  output$proportion <- renderPrint({
    dat <- data()
    print(prop(dat[[input$propvar]]))

  })



  # Plot scatter ggplots
  output$ggplot <- renderPlot({
    dat <- data()
    type = input$type
    suppressWarnings({
      n <- length(dat[[input$xvar]])
      if(type == "Lines"){
        g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]], color = 1:n)) + geom_line() +
          xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
          theme(plot.title = element_text(hjust = .5)) +
          scale_color_gradient(low="blue", high="red") +
          theme(legend.position = "none")
      }

      if(type == "Points"){
        g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]], color = 1:n)) + geom_point() +
          xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
          theme(plot.title = element_text(hjust = .5)) +
          scale_color_gradient(low="blue", high="red") +
          theme(legend.position = "none")
      }

      if(type == "Smooth"){
        g <- ggplot(data(), aes(x = dat[[input$xvar]], y = dat[[input$yvar]], color = 1:n)) + geom_smooth(color = "magenta2") +
          xlab(input$xvar) + ylab(input$yvar) + ggtitle(paste0(input$yvar, " vs. ", input$xvar)) +
          theme(plot.title = element_text(hjust = .5)) +
          scale_color_gradient(low="blue", high="red") +
          theme(legend.position = "none")
      }


      print(g)
    })

  })

  output$bivariatetest <- renderPrint({
    dat <- data()
    x1 <- dat[[input$xvar]]
    x2 <- dat[[input$yvar]]

    print(bivariatenorm(x1,x2))

  })


  # Confidence ellipse
  output$ellipse <- renderPlot({
    dat <- data()
    n <- length(dat[[input$xvar]])
    # Plot data and confidence ellipse
    g.ellipse <- qplot(data = dat, x = dat[[input$xvar]], y = dat[[input$yvar]], color = 1:n) +
      stat_ellipse(geom = "polygon",level = 1-input$alpha, type = "norm", alpha = 1/10, color = "orangered", aes(fill = "orangered")) +
      theme(legend.position = "none") +
      ylab(input$yvar) +
      xlab(input$xvar) +
      ggtitle(paste0((1-input$alpha)*100, "% confidence ellipse")) +
      theme(plot.title = element_text(hjust = .5)) +
      scale_color_gradient(low="blue", high="red")
    g.ellipse
  })

  # Task 2 univariate Q-Q plot
  output$univariateqq <- renderPlot({
    dat <- data()
    uniqq(dat[[input$xvar]])
  })
  # Task 3 Q-Q plot
  output$bivariateqq <- renderPlot({
    dat <- data()
    qqplot(dat[[input$xvar]], dat[[input$yvar]])$plot
  })


  output$correlation <- renderReactable({
    dat <- data()
    n = nrow(dat)
    p = ncol(dat)
    mat <- matrix(,nrow = 1, ncol = p)
    for(i in 1:p){
      mat[1,i] = rq(dat[,i])
    }
    colnames(mat) <- names(dat)
    react <- reactable(mat, highlight = TRUE,
                       theme = reactableTheme(
                         color = "black",
                         backgroundColor = "peachpuff"
                       ))
    print(react)

  })

  output$dot <- renderPlot({
    dat <- data()
    n = ncol(dat)
    g.dot <- ggplot(dat, aes(x = dat[[input$dotvar]])) + geom_dotplot() +
      ylab("count") +
      xlab(input$dotvar)
    g.dot


  })


  output$zval <- renderReactable({
    dat <- data()
    S <- cov(dat)
    n = nrow(dat)
    p = ncol(dat)
    z.mat <- matrix(, nrow = n, ncol = p)
    colnames(z.mat) <- c(paste0("z", 1:p))
    rownames(z.mat) <- c(1:n)
    for(i in 1:n){
      for(j in 1:p){
        z <- (dat[i,j] - mean(dat[,j]))/sqrt(S[j,j])
        z.mat[i,j] = round(z,4)
      }
    }

    if(input$showoutlier == TRUE){
      z.tab <- reactable(z.mat, highlight = TRUE,
                         rowStyle = function(row){
                           for(i in 1:ncol(z.mat)){
                             if(abs(z.mat[row,i]) > 3.5){
                               list(background = "orangered")
                             }
                           }
                         },
                         theme = reactableTheme(
                           color = "black",
                           backgroundColor = "peachpuff"
                         )
      )
    }
    else{
      z.tab <- reactable(z.mat, highlight = TRUE,
                         theme = reactableTheme(
                           color = "black",
                           backgroundColor = "peachpuff"
                         )
      )
    }
    print(z.tab)
  })


  output$boxcoxplot <- renderPlot({
    dat <- data()
    range <- seq(input$range[1],input$range[2], by = .5)

    log <- c()
    for(i in 1:length(range)){
      log[i] <- boxcox.trans(dat[[input$boxcoxvar]], range[i])$loglambda
    }



    spline_int <- as.data.frame(spline(range,log))
    n <- length(dat[[input$boxcoxvar]])
    g <- ggplot(NULL, aes(x = range, y = log)) +
      geom_line(data = spline_int, aes(x = x, y = y)) +
      geom_vline(xintercept = input$lambda, color = "orangered", lty = 2) +
      xlab(TeX("$\\lambda$")) +
      ylab(TeX("$ln(\\lambda)$"))
    g

  })

  output$shapirobox <- renderPrint({
    dat <- data()
    print(boxcox.trans(dat,input$lambda)$p.val)
  })


  output$qqbox <- renderPlot({
    chisq <- c()
    dat <- data()
    n <- length(dat[[input$boxcoxvar]])
    range <- seq(input$range[1],input$range[2], by = .5)

    x <- c()
    for(i in 1:length(range)){
      x[i] <- boxcox.trans(dat[[input$boxcoxvar]], range[i])$xlambda
    }

    for(i in 1:length(range)){
      chisq[i] <- qchisq((i - 1/2)/n,1)
    }

    g <- ggplot(NULL, aes(x = chisq, y = sort(x), color = 1:length(x))) + geom_point() +
      geom_smooth(method = "lm", se = FALSE, col = "black") +
      xlab(TeX("$q_{j}$")) +
      ylab(TeX("$x_{(j)}^{(\\lambda)}$")) +
      scale_color_gradient(low="blue", high="red")
    g
  })



}

# Run the application
shinyApp(ui = ui, server = server)
