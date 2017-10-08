# Interactive diagnostic plots for outliers in linear regression based on recomendations of
# Field, Andy P. Discovering statistics using SPSS: (and sex, drugs and rock „n“ roll). 2. Aufl. London, Thousand Oaks, Calif.: Sage Publications, 2005.


# Johann Popp
# 2017-10-08
#########################################################

# Load example data (Record sales)
dat <- read.delim("https://studysites.uk.sagepub.com/dsur/study/DSUR%20Data%20Files/Chapter%207/Album%20Sales%202.dat")

# Build example model
modLM <- lm(sales ~ adverts + airplay + attract, data = dat)

# Labels to plot
# axLabs <- c("id", "Fitted Values", "Standardized Residuals" , "Absolute Std. Residuals", paste("Absolute Std. DFBeta -", names(coef(modPlot()))),paste("DFBeta -", names(coef(modPlot()))), "Standardized DFFit", "Absolute Std. DFFit", "Covariance Ratio", "Cook's Distance", "Leverage",names(modPlot()$model))
# axLabs <- c("id", "Fitted Values", "Standardized Residuals" , "Absolute Std. Residuals", paste("Absolute Std. DFBeta -", names(coef(modLM))),paste("DFBeta -", names(coef(modLM))), "Standardized DFFit", "Absolute Std. DFFit", "Covariance Ratio", "Cook's Distance", "Leverage",names(modLM$model))

# Labels to choose


################################### 

library(shiny)
library(ggplot2)

# Shiny User Interface
ui <- fluidPage(
  
  
  h1("Outlier Diagnostics for Linear Regression"),
  # Introduction
  
  p("With these plots you can search for and investigate extreme and influential cases in a linear regression model as recommended by Field 2005¹. As an example the model that is discribed in the book is used for the plots, but you can upload your own data and model."),  
  tags$hr(),
  
  ###################
  # Enter Data and model
  h3("Enter Data"),
  
  fluidRow(
    column(4,
           fileInput("inData", "Choose File")),
    column(4,
           radioButtons("fileType", "File Type", c(
             "Text: csv (seperator = ',', decimal = '.')",
             "Text: csv2 (sepatator = ';', decimal = ',')",
             "Text, other",
             "SPSS")))
    ,
    column(4,
           conditionalPanel("input.fileType == 'Text, other'", {
             wellPanel(
               checkboxInput("header", "Variable names in 1st line", value = TRUE),
               textInput("sep", "Separator", value = ";", width = "60px"),
               textInput("dec", "Decimal", value = ",", width = "60px"),
               numericInput("skip", "Skip Lines", value = 0, width = "60px"),
               textInput("quote", "Quoting Characters", value = "\"", width = "60px")
             )}) # end conditional panel
    ) # end column
  ), # end fluidRow
  tableOutput("dataHeader"),
  tags$h4("Names of available Variables"),
  textOutput("varNames"),
  
  tags$h3("Enter Model Formula"),
  fluidRow(
    column(3,
           selectInput("depVar", "Dependent Variable", choices = "",width = 200)),
    column(4,
           selectInput("indepVar", "Independent Variables", choices = "", multiple = TRUE, selected = "1")),
    column(5,
           textInput("initForm", "Custom Formula", value = ". ~ ."),
           "Specify a model formula accoring to R conventions described", a(target = "_blank", href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html", "here"), "."),
    column(1, 
           actionButton("applyFormula", "Apply"))),
  verbatimTextOutput("initMod"),
            verbatimTextOutput("info"),
  actionButton("modSubmit", "Plot Diagnostics of this Model"),
  hr(),
  
  #####################
  # Show plots
  h3("Diagnostic Plots"),
  p("Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected cases and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end", tags$strong("never exclude data just because it does not fit your model.")),
  p("Critical thresholds are marked with lines:",
    tags$ul(
      tags$li("Standardized residuals: +-3 standard deviations"),
      tags$li("Leverage: 2 and 3 times the mean leverage"),
      tags$li("Cook's distance: 1"),
      tags$li("Covariance ratio: 1 +-[3(p)/n]  (p = number of model parameters; n = sampe size )"),
      tags$li("Standardized DFBetas & DFFit: 1")
    )),
  
  fluidRow(
    column(4,
           plotOutput("plot1", height = 250,
                      brush = brushOpts(id = "brP")),
           plotOutput("plot4", height = 250,
                      brush = brushOpts(id = "brP"))
    ),
    column(4,
           plotOutput("plot2", height = 250,
                      brush = brushOpts(id = "brP")),
           plotOutput("plot5", height = 250,
                      brush = brushOpts(id = "brP"))
    ),
    column(4,
           plotOutput("plot3", height = 250,
                      brush = brushOpts(id = "brP")),
           plotOutput("plot6", height = 250,
                      brush = brushOpts(id = "brP")),
           column(12, offset = 1,
                  selectInput("y6", "Y-axis: ", choices = "",
                              # choices = axLabs, 
                              # selected = axLabs[grep("DFBeta", axLabs)[1]], 
                              selectize = FALSE, width = "80%"))
    )
  ), # end of plotting fluid row 1 +2
  fluidRow(
    column(4,
           plotOutput("plot7", height = 250,
           brush = brushOpts(id = "brP")),
           column(12, offset = 1,
                  selectInput("y7", "Y-axis: ", choices = "",
                              selectize = FALSE, width = "80%"),
  selectInput("x1", "X-axis (for all the plots): ",
              choices = c("Fitted Values", "id"),
              selectize = FALSE, width = "80%"))
    ),
    column(4,
           plotOutput("plot8", height = 250,
                      brush = brushOpts(id = "brP")),
           column(12, offset = 1,
                  selectInput("y8", "Y-axis: ", choices = "",
                              selectize = FALSE, width = "80%"))
    ),
    column(4,
           plotOutput("plot9", height = 250,
                      brush = brushOpts(id = "brP")),
           column(12, offset = 1,
                  selectInput("y9", "Y-axis: ", choices = "",
                              selectize = FALSE, width = "80%"))
    )
  ), # end of plotting row 3
  h3("Selected Cases"),
  tableOutput("brushed"),
  h3("Change in Model Coefficients When Seleced Cases are Erased"),
  tableOutput("modelCompare"),
  fluidRow(
    column(6,
           verbatimTextOutput("full")),
    column(6,
           verbatimTextOutput("reduced"))
  ),
  
  # verbatimTextOutput("info"),
  
  "Version 10/2017. Programmed with R², shiny³ and ggplot2\u2074 by Johann Popp. Please feel free to comment and contribute at", a(target = "_blank", href = "https://github.com/JohannPopp/Outlier-Plot", "https://github.com/JohannPopp/Outlier-Plot"),
  tags$hr(),
  "¹ Field, Andy P. Discovering statistics using SPSS: (and sex, drugs and rock „n“ roll). 2nd ed. London, Thousand Oaks, Calif.: Sage Publications, 2005.", br(),
  "² R Core Team. R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing, 2017. https://www.R-project.org/.
", br(),
  "³ Chang, Winston, Joe Cheng, J. J. Allaire, Yihui Xie, und Jonathan McPherson. shiny: Web Application Framework for R, 2017. https://CRAN.R-project.org/package=shiny.
", br(),
  "\u2074 Wickham, Hadley. Ggplot2 Elegant Graphics for Data Analysis. Dordrecht; New York: Springer, 2009.
"
  
)

# Shiny Server
server <- function(input, output, session){
  
  # Data input
  #########
  
  inData <- reactive({try({
    inFile <- input$inData
    if(is.null(inFile))
      return(NULL)
    if(input$fileType == "Text: csv (seperator = ',', decimal = '.')"){
      read.csv(inFile$datapath)
    } else
      if(input$fileType == "Text: csv2 (sepatator = ';', decimal = ',')"){
        read.csv2(inFile$datapath)
      } else
        if(input$fileType == "Text, other"){
          try(read.table(inFile$datapath, header = input$header, sep = paste(input$sep), dec = input$dec, skip = input$skip, quote = input$quote))
        } else
          if(input$fileType == "SPSS"){
            library(foreign)
            data.frame(read.spss(inFile$datapath))
          }})
  })
  
  # Show data
  output$dataHeader <- renderTable({head(inData(), 3)})
  # Variable names
  output$varNames <- renderText({
    if(class(inData()) == "data.frame"){paste(names(inData()), collapse = ";  ")} else ""
  })
  
  ###################
  # Define starting model
  
  # Dependent variable
  observe({
    validate(
      need(is.data.frame(inData()), label = "Data is not correctly entered")
    )
    updateSelectInput(session, "depVar", choices = names(inData()))
  })
  # Independent variable
  observe({
    validate(
      need(is.data.frame(inData()), label = "Data is not correctly entered")
    )
    updateSelectInput(session, "indepVar", choices = names(inData()))
    
  })
  
  # Model from variable selection
  model1 <- reactive({try(lm(formula = paste(input$depVar, " ~ ", paste(input$indepVar, collapse = " + ")), data = inData()))})
  # Adding custom model formula
  initModel <- reactive({
    input$applyFormula
    
    isolate({
      if(class(model1())[1] == "lm"){
        try(update(model1(), input$initForm))
      } else {
        try(glm(formula = input$initForm, data = inData(), family = binomial))
      }
    })
  })
  
  # Show starting model
  output$initMod <- renderPrint({
    if(input$applyFormula == 0){
      return(NULL)
    }
    initModel()
  })
  
  # Output for testing of interim results
  # output$info <- renderPrint({
  #   rbind(names(outData()), axLabs())
  # })
    
    
  ########
  # Send uploaded model to plot
  
  modPlot <- reactive({
    if(input$modSubmit == 0){
      return(modLM)
    }
    
    isolate({initModel()})
  })
  
  # Example model
  ################
  
  # Data frame from model
  outData <- reactive({
    od <- data.frame(id = as.numeric(rownames(modPlot()$model)), "FittedValues" = modPlot()$fitted.values, stResid = rstandard(modPlot()), absStdResid = abs(rstandard(modPlot())), abs(dfbetas(modPlot())), dfbeta(modPlot()), dffits(modPlot()), abs(dffits(modPlot())), covratio(modPlot()), cooks.distance(modPlot()), hatvalues(modPlot()))
    
    infNames <- c(paste("dfb", names(coef(modPlot())), sep = "."), paste("Z-dfb", names(coef(modPlot())), sep = "."), "dffits", "abs.dffits", "cov.r", "cook.d", "leverage")
    
    names(od)[5:(4+length(infNames))] <- infNames
    
    data.frame(od,  modPlot()$model)
  })
  
  
  # Brushed rows of the data frame per covariate pattern
  brushTable <- reactive({
    brushedPoints(outData(), input$brP)
  })
  
  
  # Labels to plot
  axLabs <- reactive({
    c("id", "Fitted Values", "Standardized Residuals" , "Absolute Std. Residuals", paste("Absolute Std. DFBeta -", names(coef(modPlot()))),paste("DFBeta -", names(coef(modPlot()))), "Standardized DFFit", "Absolute Std. DFFit", "Covariance Ratio", "Cook's Distance", "Leverage",names(modPlot()$model))
  })
  
  # send labels to axis labels
  observe({
    updateSelectInput(session, "y6", 
                      choices = axLabs(), 
                      selected = axLabs()[grep("DFBeta", axLabs())[1]])
    updateSelectInput(session, "y7", 
                      choices = axLabs(), 
                      selected = axLabs()[grep("DFBeta", axLabs())[2]])
    updateSelectInput(session, "y8", 
                      choices = axLabs(), 
                      selected = axLabs()[grep("DFBeta", axLabs())[3]])
    updateSelectInput(session, "y9", 
                      choices = axLabs(), 
                      selected = axLabs()[grep("DFBeta", axLabs())[4]])
    
  })

  #############################
  ### Plots
  
  # Standardizes residuals vs. fitted
  output$plot1 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]

    p1 <- ggplot(data = outData(), aes_string(x = xv, y = "absStdResid"))
    p1 +  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "red") +
      annotate("label", x = min(p1$data[,names(p1$data) == p1$labels$x]),
               y = -0.1,
               label = paste(
                 round(
                   sum(prop.table(table(cut(outData()$stResid, c(-Inf, -1.96, 1.96, Inf))))[c(1,3)]) * 100,
                   2),
                 "% of points >+-1.96 SD", sep = ""),
               hjust = "left", fill = "grey90") +
      annotate("label", x = max(p1$data[,names(p1$data) == p1$labels$x]),
               y = -0.1,
               label = paste(
                 round(
                   sum(prop.table(table(cut(outData()$stResid, c(-Inf, -2.58, 2.58, Inf))))[c(1,3)]) * 100,
                   2),
                 "% of points >+-2.58 SD", sep = ""),
               hjust = "right", fill = "grey90") +
      geom_text(data = brushTable(),
                label = paste("  ", rownames(brushTable()), "  "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      ylim(-0.1, max(outData()$stResid, na.rm = TRUE)) +
      labs(x = input$x1, y = "Absolute Standardized Redsiduals")
  })

  # Leverage vs. fitted
  library(grid)
  output$plot2 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]

    ggplot(data = outData(), aes_string(x = xv, y = "leverage")) +
      geom_hline(yintercept = mean(outData()$leverage, na.rm = TRUE) * c(2,3),
                 linetype = "dashed", colour = c("darkred","red")) +
      geom_text(data = brushTable(),
                label = paste("  ", rownames(brushTable()), "  "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      ylim(0, max(outData()$leverage, na.rm = TRUE)) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~., breaks = mean(outData()$leverage)*c(2,3), labels = c("2mean", "3mean"))) +
      labs(x = input$x1, y = "Leverage")
  })

  # Cook's distance vs. fitted
  output$plot3 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]

    ggplot(data = outData(), aes_string(x = xv, y = "cook.d")) +
      geom_hline(yintercept = 1,
                 linetype = "dashed", colour = "red") +
      geom_text(data = brushTable(),
                label = paste("  ", rownames(brushTable()), "  "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      ylim(0, max(outData()$cook.d, na.rm = TRUE)) +
      labs(x = input$x1, y = "Cook's Distance")
  })

  # Convariance ratio vs. fitted
  output$plot4 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]

    ggplot(data = outData(), aes_string(x = xv, y = "cov.r")) +
      geom_hline(yintercept = 1 + (3 * ncol(modPlot()$model) / nrow(modPlot()$model)) * c(-1, 1),
                 linetype = "dashed", colour = "red") +
      geom_text(data = brushTable(),
                label = paste("  ", rownames(brushTable()), "  "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = "Covariance Ratio")
  })

  # dffit vs. fitted
  output$plot5 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]

    ggplot(data = outData(), aes_string(x = xv, y = "dffits")) +
      geom_hline(yintercept = c(-1,1),
                 linetype = "dashed", colour = "red") +
      geom_text(data = brushTable(),
                label = paste("  ", rownames(brushTable()), "  "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = "Standardized DFFit") +
      ylim(min(outData()$dffits), max(outData()$dffits))
  })
  # 
  # Std-dfbeta-intercept vs. fitted
  output$plot6 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]
    yv <- names(outData())[axLabs() == input$y6]
    yval <- outData()[,yv]

    p6 <- ggplot(data = outData(), aes_string(x = xv, y = yv))
    if(grepl("Absolute Std. DF", input$y6) == TRUE){
      p6 <- p6 + geom_hline(yintercept = 1,
                            linetype = "dashed", colour = "red")
    }
    p6 + geom_text(data = brushTable(),
                   label = paste("  ", rownames(brushTable()), "  "),
                   hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = input$y6) +
      ylim(min(yval), max(yval))
    
  })

  # dfbeta var 1 vs. fitted
  output$plot7 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]
    yv <- names(outData())[axLabs() == input$y7]
    yval <- outData()[,yv]

    p7 <-ggplot(data = outData(), aes_string(x = xv, y = yv))
    if(grepl("Absolute Std. DF", input$y6) == TRUE){
      p7 <- p7 + geom_hline(yintercept = 1,
                            linetype = "dashed", colour = "red")
    }
    p7 + geom_text(data = brushTable(),
                   label = paste("  ", rownames(brushTable()), "  "),
                   hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = input$y7) +
      ylim(min(yval), max(yval))
  })

  # dfbeta var 2 vs. fitted
  output$plot8 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]
    yv <- names(outData())[axLabs() == input$y8]
    yval <- outData()[,yv]

    p8 <- ggplot(data = outData(), aes_string(x = xv, y = yv))
    if(grepl("Absolute Std. DF", input$y6)){
      p8 <- p8 + geom_hline(yintercept = 1,
                            linetype = "dashed", colour = "red")
    }
    p8 + geom_text(data = brushTable(),
                   label = paste("  ", rownames(brushTable()), "  "),
                   hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = input$y8) +
      ylim(min(yval), max(yval))
  })

  # dfbeta var 3 vs. fitted
  output$plot9 <- renderPlot({
    xv <- names(outData())[axLabs() == input$x1]
    yv <- names(outData())[axLabs() == input$y9]
    yval <- outData()[,yv]

    p9 <- ggplot(data = outData(), aes_string(x = xv, y = yv))
    if(grepl("Absolute Std. DF", input$y6) == TRUE){
      p9 <- p9 + geom_hline(yintercept = 1,
                            linetype = "dashed", colour = "red")
    }
    p9 + geom_text(data = brushTable(),
                   label = paste("  ", rownames(brushTable()), "  "),
                   hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.3) +
      geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
      labs(x = input$x1, y = input$y9) +
      ylim(min(yval), max(yval))
  })

  ######
  # Table of brushed cases
  brushTable <- reactive({
    brushedPoints(outData(), input$brP)
  })

  output$brushed <- renderTable(
    brushTable()[,c(1,2,((ncol(brushTable())-ncol(modPlot()$model))+1):ncol(brushTable()))])

  ####
  # compare models

  # Data with brushed excluded
  trimmedData <- reactive({
    modPlot()$model[-brushedPoints(outData(), input$brP)$id,]
  })


  # trimmed model
  tModel <- reactive({
    update(modPlot(), data = trimmedData())
  })

  # Table to compare coefficients
  coefTab <- reactive({
    data.frame("Full data" = coef(modPlot()),
               "p-value.full" = summary(modPlot())$coef[,4],
               "Reduced data" = coef(tModel()),
               "p-value.red" = summary(tModel())$coef[,4],
               "Difference" = coef(modPlot()) - coef(tModel()),
               "Perc.Change" = (coef(modPlot()) - coef(tModel())) * 100 / coef(modPlot()))
  })

  output$modelCompare <- renderTable({
    if(nrow(brushTable()) == 0){
      return()
    }
    coefTab()
  }, rownames = TRUE)

  output$full <- renderPrint({
    summary(modPlot())
  })

  output$reduced <- renderPrint({
    if(nrow(brushTable()) == 0){
      return()
    }
    summary(tModel())
  })
  
}

# Start interactive session
shinyApp(ui, server)
