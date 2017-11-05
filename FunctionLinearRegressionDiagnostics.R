# Interactive diagnostic plots for outliers in linear regression based on recomendations of
# Field, Andy P. Discovering statistics using SPSS: (and sex, drugs and rock „n“ roll). 2. Aufl. London, Thousand Oaks, Calif.: Sage Publications, 2005.


# Johann Popp
# 2017-11-05
#########################################################

# Load example data (Record sales)
# dat <- read.delim("https://studysites.uk.sagepub.com/dsur/study/DSUR%20Data%20Files/Chapter%207/Album%20Sales%202.dat")

# Build example model
# model <- lm(sales ~ adverts + airplay + attract, data = dat)





DiagPlotLM <- function(model){
  ########
  # Preliminaries
  
  library(shiny)
  library(ggplot2)
  
  # Labels to plot
  axLabs <- c("id", "Fitted Values", "Standardized Residuals" , "Absolute Std. Residuals", paste("Absolute Std. DFBeta -", names(coef(model))),paste("DFBeta -", names(coef(model))), "Standardized DFFit", "Absolute Std. DFFit", "Covariance Ratio", "Cook's Distance", "Leverage",names(model$model))
  
  # Extract data frame from the model and add diagnostic statistics
  outData <- data.frame(id = as.numeric(rownames(model$model)), "FittedValues" = model$fitted.values, stResid = rstandard(model), absStdResid = abs(rstandard(model)), abs(dfbetas(model)), dfbeta(model), dffits(model), abs(dffits(model)), covratio(model), cooks.distance(model), hatvalues(model))
  
  infNames <- c(paste("dfb", names(coef(model)), sep = "."), paste("Z-dfb", names(coef(model)), sep = "."), "dffits", "abs.dffits", "cov.r", "cook.d", "leverage")
  names(outData)[5:(4+length(infNames))] <- infNames
  
  outData <- data.frame(outData,  model$model)
  
  ###########################
  # Shiny User Interface
  ###########################
  ui <- fluidPage(
    # Setting selecect headings in line.
    tags$head(
      tags$style(type="text/css",
                 "label.control-label,
               .selectize-control.single{ display: table-cell;}
               .form-group { display: table-row;}")
    ),
    
    h1("Outlier Diagnostics for Linear Regression"),
    # Introduction
    
    p("With these plots you can search for and investigate extreme and influential cases in a linear regression model as recommended by Field 2005¹. Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected cases and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end", tags$strong("never exclude data just because it does not fit your model.")),
    p("Critical thresholds are marked with lines:",
      tags$ul(
        tags$li("Standardized residuals: +-3 standard deviations"),
        tags$li("Leverage: 2 and 3 times the mean leverage"),
        tags$li("Cook's distance: 1"),
        tags$li("Covariance ratio: 1 +-[3(p)/n]  (p = number of model parameters; n = sampe size )"),
        tags$li("Standardized DFBetas & DFFit: 1")
      )),
    tags$hr(),
    
    
    # Show plots
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
                    selectInput("y6", "Y-axis: ", choices = axLabs, 
                                selected = axLabs[grep("DFBeta", axLabs)[1]], 
                                selectize = FALSE, width = "80%"))
      )
    ), # end of plotting fluid row 1 +2
    fluidRow(
      column(4,
             plotOutput("plot7", height = 250,
                        brush = brushOpts(id = "brP")),
             column(12, offset = 1,
                    selectInput("y7", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[2]],
                                selectize = FALSE, width = "80%"),
                    selectInput("x1", "X-axis (for all the plots): ", 
                                choices = c("Fitted Values", "id"),
                                selectize = FALSE, width = "80%"))
      ),
      column(4,
             plotOutput("plot8", height = 250,
                        brush = brushOpts(id = "brP")),
             column(12, offset = 1,
                    selectInput("y8", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[3]],
                                selectize = FALSE, width = "80%"))
      ),
      column(4,
             plotOutput("plot9", height = 250,
                        brush = brushOpts(id = "brP")),
             column(12, offset = 1,
                    selectInput("y9", "Y-axis: ", choices = axLabs,
                                selected = axLabs[grep("DFBeta", axLabs)[4]],
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
    
    verbatimTextOutput("info"),
    
    "Version 10/2017. Programmed with R², shiny³ and ggplot2\u2074 by Johann Popp. Please feel free to comment and contribute at ", a(href = "https://github.com/JohannPopp/Outlier-Plot/issues", "https://github.com/JohannPopp/Outlier-Plot/issues", target = "_blank"), "or send an e-mail to", a(href="mailto:johann.popp@haw-hamburg.de", "johann.popp@haw-hamburg.de"),
    tags$hr(),
    "¹ Field, Andy P. Discovering statistics using SPSS: (and sex, drugs and rock „n“ roll). 2nd ed. London, Thousand Oaks, Calif.: Sage Publications, 2005.", br(),
    "² R Core Team. R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing, 2017. https://www.R-project.org/.
             ", br(),
    "³ Chang, Winston, Joe Cheng, J. J. Allaire, Yihui Xie, und Jonathan McPherson. shiny: Web Application Framework for R, 2017. https://CRAN.R-project.org/package=shiny.
             ", br(),
    "\u2074 Wickham, Hadley. Ggplot2 Elegant Graphics for Data Analysis. Dordrecht; New York: Springer, 2009.
             "
  )
  
  #############################
  # Shiny Server
  #############################
  server <- function(input, output, session){
    
    
    
    
    # Brushed rows of the data frame per covariate pattern
    brushTable <- reactive({
      brushedPoints(outData, input$brP)
    })
    
    ### Plots
    
    # Standardizes residuals vs. fitted
    output$plot1 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      
      p1 <- ggplot(data = outData, aes_string(x = xv, y = "stResid"))
      p1 +  geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "red") +
        annotate("label", x = min(p1$data[,names(p1$data) == p1$labels$x]),
                 y = min(outData$stResid)*1.3,
                 label = paste(
                   round(
                     sum(prop.table(table(cut(outData$stResid, c(-Inf, -1.96, 1.96, Inf))))[c(1,3)]) * 100,
                     2),
                   "% of points >+-1.96 SD", sep = ""),
                 hjust = "left", fill = "grey90") +
        annotate("label", x = max(p1$data[,names(p1$data) == p1$labels$x]),
                 y = min(outData$stResid)*1.3,
                 label = paste(
                   round(
                     sum(prop.table(table(cut(outData$stResid, c(-Inf, -2.58, 2.58, Inf))))[c(1,3)]) * 100,
                     2),
                   "% of points >+-2.58 SD", sep = ""),
                 hjust = "right", fill = "grey90") +
        geom_text(data = brushTable(), 
                  label = paste("  ", rownames(brushTable()), "  "), 
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.3) +
        geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ylim(min(outData$stResid, na.rm = TRUE)*1.3, max(outData$stResid, na.rm = TRUE)) + 
        labs(x = input$x1, y = "Standardized Redsiduals")
    })
    
    # Leverage vs. fitted
    library(grid)
    output$plot2 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      
      ggplot(data = outData, aes_string(x = xv, y = "leverage")) +
        geom_hline(yintercept = mean(outData$leverage, na.rm = TRUE) * c(2,3),
                   linetype = "dashed", colour = c("darkred","red")) +
        geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.3) +
        geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ylim(0, max(outData$leverage, na.rm = TRUE)) +
        scale_y_continuous(sec.axis = sec_axis(trans = ~., breaks = mean(outData$leverage)*c(2,3), labels = c("2mean", "3mean"))) +
        labs(x = input$x1, y = "Leverage")
    })
    
    # Cook's distance vs. fitted
    output$plot3 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      
      ggplot(data = outData, aes_string(x = xv, y = "cook.d")) +
        geom_hline(yintercept = 1,
                   linetype = "dashed", colour = "red") +
        geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.3) +
        geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        ylim(0, max(outData$cook.d, na.rm = TRUE)) +
        labs(x = input$x1, y = "Cook's Distance")
    })
    
    # Convariance ratio vs. fitted
    output$plot4 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      
      ggplot(data = outData, aes_string(x = xv, y = "cov.r")) +
        geom_hline(yintercept = 1 + (3 * ncol(model$model) / nrow(model$model)) * c(-1, 1),
                   linetype = "dashed", colour = "red") +
        geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.3) +
        geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        #      ylim(min(outData$cov.r), max(outData$cov.r)) +
        labs(x = input$x1, y = "Covariance Ratio")
    })
    
    # dffit vs. fitted
    output$plot5 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      
      ggplot(data = outData, aes_string(x = xv, y = "dffits")) +
        geom_hline(yintercept = c(-1,1),
                   linetype = "dashed", colour = "red") +
        geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.3) +
        geom_point(data = brushTable(), shape = 21, fill = "blue", size = 3) +
        labs(x = input$x1, y = "Standardized DFFit") +
        ylim(min(outData$dffits), max(outData$dffits))
    })
    
    # Std-dfbeta-intercept vs. fitted
    output$plot6 <- renderPlot({
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y6]
      yval <- outData[,yv]
      
      p6 <- ggplot(data = outData, aes_string(x = xv, y = yv))
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
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y7]
      yval <- outData[,yv]
      
      p7 <-ggplot(data = outData, aes_string(x = xv, y = yv))
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
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y8]
      yval <- outData[,yv]
      
      p8 <- ggplot(data = outData, aes_string(x = xv, y = yv))
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
      xv <- names(outData)[axLabs == input$x1]
      yv <- names(outData)[axLabs == input$y9]
      yval <- outData[,yv]
      
      p9 <- ggplot(data = outData, aes_string(x = xv, y = yv))
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
      brushedPoints(outData, input$brP)
    })
    
    output$brushed <- renderTable(
      brushTable()[,c(1,2,((ncol(brushTable())-ncol(model$model))+1):ncol(brushTable()))])
    
    ####
    # compare models
    
    # Data with brushed excluded
    trimmedData <- reactive({
      model$model[-brushedPoints(outData, input$brP)$id,]
    })
    
    
    # trimmed model
    tModel <- reactive({
      update(model, data = trimmedData())
    })
    
    # Table to compare coefficients
    coefTab <- reactive({ 
      data.frame("Full data" = coef(model), 
                 "p-value.full" = summary(model)$coef[,4],
                 "Reduced data" = coef(tModel()),
                 "p-value.red" = summary(tModel())$coef[,4],
                 "Difference" = coef(model) - coef(tModel()),
                 "Perc.Change" = (coef(model) - coef(tModel())) * 100 / coef(model))
    })
    
    output$modelCompare <- renderTable({
      if(nrow(brushTable()) == 0){
        return()
      }
      coefTab()
    }, rownames = TRUE)
    
    output$full <- renderPrint({
      summary(model)
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
  
}

# DiagPlotLM(model)
