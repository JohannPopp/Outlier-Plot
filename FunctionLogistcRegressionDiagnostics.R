# Interactive diagnostic plots for outliers in logistic regression
# Reproduces the plots from Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013.

# Johann Popp
# 2017-11-05
#########################################################

# # Load example data
# glow <- aplore3::glow500
# 
# # Recode RATERISK
# glow$raterisk3 <- cut(as.numeric(glow$raterisk), 2)
# levels(glow$raterisk3) <- c("less/same", "greater")
# 
# 
# # Logistic model from Table 4.16
# model <- glm(fracture ~ age + height + priorfrac + momfrac + armassist + raterisk3 + age:priorfrac + momfrac:armassist, data = glow, family = "binomial")

#########################
# Start function
DiagPlotLogistic <- function(model){
  ######Preliminaries
#  library(epiR)
  library(shiny)
  library(ggplot2)
  
  # Function to calculate diagnostic statistics (by covariate pattern)
  logRegDiagn <- function(model){
    # Basic data extraction and calculation
    datN <- model$model
    cp <- unique(datN[,-1])
    
    cpIdN<- apply(datN[,-1], 1, function(x) paste(x, collapse = ""))
    cpIdM <- apply(cp, 1, function(x) paste(x, collapse = ""))
    
    m <- tapply(datN[,1], cpIdN, length)[rank(cpIdM)]
    Y <- tapply(model$y, cpIdN, sum)[rank(cpIdM)]
    Pi <- fitted.values(model)[rownames(cp)]
    
    # Basic residuals
    rPears <- (Y - Pi * m) / sqrt(m * Pi * (1 - Pi))
    
    rDev <- ifelse(Y - m * Pi > 0, 1, -1) * 
      (2 * (Y * log(Y / (m * Pi)) + (m - Y) * log((m - Y) / (m * (1 - Pi)))))^(1/2)
    rDev[Y == 0] <- -sqrt(2 * m[Y == 0] * abs(log(1 - Pi[Y == 0])))
    rDev[Y == m] <- sqrt(2 * m[Y == m] * abs(log(Pi[Y == m])))

    # Diagnostic statistics to plot
    V <- diag(m * Pi * (1 - Pi))
    X <- unique(model.matrix(model))
    H <- V^(1/2) %*% X %*% solve(t(X) %*% V %*% X) %*% t(X) %*% (V^(1/2)) # formula 5.21
    h <- diag(H)

    deltaBeta <- rPears^2 * h / (1 - h)^2  

    deltaChi <- rPears^2 / (1 - h)

    deltaDeviance <- rDev^2 / (1 - h)

    # collect output data
    out <- data.frame(m, Y, Pi, rPears, rDev, h, deltaBeta, deltaChi, deltaDeviance, cpIdM, cp)
    rownames(out) <- rownames(cp)
    out
  }

  ###############################
  ## ui
  ui <- fluidPage(
    h2("Logistic Regression Diagnostics"),
    tags$p("These are plots recommended by Hosmer et al 2013¹ to identify extreme and influential covariate patterns in a logistic regression model. Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected covariate patterns and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end never just exclude data just because it does not fit your model."),
    fluidRow(
      column(4,
             plotOutput("plot1", height = 300,
                        brush = brushOpts(id = "brP")),
             plotOutput("plot4", height = 300,
                        brush = brushOpts(id = "brP"))),
      column(4,
             plotOutput("plot2", height = 300,
                        brush = brushOpts(id = "brP")),
             plotOutput("plot5", height = 300,
                        brush = brushOpts(id = "brP"))),
      column(4,
             plotOutput("plot3", height = 300,
                        brush = brushOpts(id = "brP")),
             h4("Selected rows in (casewise) input data:"),
             verbatimTextOutput("selectedCases"))
    ),
    h3("Selected covariate patterns"),
    tableOutput("brushed"),
    "m = Sample size of the covariate pattern; Y = Number of outcomes in the covariate pattern; Pi = Estimated probability of the outcome", tags$br(), tags$br(),
    h3("Change in model coefficients when seleced covariate patterns are erased"),
    tableOutput("modelCompare"),
    fluidRow(
      column(6,
             verbatimTextOutput("full")),
      column(6,
            verbatimTextOutput("reduced"))
    )
  ,
  "¹ Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd. ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013."
  )
  
  
  ## server
  server <- function(input, output){

    #################
    # Calculate diagnostic statistics
    #################
    
    dat <- logRegDiagn(model)
  
    
  
    ####################
    # plots
    ##################

    # Leverage
    output$plot1 <- renderPlot(
      ggplot(data = dat, aes(x = Pi, y = h)) +
        geom_text(data = brushTable(), 
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTable(), 
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Leverage")
     )
    
    # Change in Pearson chi-squre

    output$plot2 <- renderPlot(
      ggplot(data = dat, aes(x = Pi, y = deltaChi)) +
        geom_text(data = brushTable(),
                  label = paste("  ", rownames(brushTable()), "  "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTable(),
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Change in Pearson chi-square") +
        annotate("text", x = mean(c(min(dat$Pi, na.rm = TRUE), max(dat$Pi, na.rm = TRUE))),
                 y = max(dat$deltaChi, na.rm = TRUE) * 0.9,
                 label = paste(
                   round(
                     prop.table(table(
                       cut(dat$deltaChi, c(0, 3.84, 10000))
                     ))[2]*100,
                     2),
                   "% of points >3.84", sep = "")
        )
    )

    

    # Change in deviance  
    output$plot3 <- renderPlot({
      ggplot(data = dat, aes(x = Pi, y = deltaDeviance)) +
        geom_text(data = brushTable(), 
                  label = paste("  ", rownames(brushTable()), "  "), 
                  hjust = "inward", colour = "darkblue", size = 3) +        
        geom_point(alpha = 0.5) +
        geom_point(data = brushTable(), 
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Change in deviance") +
        annotate("text", x = mean(c(min(dat$Pi), max(dat$Pi))),
                 y = max(dat$deltaDeviance, na.rm = TRUE) * 0.9,
                 label = paste(
                   round(
                     prop.table(table(
                       cut(dat$deltaDeviance, c(0, 3.84, 10000))
                     ))[2]*100, 
                     2), 
                   "% of points >3.84", sep = ""))
    })
   
    # Cooks distance  
    output$plot4 <- renderPlot(
      ggplot(data = dat, aes(x = Pi, y = deltaBeta)) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
        geom_text(data = brushTable(), 
                  label = paste("  ", rownames(brushTable()), "  "), 
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = dat[dat$deltaBeta > 1,], colour = "red") +
        geom_point(data = brushTable(), 
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        ylim(0, max(dat$deltaBeta)) +
        labs(x = "Estimated probability", y = "Cook's distance")
      
    )
    
    # Pearson chi-square with Cooks distance for size
    output$plot5 <- renderPlot(
      ggplot(data = dat, aes(x = Pi, y = deltaChi, size = deltaBeta)) +
        scale_size_area(max_size = 20) +
        geom_point(shape = 21) +
        geom_point(data = dat[dat$deltaBeta > 1,], shape = 21, colour = "red") +
        geom_point(data = brushTable(), shape = 21, fill = "blue") +
        geom_text(data = brushTable(), 
                  label = rownames(brushTable()), 
                  colour = "black", size = 3) +
        labs(x = "Estimated probability", y = "Change in Pearson chi-square") +
        annotate("text", x = mean(c(min(dat$Pi), max(dat$Pi))), y = max(dat$deltaChi)*0.9, label = "Size = Cook's distance") +
        guides(size = FALSE)
    )
    
   # Brushed rows of the data frame per covariate pattern
    brushTable <- reactive({
      brushedPoints(dat, input$brP)
    })
  
   output$brushed <- renderTable(
     brushTable()[,-c(4:10)], rownames = TRUE)
   
   # Extracting single cases 
   cpIdN <- apply(model$model[,-1], 1, function(x) paste(x, collapse = ""))
   selected <- reactive({
     is.element(cpIdN, brushTable()$cpIdM)
   })

   output$selectedCases <- renderPrint({
     rownames(model$model[selected(),])
   })

   # Data with brushed excluded
   trimmedData <- reactive({
     model$model[selected() == FALSE,]
   })
   
   # trimmed model
   tModel <- reactive({
     update(model, data = trimmedData())
   }) 
   
   # Table to compare coefficients
   coefTab <- reactive({ 
     data.frame("Full data" = coef(model), 
                "Reduced data" = coef(tModel()),
                "Difference" = coef(tModel()) - coef(model),
                "Perc.Change" = (coef(tModel()) - coef(model)) * 100 / coef(model))
     })
   
   output$modelCompare <- renderTable({
     coefTab()
   }, rownames = TRUE)
   
   output$full <- renderPrint({
     summary(model)
   })
    
   output$reduced <- renderPrint({
     summary(tModel())
   })
  }
  
  shinyApp(ui = ui, server = server)
#  summary(model)

  
}

DiagPlotLogistic(model)



