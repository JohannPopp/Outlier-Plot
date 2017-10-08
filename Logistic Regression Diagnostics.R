

# Interactive diagnostic plots for outliers in logistic regression
# Reproduces the plots from Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013.

# Johann Popp
# 2017-09-07
#########################################################

# Load example data
glow <- aplore3::glow500

# Recode RATERISK
glow$raterisk3 <- cut(as.numeric(glow$raterisk), 2)
levels(glow$raterisk3) <- c("less/same", "greater")


# Logistic model from Table 4.16
model <- glm(fracture ~ age + height + priorfrac + momfrac + armassist + raterisk3 + age:priorfrac + momfrac:armassist, data = glow, family = "binomial")

#########################
# Start function

library(epiR)
library(shiny)
library(ggplot2)
## ui
ui <- fluidPage(
  h2("Logistic Regression Diagnostics"),
  tags$p("These are plots recommended by Hosmer et al 2013¹ to identify extreme and influential covariate patterns in a logistic regression model. Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected covariate patterns and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end never just exclude data just because it does not fit your model."),
  tags$p(tags$strong("Attention:"), "The plots differ from those printed in the book that are produced with STATA. It seems to have something to do with the calculation of leverage on the level of covariate patterns. You can find a detailed description of this problem at", tags$a(href = "https://github.com/JohannPopp/Outlier-Plot/blob/master/diganosticPlotInconsistency.pdf", "https://github.com/JohannPopp/Outlier-Plot/blob/master/diganosticPlotInconsistency.pdf"), "."),
  tags$p("You can download this code as an R-function to apply it to your logistic regression model: ", tags$a(href = "https://github.com/JohannPopp/Outlier-Plot", "https://github.com/JohannPopp/Outlier-Plot"), "."),
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
  
  #############
  # Convert to covariate patterns
  ###############
  
  # aggregate to covariate pattern
  cp <- epi.cp(model.frame(model)[-1])
  
  # Number of outcome events per covariate pattern
  obs <- as.vector(by(as.numeric(as.factor(model$model[,1]))-1, as.factor(cp$id), sum))
  
  # Estimated outcome probability per covariate pattern
  fit <- as.vector(by(model$fitted.values, as.factor(cp$id), min))
  
  # Calculate residuals and influence measures per covariate pattern
  res <- epi.cpresids(obs, fit, cp)
  
  # Calculate Change in deviance
  deltaDeviance <- res$deviance^2 + ((res$pearson^2*res$leverage)/(1 - res$leverage))
  
  # Put it together in one data.frame
  dat <- data.frame(res, deltaDeviance, fit, cp$cov.pattern)
  
  
  
  ####################
  # plots
  ##################
  
  # Leverage
  output$plot1 <- renderPlot(
    ggplot(data = dat, aes(x = fit, y = leverage)) +
      geom_text(data = brushTable(), 
                label = paste(" ", brushTable()$cpid, " "), 
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.5) +
      geom_point(data = brushTable(), 
                 shape = 21, size = 2.5, fill = "blue", alpha = 1) +
      labs(x = "Estimated probability", y = "Leverage")
  )
  
  # Change in Pearson chi-squre
  
  output$plot2 <- renderPlot(
    ggplot(data = dat, aes(x = fit, y = deltachi)) +
      geom_text(data = brushTable(),
                label = paste(" ", brushTable()$cpid, " "),
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.5) +
      geom_point(data = brushTable(),
                 shape = 21, size = 2.5, fill = "blue", alpha = 1) +
      labs(x = "Estimated probability", y = "Change in Pearson chi-square") +
      annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
               y = max(dat$deltachi, na.rm = TRUE) * 0.9,
               label = paste(
                 round(
                   prop.table(table(
                     cut(dat$deltachi, c(0, 3.84, 10000))
                   ))[2]*100,
                   2),
                 "% of points >3.84", sep = "")
      )
  )
  
  
  
  # Change in deviance  
  output$plot3 <- renderPlot({
    ggplot(data = dat, aes(x = fit, y = deltaDeviance)) +
      geom_text(data = brushTable(), 
                label = paste(" ", brushTable()$cpid, " "), 
                hjust = "inward", colour = "darkblue", size = 3) +        
      geom_point(alpha = 0.5) +
      geom_point(data = brushTable(), 
                 shape = 21, size = 2.5, fill = "blue", alpha = 1) +
      labs(x = "Estimated probability", y = "Change in deviance") +
      annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
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
    ggplot(data = dat, aes(x = fit, y = deltabeta)) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
      geom_text(data = brushTable(), 
                label = paste(" ", brushTable()$cpid, " "), 
                hjust = "inward", colour = "darkblue", size = 3) +
      geom_point(alpha = 0.5) +
      geom_point(data = dat[dat$deltabeta > 1,], colour = "red") +
      geom_point(data = brushTable(), 
                 shape = 21, size = 2.5, fill = "blue", alpha = 1) +
      ylim(0, max(dat$deltabeta, na.rm = TRUE)) +
      labs(x = "Estimated probability", y = "Cook's distance")
    
  )
  
  # Pearson chi-square with Cooks distance for size
  output$plot5 <- renderPlot(
    ggplot(data = dat, aes(x = fit, y = deltachi, size = deltabeta)) +
      scale_size_area(max_size = 20) +
      geom_point(shape = 21) +
      geom_point(data = dat[dat$deltabeta > 1,], shape = 21, colour = "red") +
      geom_point(data = brushTable(), shape = 21, fill = "blue") +
      geom_text(data = brushTable(), 
                label = brushTable()$cpid, 
                colour = "black", size = 3) +
      labs(x = "Estimated probability", y = "Change in Pearson chi-square") +
      annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))), y = max(dat$deltachi, na.rm = TRUE)*0.9, label = "Size = Cook's distance") +
      guides(size = FALSE)
  )
  
  # Brushed rows of the data frame per covariate pattern
  brushTable <- reactive({
    brushedPoints(dat, input$brP)
  })
  
  output$brushed <- renderTable(
    brushTable()[,-c(5:17)])
  
  # Extracting single cases 
  selected <- reactive({
    is.element(cp$id, brushTable()$cpid)
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
               "p-value.full" = summary(model)$coef[,4],
               "Reduced data" = coef(tModel()),
               "p-value.red" = summary(tModel())$coef[,4],
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








