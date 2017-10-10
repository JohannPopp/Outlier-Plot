# Interactive diagnostic plots for outliers in logistic regression
# Reproduces the plots from Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013.

# Johann Popp
# 2017-10-08
#########################################################

# Load example data
glow <- aplore3::glow500

# Recode RATERISK
glow$raterisk3 <- cut(as.numeric(glow$raterisk), 2)
levels(glow$raterisk3) <- c("less/same", "greater")


# Logistic model from Table 4.16
model <- glm(fracture ~ age + height + priorfrac + momfrac + armassist + raterisk3 + age:priorfrac + momfrac:armassist, data = glow, family = "binomial")

#############
# Fast version of epiR::epi.cp
popp.cp <- function(dat){
  
  dat <- data.frame(id = 1:nrow(dat), dat)
  
  # add an indicator variable for covariate patterns
  dat$indi <- apply(dat[,ncol(dat):2], 1, function(x) as.factor(paste(x, collapse = "")))
  
  # order according to the indicator variable
  dat <- dat[order(dat$indi),]
  
  # creating a variable that indicates all the cases of each covariate pattern
  cp.id <- tapply(dat$id, dat$indi, function(x) paste(x, collapse = ","))
  
  n <- as.numeric(unlist(lapply(strsplit(cp.id, ","), length)))
  # obs <- tapply(testMod$model[,1], dat$indi, sum)
  
  
  # Creating a data.frame of covariate patterns
  cp <- unique(dat[,2:ncol(dat)])
  n <- as.numeric(unlist(lapply(strsplit(cp.id, ","), length)))
  id <- tapply(dat$id, dat$indi, function(x) (x)[1])
  cov.pattern <- data.frame(id, n, cp[,-ncol(cp)])
  rownames(cov.pattern) <- rownames(cp)
  ## Create a vector with the covariate pattern for each case
  id <- as.numeric(unlist(lapply(strsplit(cp.id, ","), function(x) rep(min(as.numeric(unlist(x))), length(x)))))[order(dat$id)]
  
  list(cov.pattern = cov.pattern, id = id)
  
}

#########################

  library(epiR)
  library(shiny)
  library(ggplot2)
  ## ui
ui <- fluidPage(h1("Logistic Regression Diagnostics"),
  tabsetPanel(
  tabPanel("Example Data",
           
           tags$p("These are plots recommended by Hosmer et al 2013¹ to identify extreme and influential covariate patterns in a logistic regression model. Inspect points that are way out of line of the others. Mark them by brushing (hold left mouse button and move the pointer). This will show you the variable values of the selected covariate patterns and highlight them in all the other plots. Try to find out what makes these covariate patterns so special. Perhaps they are data entry errors or they are an important combination of variables that needs to be considered by an interaction in the model. See what happens to the model if you would exclude them. But in the end never just exclude data just because it does not fit your model."),
           tags$p(tags$strong("Attention:"), "The plots differ from those printed in the book that are produced with STATA. It seems to have something to do with the calculation of leverage on the level of covariate patterns. You can find a detailed description of this problem at", tags$a(target = "_blank", href = "https://github.com/JohannPopp/Outlier-Plot/blob/master/diganosticPlotInconsistency.pdf", "https://github.com/JohannPopp/Outlier-Plot/blob/master/diganosticPlotInconsistency.pdf"), "."),
           tags$p("You can download this code as an R-function to apply it to your logistic regression model: ", tags$a(target = "_blank", href = "https://github.com/JohannPopp/Outlier-Plot", "https://github.com/JohannPopp/Outlier-Plot"), "."),
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
           h3("Selected Covariate Patterns"),
           tableOutput("brushed"),
           h3("Change in Model Coefficients When Seleced Covariate Patterns are Erased"),
           tableOutput("modelCompare"),
           fluidRow(
             column(6,
                    verbatimTextOutput("full")),
             column(6,
                    verbatimTextOutput("reduced"))
           )
           ,
           "¹ Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd. ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013."
  ),
  
  tabPanel("Enter Your Own Data and Model",
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
           tags$p("Names of available Variables"),
           textOutput("varNames"),
           
             fluidRow(tags$h4("Enter Model Formula")),
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
#           verbatimTextOutput("info"),
           actionButton("modSubmit", "Plot Diagnostics of this Model"),
           fluidRow(
             column(4,
                    plotOutput("plot1C", height = 300,
                               brush = brushOpts(id = "brPC")),
                    plotOutput("plot4C", height = 300,
                               brush = brushOpts(id = "brPC"))),
             column(4,
                    plotOutput("plot2C", height = 300,
                               brush = brushOpts(id = "brPC")),
                    plotOutput("plot5C", height = 300,
                               brush = brushOpts(id = "brPC"))),
             column(4,
                    plotOutput("plot3C", height = 300,
                               brush = brushOpts(id = "brPC")),
                    h4("Selected rows in (casewise) input data:"),
                    verbatimTextOutput("selectedCasesC"))
           ),
           h3("Brushed Covariate Patterns"),
           tableOutput("brushedC"),
           h3("Change in Model Coefficients When Seleced Covariate Patterns are Erased"),
           tableOutput("modelCompareC"),
  fluidRow(
    column(6,{
      verbatimTextOutput("fullC")
    }),
    column(6,{
      verbatimTextOutput("reducedC")
    })
  )
            ) # end tab panel
)
)
  
  
  ## server
  server <- function(input, output, session){
    
    # Handle input data
    
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
    model1 <- reactive({try(glm(formula = paste(input$depVar, " ~ ", paste(input$indepVar, collapse = " + ")), data = inData(), family = binomial))})
    # Adding custom model formula
    initModel <- reactive({
      input$applyFormula
      
      isolate({
        if(class(model1())[1] == "glm"){
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
      cat(paste("MODEL FORMULA:", initModel()$formula[2], initModel()$formula[1], initModel()$formula[3]))
            })

     output$info <- renderPrint({
       head(datC())
         })
    

     
    ########
    # Send uploaded model to plot
     
    modelC <- reactive({
      input$modSubmit
      
      isolate({initModel()})
    })
   
     
    ##################################################
    # Example Data
    ##################################################  
    #############
    # Convert to covariate patterns
    ###############

    # aggregate to covariate pattern
    cp <- popp.cp(model.frame(model)[-1])
    
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
    output$plot1 <- renderPlot({
            ggplot(data = dat, aes(x = fit, y = leverage)) +
        geom_text(data = brushTable(), 
                  label = paste(" ", brushTable()$cpid, " "), 
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTable(), 
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Leverage")
    })
    
    # Change in Pearson chi-squre
    
    output$plot2 <- renderPlot(
      ggplot(data = dat, aes(x = fit, y = deltachi)) +
        geom_text(data = brushTable(),
                  label = paste(" ", brushTable()$cpid, " "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTable(),
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Change in Pearson chi-square") #+
        # annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
        #          y = max(dat$deltachi, na.rm = TRUE) * 0.9,
        #          label = paste(
        #            round(
        #              prop.table(table(
        #                cut(dat$deltachi, c(0, 3.84, 10000))
        #              ))[2]*100,
        #              2),
        #            "% of points >3.84", sep = "")
        #)
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
        labs(x = "Estimated probability", y = "Change in deviance") #+
        # annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
        #          y = max(dat$deltaDeviance, na.rm = TRUE) * 0.9,
        #          label = paste(
        #            round(
        #              prop.table(table(
        #                cut(dat$deltaDeviance, c(0, 3.84, 10000))
        #              ))[2]*100, 
        #              2), 
        #            "% of points >3.84", sep = ""))
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
    

    
    #######################################
    # Custom data
    #########################################
    
    # Convert to covariate pattern with diagnostic statistics
    # aggregate to covariate pattern
    cpC <- reactive({
      withProgress(message="It may take some time to draw the plots ...", {
      popp.cp(model.frame(modelC())[-1])
      })})
    
    # Number of outcome events per covariate pattern
    obsC <- reactive({
      as.vector(by(as.numeric(as.factor(modelC()$model[,1]))-1, as.factor(cpC()$id), sum))
      })
    
    # Estimated outcome probability per covariate pattern
    fitC <- reactive({
      as.vector(by(modelC()$fitted.values, as.factor(cpC()$id), min))
      })
    
    # Calculate residuals and influence measures per covariate pattern
    resC <- reactive({
      epi.cpresids(obsC(), fitC(), cpC())
      })
    
    # Calculate Change in deviance
    deltaDevianceC <- reactive({
      resC()$deviance^2 + ((resC()$pearson^2*resC()$leverage)/(1 - resC()$leverage))
      })
    
    # Put it together in one data.frame
    datC <- reactive({
      data.frame(resC(), deltaDeviance = deltaDevianceC(), fit = fitC(), cpC()$cov.pattern)
      })
    # library(LogisticDx)
    # datC <- reactive({
    #   cptab <- dx(modelC())
    #   data.frame(cpid = rownames(cptab), cptab[,-1])
    #   
    # })

    
    # plots
    ##################
    
    # Leverage
    output$plot1C <- renderPlot({
      if(input$modSubmit == 0){
        return()
      }
      ggplot(data = datC(), aes(x = fit, y = leverage)) +
        geom_text(data = brushTableC(),
                  label = paste(" ", brushTableC()$cpid, " "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTableC(),
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Leverage")
    })

    # Change in Pearson chi-squre

    output$plot2C <- renderPlot({
      if(input$modSubmit == 0){
        return()
      }

      ggplot(data = datC(), aes(x = fit, y = deltachi)) +
        geom_text(data = brushTableC(),
                  label = paste(" ", brushTableC()$cpid, " "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTableC(),
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Change in Pearson chi-square") #+
        # annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
        #          y = max(dat$deltachi, na.rm = TRUE) * 0.9,
        #          label = paste(
        #            round(
        #              prop.table(table(
        #                cut(dat$deltachi, c(0, 3.84, 10000))
        #              ))[2]*100,
        #              2),
        #            "% of points >3.84", sep = "")
        # )
    })

    # Change in deviance
    output$plot3C <- renderPlot({
      if(input$modSubmit == 0){
        return()
      }

      ggplot(data = datC(), aes(x = fit, y = deltaDeviance)) +
        geom_text(data = brushTableC(),
                  label = paste(" ", brushTableC()$cpid, " "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = brushTableC(),
                   shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        labs(x = "Estimated probability", y = "Change in deviance") #+
        # annotate("text", x = mean(c(min(dat$fit, na.rm = TRUE), max(dat$fit, na.rm = TRUE))),
        #          y = max(dat$deltaDeviance, na.rm = TRUE) * 0.9,
        #          label = paste(
        #            round(
        #              prop.table(table(
        #                cut(dat$deltaDeviance, c(0, 3.84, 10000))
        #              ))[2]*100,
        #              2),
        #            "% of points >3.84", sep = ""))
    })

    # Cooks distance
    output$plot4C <- renderPlot({
      if(input$modSubmit == 0){
        return()
      }
      ggplot(data = datC(), aes(x = fit, y = deltabeta)) +
        geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
        geom_text(data = brushTableC(),
                  label = paste(" ", brushTableC()$cpid, " "),
                  hjust = "inward", colour = "darkblue", size = 3) +
        geom_point(alpha = 0.5) +
        geom_point(data = datC()[datC()$deltabeta > 1,], colour = "red") +
        geom_point(data = brushTableC(),
                  shape = 21, size = 2.5, fill = "blue", alpha = 1) +
        ylim(0, max(datC()$deltabeta, na.rm = TRUE)) +
        labs(x = "Estimated probability", y = "Cook's distance")

    })

    # Pearson chi-square with Cooks distance for size
    output$plot5C <- renderPlot({
      if(input$modSubmit == 0){
        return()
      }
      ggplot(data = datC(), aes(x = fit, y = deltachi, size = deltabeta)) +
        scale_size_area(max_size = 20) +
        geom_point(shape = 21) +
        geom_point(data = datC()[datC()$deltabeta > 1,], shape = 21, colour = "red") +
        geom_point(data = brushTableC(), shape = 21, fill = "blue") +
        geom_text(data = brushTableC(),
                  label = brushTableC()$cpid,
                  colour = "black", size = 3) +
        labs(x = "Estimated probability", y = "Change in Pearson chi-square") +
        annotate("text", x = mean(c(min(datC()$fit, na.rm = TRUE), max(datC()$fit, na.rm = TRUE))), y = max(datC()$deltachi, na.rm = TRUE)*0.9, label = "Size = Cook's distance") +
        guides(size = FALSE)
    })
    
    # Brushed rows of the data frame per covariate pattern
    brushTableC <- reactive({
      brushedPoints(datC(), input$brPC)
    })
    
    output$brushedC <- renderTable({
      if(input$modSubmit == 0){
        return()
      }
      brushTableC()[,-c(5:17)]
      })
    
    # Extracting single cases 
    selectedC <- reactive({
      is.element(cpC()$id, brushTableC()$cpid)
    })
    output$selectedCasesC <- renderPrint({
      if(input$modSubmit == 0){
        return()
      }
      rownames(modelC()$model[selectedC(),])
    })
    
    
    # Data with brushed excluded
    trimmedDataC <- reactive({
      modelC()$data[rownames(modelC()$model)[selectedC() == FALSE],]
    })
    
    # trimmed model
    tModelC <- reactive({
      update(modelC(), data = trimmedDataC())
    }) 
    
    # Table to compare coefficients
    coefTabC <- reactive({ 
      data.frame("Full data" = coef(modelC()), 
                 "p-value.full" = summary(modelC())$coef[,4],
                 "Reduced data" = coef(tModelC()),
                 "p-value.red" = summary(tModelC())$coef[,4],
                 "Difference" = coef(tModelC()) - coef(modelC()),
                 "Perc.Change" = (coef(tModelC()) - coef(modelC())) * 100 / coef(modelC()))
    })
    
    output$modelCompareC <- renderTable({
      if(input$modSubmit == 0){
        return()
      }
      coefTabC()
    }, rownames = TRUE)
 
    
    
    # Print full and reduced model   
    output$fullC <- renderPrint({
      if(input$modSubmit == 0){
        return()
      }
      summary(modelC())
    })
    
    output$reducedC <- renderPrint({
      if(input$modSubmit == 0){
        return()
      }
      summary(tModelC())
    })
  }
  
  shinyApp(ui = ui, server = server)

  
  



