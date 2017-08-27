# Interactive diagnostic plots for outliers in logistic regression
# Reproduces the plots from Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3. Aufl. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013.

# Johann Popp
# 2015-12-04
#########################################################

# Load data
glow <- aplore3::glow500

# Recode RATERISK
glow$raterisk3 <- cut(as.numeric(glow$raterisk), 2)

# Logistic model from Table 4.16
mod <- glm(fracture ~ age + height + priorfrac + momfrac + armassist + raterisk3 + age:priorfrac + momfrac:armassist, data = glow, family = "binomial")
summary(mod)

##################################
# Transform to covariate pattern
##################################

library(epiR)

# aggregate to covariate pattern
glow.cp <- epi.cp(model.frame(mod)[-1])

# Number of outcome events per covariate pattern
glow.obs <- as.vector(by(as.numeric(mod$model$fracture)-1, as.factor(glow.cp$id), sum))

# Estimated outcome probability per covariate pattern
glow.fit <- as.vector(by(mod$fitted.values, as.factor(glow.cp$id), min))

# Calculate residuals and influence measures per covariate pattern
glow.res <- epi.cpresids(glow.obs, glow.fit, glow.cp)

# Calculate Change in deviance
deltaDeviance <- glow.res$deviance^2 + ((glow.res$pearson^2*glow.res$leverage)/(1 - glow.res$leverage))

# Put it together in one data.frame
dat <- data.frame(glow.res, deltaDeviance, glow.fit, glow.cp$cov.pattern)

##############################
# Plots
##############################
library(shiny)
library(ggvis)

ui <- fluidPage(
  h2("Logistic Regression Diagnostics"),
  fluidRow(
    column(3, 
           ggvisOutput("plot1"),
           verbatimTextOutput("model")),
    column(3, offset = 1,
           ggvisOutput("plot2"),
           ggvisOutput("plot5")),
    column(3, offset = 1,
           ggvisOutput("plot3"),
           ggvisOutput("plot4"))
  ),
  fluidRow(
    h3("Selected Covariate Patterns"),
    verbatimTextOutput("data")
  )
)


server <- function(input, output){
  
  # This linkes the brush
  lb <- linked_brush(keys = dat$cpid, fill = "red")
  
  # selector
  #  selector <- input_select(colnames(dat2), selected = "pred", label = "X Value")
  
  # Plot 1
  scatPlot <- function(x, y, Xlab = "Estimated probability", Ylab = "y", plotNo = "plot1") {
    dat %>% 
      ggvis(~x, ~y, key := ~cpid) %>% 
      layer_points(fill := lb$fill, fill.brush := "red", stroke := "black", fillOpacity := 0.7, size := 30) %>%
      add_axis("x", title = Xlab) %>%
      add_axis("y", title = Ylab) %>%
      lb$input() %>%
      set_options(width = 300, height = 250) %>%
      bind_shiny(plot_id = plotNo)
  }
  
  
  
  scatPlot(x = dat$glow.fit, y = dat$leverage, Ylab = "Leverage")
  
  
  # Selection based on the brush
  selected <- lb$selected
  dat_selected <- reactive({
    dat[selected(),]
  })
  
  # Plot 2
  scatPlot(dat$glow.fit, dat$deltachi, plotNo = "plot2", Ylab = "Change in Pearson chi-square")
  
  # Plot 3
  scatPlot(dat$glow.fit, dat$deltaDeviance, plotNo = "plot3", Ylab = "Change in Deviance")
  
  # Plot 4
  scatPlot(dat$glow.fit, dat$deltabeta, plotNo = "plot4", Ylab = "Cook's distance")
  
  # Plot 5
  dat %>%
    ggvis(~dat$glow.fit, ~dat$deltachi, key := ~cpid) %>%
    layer_points(fill := lb$fill, fill.brush := "red", stroke := lb$fill, fillOpacity := 0.05, size := input_slider(0, 10, map = function(x) x * dat$deltabeta * 200 / max(dat$deltabeta))) %>%
    hide_legend("size") %>%
    add_axis("x", title = "Estimated probability") %>%
    add_axis("y", title = "Change in Pearson chi-square") %>%
    lb$input() %>%
    set_options(width = 300, height = 250)  %>%
    bind_shiny(plot_id = "plot5")
  
  # Text output
  output$data <- renderPrint(if(any(selected())) {dat_selected()[,c(2:4, 18:ncol(dat))]})
  

  
  
}


shinyApp(ui = ui, server = server)