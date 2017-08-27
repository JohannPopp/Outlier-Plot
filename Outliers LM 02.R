# Outlier for linear regression

# Johann Popp
# created:  2015-07-15
# updated:  

#######################################

# Read and prepare data

dat <- read.csv2("/media/poppi/Windows7_OS/Austausch/Literatur/Statistik/daten für Hosmer logistic regression/lowbwt.csv")
head(dat)
dat$RACE <- as.factor(dat$RACE)
dat$SMOKE <- as.factor(dat$SMOKE)
dat$BWT[c(1,3,7,10,15,16)] <- NA


# Model data

mod1 <- lm(BWT ~ LWT + AGE + SMOKE + RACE, data = dat)
summary(mod1)

mod2 <- lm(BWT ~ LWT + SMOKE + RACE, data = dat, y = TRUE, x = TRUE)  
# y=TRUE and x=TRUE is important to save the original data in the model.
summary(mod2)


# Combine data and influence statistics in a new data frame.

dat2 <- data.frame(outID = rownames(mod2$model), mod2$model, pred = predict(mod2), zResiduals = rstandard(mod2), dffits = dffits(mod2), dfbetas = dfbetas(mod2), covratio(mod2), cooks.distance(mod2), hatvalues(mod2))


# Plots
library(shiny)
library(ggvis)

ui <- fluidPage(
  fluidRow(
    column(3, 
           ggvisOutput("plot1"),
           selectInput("x1", "x value", c("dat2$pred", "dffit"))),
    column(3, offset = 1,
           ggvisOutput("plot2")),
    column(3, offset = 1,
           ggvisOutput("plot3"),
           ggvisOutput("plot4"))
  )
)

server <- function(input, output){
  
  # This linkes the brush
  lb <- linked_brush(keys = dat2$outID, fill = "red")
  
  # selector
#  selector <- input_select(colnames(dat2), selected = "pred", label = "X Value")
  
  # Plot 1
  scatPlot <- function(x, y = zResiduals, plotNo = "plot1", ylab = "y", xlab = "Predicted") {
    dat2 %>% 
      ggvis(~x, ~y, key := ~outID) %>% 
      layer_points(fill := lb$fill, fill.brush := "red", stroke := "black", fillOpacity := 0.7, size := 30) %>%
      add_axis("x", title = xlab) %>% 
      add_axis("y", title = ylab) %>%
      lb$input() %>%
      set_options(width = 300, height = 300) %>%
      bind_shiny(plot_id = plotNo)
  }
  
#  scatPlot(x = input$x1, y = dat2$zResiduals)
  
  dat2 %>% 
    ggvis(~pred, ~zResiduals, key := ~outID) %>% 
    layer_points(fill := lb$fill, fill.brush := "red", stroke := "black", 
                 fillOpacity := 0.7, size := 30) %>%
    add_axis("y", title = "Standardized Residuals") %>%
    add_axis("x", title = "Predicted") %>%
    layer_lines() %>%
    lb$input() %>%
    set_options(width = 300, height = 300) %>%
    bind_shiny("plot1")
  

  # Selection based on the brush
  selected <- lb$selected
  dat2_selected <- reactive({
    dat2[selected(),]
  })
  
  # Plot 2
  scatPlot(dat2$pred, dat2$hatvalues.mod2., plotNo = "plot2", ylab = "Leverage")
  
  # Plot 3
  scatPlot(dat2$pred, dat2$cooks.distance.mod2., plotNo = "plot3", "Cooks Distance")
}
  

shinyApp(ui = ui, server = server)