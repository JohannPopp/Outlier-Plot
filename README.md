# Outlier-Plot
Creating outlier plots for logistic regression

The code produces plots for diagnosis of outliers in logistic regression as recomended by Hosmer et al 2013[^1]. A brush function hightlights data points that are selected in one of these plots in all the other plots. 

The first attempts where based on shiny and ggvis. Starting with "outlierPlotFunction.R" I am using shiny and ggplot2.

The plots are quite the same as figures 5.12 to 5.15 in the book but there are some substantial differences that are described in "diagnosticPlotInconsistencies.Rmd"


[^1]: Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd Ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013, p 186 ff.
