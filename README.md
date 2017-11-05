# Outlier-Plot
Creating outlier plots for logistic and linear regression

The code produces plots for diagnosis of outliers in logistic regression as recomended by Hosmer et al 2013[^1]. A brush function hightlights data points that are selected in one of these plots in all the other plots. 

After some difficulties reproducing the example plots from the book exactly, I finally programed the calculation of the statistics myself and now it seems that the code does exactly the same as described in the book. I described the process to achive this at [diganosticPlotInconsistency.pdf](https://github.com/JohannPopp/Outlier-Plot/blob/master/diganosticPlotInconsistency.pdf)

[Example Logistic Regression Diagnostics.R](https://github.com/JohannPopp/Outlier-Plot/blob/master/Example%20Logistic%20Regression%20Diagnostics.R) is the shiny app published at https://poppi.shinyapps.io/logisticregressiondiagnostics/. Within this app it is possible to load your own data and specify your own model. For R users it might be even better to source [FunctionLogisticRegressionDiagnostics.R](https://github.com/JohannPopp/Outlier-Plot/blob/master/FunctionLogisticRegressionDiagnostics.R) and than to apply the function DiagPlotLogistic() to your glm-object. 

[Linear Regression Diagnostics.R](https://github.com/JohannPopp/Outlier-Plot/blob/master/Linear%20Regression%20Diagnostics.R) produces plots for a linear regression model as recommended by Field 2005[^2] at https://poppi.shinyapps.io/linear-model-diagnostics/. The R-function to apply the plots to a lm()-object can be found at [FunctionLinearRegressionDiagnostics.R](https://github.com/JohannPopp/Outlier-Plot/blob/master/FunctionLinearRegressionDiagnostics.R)


[^1]: Hosmer, David W., Stanley Lemeshow, und Rodney X. Sturdivant. Applied logistic regression. 3rd Ed. Wiley series in probability and statistics. Hoboken, NJ: Wiley, 2013, p 186 ff.

[^2]: Field, Andy P. Discovering statistics using SPSS: (and sex, drugs and rock „n“ roll). 2nd ed. London, Thousand Oaks, Calif.: Sage Publications, 2005.
