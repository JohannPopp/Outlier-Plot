# Calculate logistic regression diagnostics according to Hosmer et al. 2013.

# Johann Popp
# 2017-11-03
#############################################

logRegDiagn <- function(model){
  # model is a logistic regression model
  # out.value is the value that is used to indicate that the outcome is present
  
  ################
  # Basic data extraction and calculation
  
  # Extract non-aggregated data
  datN <- model$model
    # convert to covariate pattern
  cp <- unique(datN[,-1])
  
  # Indicator for covariate patterns
  cpIdN<- apply(datN[,-1], 1, function(x) paste(x, collapse = ""))
  cpIdM <- apply(cp, 1, function(x) paste(x, collapse = ""))
  
  # Size of covariate patterns
  m <- tapply(datN[,1], cpIdN, length)[rank(cpIdM)]
  # Number of cases per covariate pattern
  Y <- tapply(model$y, cpIdN, sum)[rank(cpIdM)]
  # Estimated probability
  Pi <- fitted.values(model)[rownames(cp)]
  
  ####################
  # Basic residuals
  
  # Pearson residual (formula 5.1)
  rPears <- (Y - Pi * m) / sqrt(m * Pi * (1 - Pi))
  
  # Deviance residual (formula 5.3)
  rDev <- ifelse(Y - m * Pi > 0, 1, -1) * 
    (2 * (Y * log(Y / (m * Pi)) + (m - Y) * log((m - Y) / (m * (1 - Pi)))))^(1/2)
  rDev[Y == 0] <- -sqrt(2 * m[Y == 0] * abs(log(1 - Pi[Y == 0])))
  rDev[Y == m] <- sqrt(2 * m[Y == m] * abs(log(Pi[Y == m])))
  
  
  ############
  # Diagnostic statistics to plot
  
  # Leverage
  V <- diag(m * Pi * (1 - Pi))
  X <- unique(model.matrix(model))
  
  H <- V^(1/2) %*% X %*% solve(t(X) %*% V %*% X) %*% t(X) %*% (V^(1/2)) # formula 5.21
  h <- diag(H)

  # Cook's distance (formula 5.24)
  deltaBeta <- rPears^2 * h / (1 - h)^2  
  
  # Change in Pearson chi-square (formula 5.25)
  deltaChi <- rPears^2 / (1 - h)
  
  # Change in deviance (formula 5.26)
  deltaDeviance <- rDev^2 / (1 - h)
  
  
  ################
  # collect output data
  out <- data.frame(m, Y, Pi, rPears, rDev, h, deltaBeta, deltaChi, deltaDeviance, cpIdM, cp)
  rownames(out) <- rownames(cp)
  out
  
}

logRegDiagn(model)
