extract.gstslshet <- function(model) {
  s <- summary(model)
  #ex <- extract(model)
  coef.names <- rownames(s$CoefTable)
  rho_i <- which(coef.names == "lambda")
  lambda_i <- which(coef.names == "rho")
  coef.names[lambda_i] <- "lambda"
  coef.names[rho_i] <- "rho"
  coef <- s$CoefTable[,1]
  se <- s$CoefTable[, 2]
  pvalues <- s$CoefTable[, 4]
  n <- length(s$residuals)
  rsquared <- 1 - crossprod(s$residuals)/(nrow(s$model) * var(s$model$y))
  gof <- c(n, rsquared)
  gof.names <- c("Num. obs.", "R$^2$")
  tr <- createTexreg(coef.names = coef.names,
                     coef = coef,
                     se = se,
                     pvalues = pvalues,
                     gof = gof,
                     gof.names = gof.names)
  return(tr) 
}

extract.stsls <- function(model) {
  s <- summary(model)
  #ex <- extract(model)
  coef.names <- rownames(s$Coef)
  coef <- s$Coef[,1]
  se <- s$Coef[, 2]
  pvalues <- s$Coef[, 4]
  n <- length(s$residuals)
  rsquared <- model$rsquared
  gof <- c(n, rsquared)
  gof.names <- c("Num. obs.", "R$^2$")
  tr <- createTexreg(coef.names = coef.names,
                     coef = coef,
                     se = se,
                     pvalues = pvalues,
                     gof = gof,
                     gof.names = gof.names)
  return(tr) 
}
