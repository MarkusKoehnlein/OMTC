Variance_Analysis = function(Daten){
  metric_old = lm(y ~ x, data = Daten)
  means <- aggregate(Daten$y, list(Daten$x), FUN = mean)
  names(means)[names(means)=="x"] <- "y_means"
  names(means)[names(means)=="Group.1"] <- "x"
  means <- means[order(means$x),]
  means$k = order(seq(1, length(unique(Daten$x)),1))
  means$x_nc = (means$y_means - coef(metric_old)[1]) / coef(metric_old)[2]
  means <- means[c("k", "x", "x_nc", "y_means")]
  Daten = merge(Daten, means, by = "x")
  Daten$k = NULL
  means <- rename(means, c(x = "x_oc"))
  Daten$x = as.character(Daten$x)
  Dummy = lm(y ~ x, data = Daten)
  Daten$x = as.numeric(Daten$x)
  metric_nc = lm(y ~ x_nc, data = Daten)
  mean_modell = lm(y_means ~ x, data = Daten)
  Daten$x = as.character(Daten$x)
  Daten$Dummy = predict(Dummy, newdata = Daten, type = "response")
  Daten$x = as.numeric(Daten$x)
  Daten$metric_old = predict(metric_old, newdata = Daten, type = "response")
  Daten$metric_nc = predict(metric_nc, newdata = Daten, type = "response")
  OMTC = (sum((metric_old$residuals)^2) - (sum((metric_nc$residuals)^2)))/sum((metric_old$residuals)^2)
  A = ifelse(OMTC <= 0.25, "excellent",
             ifelse(OMTC > 0.25 & OMTC <= 0.5, "good",
                    ifelse(OMTC > 0.5 & OMTC <= 0.7, "acceptable",
                           ifelse(OMTC > 0.7 & OMTC <= 0.85, "questionable",
                                  ifelse(OMTC > 0.85 & OMTC <= 0.95, "weak",
                                         ifelse(OMTC > 0.95 & OMTC <= 1, "unsuitable", "error"))))))
  Variance = matrix(1:12, ncol = 3, byrow = TRUE)
  colnames(Variance) = c('metric_oc', 'metric_nc', 'mean_modell')
  rownames(Variance) = c('abs. Residual SS', 'rel. Residual SS in %', 'R2', 'OMTC')
  Variance[1,1] = round(round(sum((metric_old$residuals)^2), digits = 4), digits = 4)
  Variance[1,2] = round(round(sum((metric_nc$residuals)^2), digits = 4), digits = 4)
  Variance[1,3] = round(Variance[1,1] - Variance[1,2], digits = 4)
  Variance[2,1] = round(100, digits = 4)
  Variance[2,2] = round(Variance[1,2] / Variance[1,1] * 100, digits = 4)
  Variance[2,3] = round(Variance[2,1] - Variance[2,2], digits = 4)
  Variance[3,1] = round(summary(metric_old)$r.squared, digits = 4)
  Variance[3,2] = round(summary(metric_nc)$r.squared, digits = 4)
  Variance[4,1] = round(OMTC, digits = 4)
  Variance[4,2] = A
  Variance[3,3] = ""
  Variance[4,3] = ""
  Variance = as.data.frame(Variance)
  Variance
}
