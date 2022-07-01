Dashboard = function(Daten){
  Daten$x = as.numeric(Daten$x)
  Daten$y = as.numeric(Daten$y)
  head(Daten)

  metric_old = lm(y ~ x, data = Daten)
  metric_old
  coef(metric_old)[1]
  coef(metric_old)[2]
  means <- aggregate(Daten$y, list(Daten$x), FUN = mean)
  names(means)[names(means)=="x"] <- "y_means"
  names(means)[names(means)=="Group.1"] <- "x"
  means <- means[order(means$x),]
  means$k = order(seq(1, length(unique(Daten$x)),1))
  means$x_nc = (means$y_means - coef(metric_old)[1]) / coef(metric_old)[2]
  means <- means[c("k", "x", "x_nc", "y_means")]
  means
  coding = means
  coding

  Order = means
  Order$auf = sort(Order$y_means, decreasing = FALSE)
  Order$ab = sort(Order$y_means, decreasing = TRUE)
  Order$test_auf = ifelse(Order$y_means == Order$auf, 1, 0)
  Order$test_ab = ifelse(Order$y_means == Order$ab, 1, 0)
  Order
  Ordinal_sequence_observed = ifelse(sum(Order$test_auf) == length(Order$k)
                                     | sum(Order$test_ab) == length(Order$k), "TRUE", "FALSE")
  Ordinal_sequence_observed

  Daten = merge(Daten, means, by = "x")
  Daten$k = NULL
  means <- rename(means, c(x = "x_oc"))

  head(Daten)

  head(Daten)

  Daten$x = as.character(Daten$x)
  Dummy = lm(y ~ x, data = Daten)
  summary(Dummy)
  Daten$x = as.numeric(Daten$x)

  metric_nc = lm(y ~ x_nc, data = Daten)
  summary(metric_nc)

  mean_modell = lm(y_means ~ x, data = Daten)
  summary(mean_modell)

  Dashboard <- matrix(1:64, ncol = 4, byrow=TRUE)
  colnames(Dashboard) <- c('Dummy','metric oc', 'better model', 'metric nc')
  rownames(Dashboard) <- c('R2','Adjusted R2','SS', 'MSE Train', 'MSE Test', 'RMSE Train',
                           'RMSE Test', 'rel. RMSE Train (in %)', 'rel. RMSE Test (in %)',
                           'MAE Train', 'MAE Test', 'MAPE Train (in %)', 'MAPE Test (in %)',
                           'df Train', 'df Test','Ordinal sequence')
  Dashboard
  Dashboard[1,1] = round(summary(Dummy)$r.squared, digits = 4)
  Dashboard[1,2] = round(summary(metric_old)$r.squared, digits = 4)
  Dashboard[1,4] = round(summary(metric_nc)$r.squared, digits = 4)
  Dashboard[2,1] = round(summary(Dummy)$adj.r.squared, digits = 4)
  Dashboard[2,2] = round(summary(metric_old)$adj.r.squared, digits = 4)
  Dashboard[2,4] = round(1 - (((length(Daten$y) - 1) / (length(Daten$y)
                                                        - length(coef(Dummy)) - length(coef(metric_old))))
                              * (1 - summary(metric_nc)$r.squared)), digits = 4)
  Dashboard[3,1] = round(sum((Dummy$residuals)^2), digits = 4)
  Dashboard[3,2] = round(sum((metric_old$residuals)^2), digits = 4)
  Dashboard[3,4] = round(sum((metric_nc$residuals)^2), digits = 4)
  Dashboard[4,1] = round(sum((Dummy$residuals)^2) / (length(Daten$y)
                                                     - length(coef(Dummy))), digits = 4)
  Dashboard[4,2] = round(sum((metric_old$residuals)^2) / (length(Daten$y)
                                                          - length(coef(metric_old))), digits = 4)
  Dashboard[4,4] = round(sum((metric_nc$residuals)^2) /
                           (length(Daten$y) - length(coef(metric_nc))
                            - length(coef(Dummy))), digits = 4)
  Dashboard[5,1] = round(sum((Dummy$residuals)^2) / length(Daten$y), digits = 4)
  Dashboard[5,2] = round(sum((metric_old$residuals)^2) / length(Daten$y), digits = 4)
  Dashboard[5,4] = round(sum((metric_nc$residuals)^2) / length(Daten$y), digits = 4)
  Dashboard[6,1] = round(sqrt(sum((Dummy$residuals)^2)
                              / (length(Daten$y) - length(coef(Dummy)))), digits = 4)
  Dashboard[6,2] = round(sqrt(sum((metric_old$residuals)^2)
                              / (length(Daten$y) - length(coef(metric_old)))), digits = 4)
  Dashboard[6,4] = round(sqrt(sum((metric_nc$residuals)^2)
                              / (length(Daten$y) - length(coef(metric_nc))
                                 - length(coef(Dummy)))), digits = 4)
  Dashboard[7,1] = round(sqrt(sum((Dummy$residuals)^2) / length(Daten$y)), digits = 4)
  Dashboard[7,2] = round(sqrt(sum((metric_old$residuals)^2) / length(Daten$y)), digits = 4)
  Dashboard[7,4] = round(sqrt(sum((metric_nc$residuals)^2) / length(Daten$y)), digits = 4)
  Dashboard[8,1] = round(sqrt(sum((Dummy$residuals)^2)
                              / (length(Daten$y) - length(coef(Dummy))))
                         / mean(Daten$y) * 100, digits = 4)
  Dashboard[8,2] = round(sqrt(sum((metric_old$residuals)^2)
                              / (length(Daten$y) - length(coef(metric_old))))
                         / mean(Daten$y) * 100, digits = 4)
  Dashboard[8,4] = round(sqrt(sum((metric_nc$residuals)^2)
                              / (length(Daten$y) - length(coef(metric_nc)) - length(coef(Dummy))))
                         / mean(Daten$y) * 100, digits = 4)
  Dashboard[9,1] = round(sqrt(sum((Dummy$residuals)^2)
                              / length(Daten$y)) / mean(Daten$y) * 100, digits = 4)
  Dashboard[9,2] = round(sqrt(sum((metric_old$residuals)^2)
                              / length(Daten$y)) / mean(Daten$y) * 100, digits = 4)
  Dashboard[9,4] = round(sqrt(sum((metric_nc$residuals)^2)
                              / length(Daten$y)) / mean(Daten$y) * 100, digits = 4)
  Dashboard[10,1] = round(sum(abs(Dummy$residuals))
                          / (length(Daten$y) - length(coef(Dummy))), digits = 4)
  Dashboard[10,2] = round(sum(abs(metric_old$residuals))
                          / (length(Daten$y) - length(coef(metric_old))), digits = 4)
  Dashboard[10,4] = round(sum(abs(metric_nc$residuals))
                          / (length(Daten$y) - length(coef(metric_nc))
                             - length(coef(Dummy))), digits = 4)
  Dashboard[11,1] = round(sum(abs(Dummy$residuals)) / length(Daten$y), digits = 4)
  Dashboard[11,2] = round(sum(abs(metric_old$residuals)) / length(Daten$y), digits = 4)
  Dashboard[11,4] = round(sum(abs(metric_nc$residuals)) / length(Daten$y), digits = 4)
  Dashboard[12,1] = round(sum(abs(Dummy$residuals / Daten$y))
                          / (length(Daten$y) - length(coef(Dummy))) * 100, digits = 4)
  Dashboard[12,2] = round(sum(abs(metric_old$residuals / Daten$y))
                          / (length(Daten$y) - length(coef(metric_old))) * 100, digits = 4)
  Dashboard[12,4] = round(sum(abs(metric_nc$residuals / Daten$y))
                          / (length(Daten$y) - length(coef(metric_nc))
                             - length(coef(Dummy))) * 100, digits = 4)
  Dashboard[13,1] = round(sum(abs(Dummy$residuals / Daten$y))
                          / length(Daten$y) * 100, digits = 4)
  Dashboard[13,2] = round(sum(abs(metric_old$residuals / Daten$y))
                          / length(Daten$y) * 100, digits = 4)
  Dashboard[13,4] = round(sum(abs(metric_nc$residuals / Daten$y))
                          / length(Daten$y) * 100, digits = 4)
  Dashboard[14,1] = summary(Dummy)$df[2]
  Dashboard[14,2] = summary(metric_old)$df[2]
  Dashboard[14,4] = length(Daten$y) - length(coef(metric_nc)) - length(coef(Dummy))
  Dashboard[15,1] = length(Daten$y)
  Dashboard[15,2] = length(Daten$y)
  Dashboard[15,4] = length(Daten$y)
  Dashboard[16,1] = "observed"
  Dashboard[16,2] = paste("in", paste(max(sum(Order$test_auf),sum(Order$test_ab)),
                                      "/", length(Order$k), sep=""), "cases.")
  Dashboard[16,4] = ""
  Dashboard[1,3] = ifelse(as.numeric(Dashboard[1,1]) > as.numeric(Dashboard[1,2]),
                          "Dummy", "metric") #R2
  Dashboard[2,3] = ifelse(as.numeric(Dashboard[2,1]) > as.numeric(Dashboard[2,2]),
                          "Dummy", "metric") #Adjusted R2
  Dashboard[3,3] = ifelse(as.numeric(Dashboard[3,1]) < as.numeric(Dashboard[3,2]),
                          "Dummy", "metric") #SS
  Dashboard[4,3] = ifelse(as.numeric(Dashboard[4,1]) < as.numeric(Dashboard[4,2]),
                          "Dummy", "metric") #MSE Train
  Dashboard[5,3] = ifelse(as.numeric(Dashboard[5,1]) < as.numeric(Dashboard[5,2]),
                          "Dummy", "metric") #MSE Test
  Dashboard[6,3] = ifelse(as.numeric(Dashboard[6,1]) < as.numeric(Dashboard[6,2]),
                          "Dummy", "metric") #RMSE Train
  Dashboard[7,3] = ifelse(as.numeric(Dashboard[7,1]) < as.numeric(Dashboard[7,2]),
                          "Dummy", "metric") #RMSE Test
  Dashboard[8,3] = ifelse(as.numeric(Dashboard[8,1]) < as.numeric(Dashboard[8,2]),
                          "Dummy", "metric") #rel. RMSE Train
  Dashboard[9,3] = ifelse(as.numeric(Dashboard[9,1]) < as.numeric(Dashboard[9,2]),
                          "Dummy", "metric") #rel. RMSE Test
  Dashboard[10,3] = ifelse(as.numeric(Dashboard[10,1]) < as.numeric(Dashboard[10,2]),
                           "Dummy", "metric") #MAE Train
  Dashboard[11,3] = ifelse(as.numeric(Dashboard[11,1]) < as.numeric(Dashboard[11,2]),
                           "Dummy", "metric") #MAE Test
  Dashboard[12,3] = ifelse(as.numeric(Dashboard[12,1]) < as.numeric(Dashboard[12,2]),
                           "Dummy", "metric") #MAPE Train
  Dashboard[13,3] = ifelse(as.numeric(Dashboard[13,1]) < as.numeric(Dashboard[13,2]),
                           "Dummy", "metric") #MAPE Test
  Dashboard[14,3] = ifelse(as.numeric(Dashboard[14,1]) < as.numeric(Dashboard[14,2]),
                           "Dummy", "metric") #df Train
  Dashboard[15,3] = ifelse(as.numeric(Dashboard[14,1]) < as.numeric(Dashboard[14,2]),
                           "Dummy", "metric") #df Test
  Dashboard[16,3] = ifelse(Ordinal_sequence_observed == "TRUE", "metric", "Dummy") #Ordinal sequence
  Dashboard = as.data.frame(Dashboard)
  Dashboard
}
