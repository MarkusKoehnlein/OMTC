Grafic = function(Daten){
  Daten$x = as.numeric(Daten$x)
  Daten$y = as.numeric(Daten$y)
  metric_old = lm(y ~ x, data = Daten)
  metric_old
  coef(metric_old)[1]
  coef(metric_old)[2]
  means <- aggregate(Daten$y, list(Daten$x), FUN = mean)
  names(means)[names(means)=="x"] <- "y_means"
  names(means)[names(means)=="Group.1"] <- "x_oc"
  means <- means[order(means$x_oc),]
  means$k = order(seq(1, length(unique(Daten$x)),1))
  means$x_nc = (means$y_means - coef(metric_old)[1]) / coef(metric_old)[2]
  means <- means[c("k", "x_oc", "x_nc", "y_means")]
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

  Daten$y_means = ifelse(Daten$x == means$x_oc, means$y_means)
  Daten$x_nc = (Daten$y_means - coef(metric_old)[1]) / coef(metric_old)[2]

  plot(Daten$x_nc, Daten$y, xlab = "x-Werte", ylab = "y-Werte",
       xlim = c(min(coding$x_oc, coding$x_nc, 1) - 1, max(coding$x_oc, coding$x_nc) + 0.2),
       ylim = c(min(min(Daten$y) - 0.3, 0), max(Daten$y) + 0.3), xaxs="i", yaxs="i",
       col = "black", cex = 2, cex.lab = 2, cex.axis = 1.5)
  points(Daten$x, Daten$y, cex = 2)
  abline(lm(Daten$y ~ Daten$x)$coef, col = "darkblue",cex = 2, lwd = 2)

  for (i in 1:nrow(means)){
    points(coding[i,2], aggregate(Daten$y, list(Daten$x),
                                  FUN = mean)[i, 2], col = i, pch = 19, cex = 2)
  }

  for (i in 1:nrow(means)){
    points(coding[i,3], aggregate(Daten$y, list(Daten$x),
                                  FUN = mean)[i, 2], col = i, pch = 19, cex = 2)
  }

  arrows(coding$x_oc, coding$y_means, coding$x_nc, coding$y_means,
         length = 0.25, code = 2, lwd = 2, col = "gray30")

}
