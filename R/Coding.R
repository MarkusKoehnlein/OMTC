Coding = function(Daten){
  Daten$x = as.numeric(Daten$x)
  Daten$y = as.numeric(Daten$y)
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
  Daten$y_means = NULL
  return(Daten)
}
