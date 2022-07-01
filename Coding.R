Coding = function(Daten){
  Daten$x = as.numeric(Daten$x)
  Daten$y = as.numeric(Daten$y)
  metric_old = lm(y ~ x, data = Daten)
  means <- aggregate(Daten$y, list(Daten$x), FUN = mean)
  names(means)[names(means)=="x"] <- "y_means"
  names(means)[names(means)=="Group.1"] <- "x_oc"
  means <- means[order(means$x_oc),]
  means$k = order(seq(1, length(unique(Daten$x)),1))
  means$x_nc = (means$y_means - coef(metric_old)[1]) / coef(metric_old)[2]
  means <- means[c("k", "x_oc", "x_nc", "y_means")]
  Daten$y_means = ifelse(Daten$x == means$x_oc, means$y_means)
  Daten$x_nc = (Daten$y_means - coef(metric_old)[1]) / coef(metric_old)[2]
  Daten$y_means = NULL
  return(Daten)
}

