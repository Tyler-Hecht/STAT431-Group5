library(brms)
library(gtools)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df$county = as.numeric(as.factor(df$Sentencing.County))
df$Race = as.numeric(as.factor(df$Race))
df = df[df$Sentence.Time != "SDP",]
bin = function(data, binsize = 10, maxbin = 70) {
  n_bins = maxbin / binsize + 1
  binned = rep(0, length(data))
  for (i in 1:length(data)) {
    value = data[i]
    if (value == "LIFE") {
      binned[i] = n_bins
    } else {
      value = as.numeric(value)
      if (value > maxbin) {
        binned[i] = n_bins
      } else {
        binned[i] = ceiling(value / binsize)
      }
    }
  }
  return(binned)
}
df$bins= bin(df$Sentence.Time)
brm_model = brm(bins ~ (1|county), data = df, family=cumulative("logit"))
estimates = inv.logit(fixef(brm_model)[,1])
p = rep(0, length(estimates)+1)
for (i in 1:(length(estimates)+1)) {
  if (i == 1) {
    p[i] = estimates[i]
  } else if (i == length(estimates)+1) {
    p[i] = 1 - estimates[i-1]
  } else {
    p[i] = estimates[i] - estimates[i-1]
  }
}
p
true = table(df$bins)/length(df$bins)
true
