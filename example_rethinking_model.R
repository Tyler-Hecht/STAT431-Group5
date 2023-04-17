# setup
library(rethinking)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df$Race = as.numeric(as.factor(df$Race))
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
df$bins= as.factor(bin(df$Sentence.Time, binsize = 10))
df = df[!is.na(df$Setence.Time..num.),]

# create model
dat = list(bin = df$bins, race = df$Race, time = round(df$Sentence.Time..num.))
model = ulam(
  alist(
    bin ~ dordlogit(phi, cutpoints),
    phi <- b0[race],
    b0[race] ~ dnorm(0, 1),
    cutpoints ~ dnorm(0, 1)
  ), data = dat, chains = 4, cores = 4
)
save(model, file = "model_rethinking.file")
load("model_rethinking.file")

# check results and diagnostics
a = precis(model, depth = 2)
traceplot_ulam(model)
trankplot(model)
a$mean
barplot_race = function(race) {
  b = rep(0, 7)
  for (i in 8:14) {
    b[i-7] = a$mean[i] - a$mean[race]
  }
  library(gtools)
  b = inv.logit(b)
  p = rep(0, 8)
  for (i in 1:8) {
    if (i == 1) {
      p[1] = b[1]
    } else if (i == 8) {
      p[8] = 1 - b[7]
    } else {
      p[i] = b[i] - b[i-1]
    }
  }
  print(p)
  x = 1:8
  return(barplot(p ~ x, ylim = c(0, ceiling(max(p)*10)/10)))
}
barplot_race(4)
