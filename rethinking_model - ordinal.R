# setup
library(rethinking)
library(gtools)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df = df[!is.na(df$Crime.Class),]

# bin function for non-equal bins
bin = function(data, binsize = 10, maxbin = 70) {
  n_bins = maxbin / binsize + 1
  binned = rep(0, length(data))
  for (i in 1:length(data)) {
    value = data[i]
    if (value == "LIFE") {
      binned[i] = n_bins
    } else {
      value = as.numeric(value)
      if (value >= maxbin) {
        binned[i] = n_bins
      } else {
        binned[i] = ceiling(value / binsize)
      }
    }
  }
  return(binned)
}
# df$bins= as.factor(bin(df$Sentence.Time, binsize = 10))

# bins into approximately equal bins
bin_equal = function(data) {
  binned = rep(0, length(data))
  for (i in 1:length(data)) {
    value = data[i]
    if (is.na(value)) {
      binned[i] = 6
    } else {
      if (value <= 3) {
        binned[i] = 1
      } else if (value <= 6) {
        binned[i] = 2
      } else if (value <= 10) {
        binned[i] = 3
      } else if (value <= 19) {
        binned[i] = 4
      } else if (value <= 35) {
        binned[i] = 5
      } else {
        binned[i] = 6
      }
    }
  }
  return(binned)
}
df$bins = bin_equal(df$Sentence.Time..num.)
barplot(table(df$bins)/length(df$bins), ylim = c(0, 1))
bin_names = c("< 3", "(3, 6]", "(6, 10]", "(10, 19]", "(19, 35]", "> 35 or Life")
guide = data.frame(bin = 1:6, time = bin_names)

# create model
dat = list(bin = df$bins, race = df$Race, sex = df$Sex, veteran = df$Veteran.Status, class = df$Crime.Class, offense = df$Offense.Type, region = df$IDHS.Region, age = df$Age)
model = ulam(
  alist(
    bin ~ dordlogit(phi, cutpoints),
    cutpoints ~ dnorm(0, 1),
    phi <- b1[race] + b2[sex] + b3[veteran] + b4[class] + b5[offense] + b6[region] + b7*age,
    b1[race] ~ dnorm(0, 1),
    b2[sex] ~ dnorm(0, 1),
    b3[veteran] ~ dnorm(0, 1),
    b4[class] ~ dnorm(0, 1),
    b5[offense] ~ dnorm(0, 1),
    b6[region] ~ dnorm(0, 1),
    b7 ~ dnorm(0, 1)
  ), data = dat, chains = 4, cores = 4, log_lik = T
)
save(model, file = "rethinking_ordinal_additive_model - all.file")
load("rethinking_ordinal_additive_model - all.file")

# plot
results = precis(model, depth = 2)[,1]
cutpoint_means = results[1:5]
b1_means = results[6:12]
b2_means = results[13:14]
b3_means = results[15:17]
b4_means = results[18:22]
b5_means = results[23:27]
b6_means = results[28:32]
b7_mean = results[33]
coefs = list(cutpoint = cutpoint_means, b1 = b1_means, b2 = b2_means,
          b3 = b3_means, b4 = b4_means, b5 = b5_means, b6 = b6_means,
          b7 = b7_mean)

ordinal_estimate = function(race, sex, veteran, class, offense, region, age) {
  phi = coefs$b1[race] + coefs$b2[sex] + coefs$b3[veteran] + coefs$b4[class] + coefs$b5[offense] + coefs$b6[region] + coefs$b7*age
  pre_link = rep(0, 5)
  for (i in 1:5) {
    pre_link[i] = coefs$cutpoint[i] - phi
  }
  post_link = inv.logit(pre_link)
  props = rep(0, 6)
  props[1] = post_link[1]
  props[2] = post_link[2] - post_link[1]
  props[3] = post_link[3] - post_link[2]
  props[4] = post_link[4] - post_link[3]
  props[5] = post_link[5] - post_link[4]
  props[6] = 1 - post_link[5]
  return(props)
}
props = ordinal_estimate(
  race = 1,
  sex = 1,
  veteran = 1,
  class = 1,
  offense = 1,
  region = 1,
  age = 20)
barplot(props, names = c(1:6), ylim = c(0, 1), xlab = "Bin", ylab = "Probability")
guide

# check results and diagnostics
precis(model, depth = 2)
traceplot_ulam(model)
trankplot(model)
plot(precis(model, depth = 2))
