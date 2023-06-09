# setup
library(rethinking)
library(gtools)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df = df[!is.na(df$Crime.Class),]

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

# MODEL 1
dat = list(bin = df$bins, race = df$Race, sex = df$Sex, veteran = df$Veteran.Status, class = df$Crime.Class, offense = df$Offense.Type, region = df$IDHS.Region, age = df$Age)
model = ulam(
  alist(
    bin ~ dordlogit(phi, cutpoints),
    cutpoints ~ dnorm(0, 1),
    phi <- b1[race] + b2[sex] + b3[veteran] + b5[offense] + b6[region] + b7*age,
    b1[race] ~ dnorm(0, 1),
    b2[sex] ~ dnorm(0, 1),
    b3[veteran] ~ dnorm(0, 1),
    b5[offense] ~ dnorm(0, 1),
    b6[region] ~ dnorm(0, 1),
    b7 ~ dnorm(0, 1)
  ), data = dat, chains = 4, cores = 4, log_lik = T
)
save(model, file = "rethinking_ordinal_additive_model - no class.file")

# MODEL 2
model = ulam(
  alist(
    bin ~ dordlogit(phi, cutpoints),
    cutpoints ~ dnorm(0, 1),
    phi <- b1[race] + b2[sex] + b3[veteran] + b4[class] + b5[offense] + b7*age,
    b1[race] ~ dnorm(0, 1),
    b2[sex] ~ dnorm(0, 1),
    b3[veteran] ~ dnorm(0, 1),
    b4[class] ~ dnorm(0, 1),
    b5[offense] ~ dnorm(0, 1),
    b7 ~ dnorm(0, 1)
  ), data = dat, chains = 4, cores = 4, log_lik = T
)
save(model, file = "rethinking_ordinal_additive_model - no region.file")

# MODEL 3
model = ulam(
  alist(
    bin ~ dordlogit(phi, cutpoints),
    cutpoints ~ dnorm(0, 1),
    phi <- b1[race] + b2[sex] + b3[veteran] + b4[class] + b5[offense] + b6[region],
    b1[race] ~ dnorm(0, 1),
    b2[sex] ~ dnorm(0, 1),
    b3[veteran] ~ dnorm(0, 1),
    b4[class] ~ dnorm(0, 1),
    b5[offense] ~ dnorm(0, 1),
    b6[region] ~ dnorm(0, 1),
  ), data = dat, chains = 4, cores = 4, log_lik = T
)
save(model, file = "rethinking_ordinal_additive_model - no age.file")