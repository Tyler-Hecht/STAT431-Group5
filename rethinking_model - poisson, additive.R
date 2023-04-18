# setup
library(rethinking)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df = df[!is.na(df$Sentence.Time..num.),]
df = df[!is.na(df$Crime.Class),]

# create model
dat = list(time = df$Sentence.Time..num., race = df$Race, sex = df$Sex, veteran = df$Veteran.Status, class = df$Crime.Class, offense = df$Offense.Type, region = df$IDHS.Region, age = df$Age)
model = ulam(
  alist(
    time ~ dnorm(mu, sigma),
    mu <- b0 + b1[race] + b2[sex] + b3[veteran] + b4[class] + b5[offense] + b6[region] + b7*age,
    b0 ~ dnorm(3, 1),
    b1[race] ~ dnorm(0, 1),
    b2[sex] ~ dnorm(0, 1),
    b3[veteran] ~ dnorm(0, 1),
    b4[class] ~ dnorm(0, 1),
    b5[offense] ~ dnorm(0, 1),
    b6[region] ~ dnorm(0, 1),
    b7 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = dat, chains = 4, cores = 4
)
save(model, file = "rethinking_additive_poisson_model - intercept, all.file")
load("rethinking_additive_poisson_model - intercept, race, age.file")

# check results and diagnostics
precis(model, depth = 2)
traceplot_ulam(model)
trankplot(model)
