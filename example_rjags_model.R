library(rjags)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df = df[!is.na(df$Sentence.Time..num.),]
df = df[!is.na(df$Crime.Class),]

model.text = "model {
  for (i in 1:N) {
    time[i] ~ dpois(lambda[i])
    log(lambda[i]) <- b1[race[i]] + b2[sex[i]] + b3[veteran[i]] + b4[offense[i]] + b5[class[i]] + b6[region[i]] + b7*age[i]
  }
  for (race in 1:R) {
    b1[race] ~ dnorm(0, 1)
  }
  for (sex in 1:S) {
    b2[sex] ~ dnorm(0, 1)
  }
  for (veteran in 1:V) {
    b3[veteran] ~ dnorm(0, 1)
  }
  for (offense in 1:O) {
    b4[offense] ~ dnorm(0, 1)
  }
  for (class in 1:C) {
    b5[class] ~ dnorm(0, 1)
  }
  for (region in 1:RE) {
    b6[region] ~ dnorm(0, 1)
  }
  b7 ~ dnorm(0, 1)
}"
cat(model.text, file = {example.rjags.model = tempfile()})
d = list(time = round(df$Sentence.Time..num), N = nrow(df), race = df$Race, R = 7, veteran = df$Veteran.Status, V = 3, sex = df$Sex, S = 2, offense = df$Offense.Type, O = 5, class = df$Crime.Class, C = 5, region = df$IDHS.Region, RE = 5, age = df$Age)
model = jags.model(example.rjags.model, d, n.chains=3)
x = coda.samples(model, c("b1", "b2", "b3", "b4", "b5", "b6", "b7"), n.iter=5000)
traceplot(x) 
gelman.diag(x)
summary(window(x, 1500, 6000))
save(x, file = "rjags_additive_poisson_model.file")
load("rjags_additive_poisson_model.file")
