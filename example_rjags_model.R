library(rjags)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df = df[!is.na(df$Sentence.Time..num.),]
df = df[!is.na(df$Crime.Class),]

model.text = "model {
  for (i in 1:N) {
    time[i] ~ dnorm(mu[i], tausq)
    mu[i] <- b1[sex[i], region[i]]
  }
  tausq ~ dgamma(0.001, 0.001)
  for (region in 1:RE) {
    for (sex in 1:S) {
      b1[sex, region] ~ dnorm(mu_bar[sex], tausq_bar[sex])
    }
  }
  for (sex in 1:S) {
    mu_bar[sex] ~ dnorm(0, 0.0001)
    tausq_bar[sex] ~ dgamma(0.001, 0.001)
  }
}"
cat(model.text, file = {example.rjags.model = tempfile()})
d = list(time = df$Sentence.Time..num, N = nrow(df), race = df$Race, R = 7, veteran = df$Veteran.Status, V = 3, sex = df$Sex, S = 2, offense = df$Offense.Type, O = 5, class = df$Crime.Class, C = 5, region = df$IDHS.Region, RE = 5, age = df$Age)
model = jags.model(example.rjags.model, d, n.chains=3)
x = coda.samples(model, c("b1", "mu_bar", "tausq_bar", "tausq"), n.iter=5000)
traceplot(x) 
gelman.diag(x)
summary(window(x, 500, 5000))
save(x, file = "rjags_additive_poisson_model.file")
load("rjags_additive_poisson_model.file")
