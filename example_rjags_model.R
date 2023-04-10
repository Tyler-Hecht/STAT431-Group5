library(rjags)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df$Race = as.factor(df$Race)
df = df[!is.na(df$Setence.Time..num.),]

model.text = "model {
  for (i in 1:N) {
    time[i] ~ dpois(pre_lambda[i])
    log(pre_lambda[i]) <- b0[race[i]]
  }
  for (race in 1:R) {
    b0[race] ~ dnorm(0, 10)
  }
}"
cat(model.text, file = {example.rjags.model = tempfile()})
d = list(time = round(df$Setence.Time..num), N = nrow(df), race = df$Race, R = length(unique(df$Race)))
model = jags.model(example.rjags.model, d, n.chains=3)
x = coda.samples(model, c("b0"), n.iter=5000)
traceplot(x)
gelman.plot(x)
summary(window(x, 1500, 6000))
save(x, file = "tmp.file")
