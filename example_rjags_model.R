library(rjags)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df$Sentencing.County = as.numeric(as.factor(df$Sentencing.County))

model.text = "model {
  for (i in 1:N) {
    time[i] ~ dpois(pre_lambda[i])
    log(pre_lambda[i]) <- lambda[i]
    lambda[i] <- b0[county[i]]
  }
  for (county in 1:C) {
    b0[county] ~ dnorm(mu_bar, tausq_bar)
  }
  mu_bar ~ dgamma(10, 1)
  tausq_bar ~ dexp(10)
  sigma_bar <- 1 / sqrt(tausq_bar)
}"
cat(model.text, file = {example.rjags.model = tempfile()})
d = list(time = round(df$Sentence.Time), county = df$Sentencing.County, N = nrow(df), C = length(unique(df$Sentencing.County)))
model = jags.model(example.rjags.model, d, n.chains=3)
x = coda.samples(model, c("mu_bar", "sigma_bar"), n.iter=5000)
summary(window(x, 500, 5000))
mus = c(x[,1][[1]],x[,1][[2]],x[,1][[3]])
mean(exp(mus))
