# setup
library(rethinking)
setwd("C:/Users/tyler/Documents/Courses/23S/STAT 431/Project/STAT431-Group5")
df = read.csv("incar_data.csv")
df$Sentencing.County = as.numeric(as.factor(df$Sentencing.County))

# create model
dat = list(time = df$Sentence.Time, county = df$Sentencing.County)
model = ulam(
  alist(
    time ~ dpois(pre_lambda),
    log(pre_lambda) <- lambda[county],
    lambda <- b0[county],
    b0[county] ~ dnorm(a_bar, b_bar),
    a_bar ~ dnorm(0, 10),
    b_bar ~ dexp(10)
  ), data = dat, chains = 4, cores = 4
)
precis(model, depth = 2)

# check diagnostics (only the first county is looked at here)
traceplot(model, pars = c("a_bar", "b_bar", "b0[1]"))
trankplot(model, pars = c("a_bar", "b_bar", "b0[1]"))

# sample from posterior
post = extract.samples(model)
means = apply(exp(post$lambda), 2, mean)
grand.mean = mean(exp(post$a_bar))
grand.sd = sd(exp(post$a_bar))
sds = apply(exp(post$lambda), 2, sd)

# plot results
county = unique(df$Sentencing.County)
plot(means ~ county, xlab = "county #", ylab = "lambda", ylim = c(0, 40))
arrows(county, means, county, means + sds, length = 0.02, angle = 90)
arrows(county, means, county, means - sds, length = 0.02, angle = 90)
abline(h = grand.mean, col = "red")
abline(h = grand.mean + grand.sd, col = "orange")
abline(h = grand.mean - grand.sd, col = "orange")
