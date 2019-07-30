library(readr)
library(rstan)
library(zoo)
library(dplyr)
require('xml2')
require('rvest')
require('XML')
require('magrittr')
require('stringr')

Polls <- getPollData()
Elections <- read.csv2("data/Elections.csv")

Elections$Datum <- as.Date(Elections$Datum)

Date <- "2017-06-25"

Polls <- Polls %>% filter(Datum <= Date)

###
Polls %>% na.locf(fromLast = TRUE) %>% na.locf -> Polls2
dateSeq <- seq(min(Polls2$Datum), max(Polls2$Datum) , by = "week")
smoothProportions <- sapply(colnames(Polls2)[3:7],
                            function(y){ 
                              sSpline <- smooth.spline(x = Polls2$Datum, y = unlist(Polls2[,y]))
                              predict(sSpline, x = as.numeric(dateSeq))$y
                            })
acfs <- rowMeans(apply(apply(smoothProportions, 2, diff), 2, function(x) acf(x, 200)$acf))
plot(acfs, type = "h")
abline(h = 0)
plot(cumsum(acfs)-1)
###
adds <- 0
substract <- 0
y <- rep(0,50)
epsilon <- c(0, 1, rep(0,48))
theta = 0.87
theta2 = 0.72
alpha = 0.4774

for(n in 2:50){
  y[n]         = y[n-1]  + epsilon[n] + adds - substract;
  adds = (adds + epsilon[n]) * theta2;
  substract = substract * theta + (alpha * (adds + epsilon[n])) * (1 - theta);
}

plot(y, type = "l")

epsilon <- rnorm(100000)
y <- rep(0,100000)
for(n in 2:100000){
  y[n]         = y[n-1]  + epsilon[n] + adds - substract;
  adds = (adds + epsilon[n]) * theta2;
  substract = substract * theta + (alpha * (adds + epsilon[n])) * (1 - theta);
}

acf(diff(y), 50)

###

u <- Elections[Elections$Datum < Date, c(10,9,1:6)]
v <- Polls[,c(1,2,3:8)]
names(u) <- names(v)

u$Election = TRUE
v$Election = FALSE

X = rbind(v,u)
X <- X %>% arrange(Datum)

Missing <- t((is.na(X[,3:8]))) * 1

for(i in 3:8){
  X[, i] <- na.locf(X[, i], fromLast = TRUE, na.rm = TRUE)
}

rm(u, v, Polls)
IMatrix <- model.matrix(~ Institut - 1, data = X)
IMatrix <- IMatrix[, - which(colnames(IMatrix) == "InstitutBTW")]
NPollsters <- ncol(IMatrix)

X = X[, -1]
X[,1] = X[,1] %>% as.numeric()
X = as.matrix(X)

IMatrix = IMatrix[order(X[,1], decreasing = F), ]
X = X[order(X[,1], decreasing = F), ]
BTWIndikator <- rowSums(IMatrix)

#Wochenbasis
X[,1] = ceiling(X[,1]/7)

pollCommon <- model.matrix(~commonPoll - 1, data.frame(commonPoll = factor(cumsum(X[,"Election"]) + 1)))
pollCommon[rowSums(IMatrix) == 0] <- 0


NBTW <- ncol(pollCommon)

NTOTAL = nrow(X)

poll=X[,2:7]
poll <- log(poll/(1-poll))
poll[is.na(poll)] <- -Inf
NParties <- ncol(poll)

epst <- seq(min(X[,"Datum"]), max(X[,"Datum"]) + 52, by = 1)
YTOTAL = length(epst)

govMatrix <- matrix(0, ncol = NParties, nrow = YTOTAL)
colnames(govMatrix) <- colnames(poll)
ElectionWeeklyDates <- ceiling(as.numeric(Elections$Datum)/7)

govMatrix[,4][epst <= ElectionWeeklyDates[Elections$Year == 1998]] <- 1
govMatrix[,1][epst <= ElectionWeeklyDates[Elections$Year == 1998]] <- 1

govMatrix[,2][epst > ElectionWeeklyDates[Elections$Year == 1998] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2002]] <- 1
govMatrix[,3][epst > ElectionWeeklyDates[Elections$Year == 1998] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2002]] <- 1

govMatrix[,2][epst > ElectionWeeklyDates[Elections$Year == 2002] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2005]] <- 1
govMatrix[,3][epst > ElectionWeeklyDates[Elections$Year == 2002] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2005]] <- 1

govMatrix[,2][epst > ElectionWeeklyDates[Elections$Year == 2005] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2009]] <- 1
govMatrix[,1][epst > ElectionWeeklyDates[Elections$Year == 2005] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2009]] <- 1

govMatrix[,4][epst > ElectionWeeklyDates[Elections$Year == 2009] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2013]] <- 1
govMatrix[,1][epst > ElectionWeeklyDates[Elections$Year == 2009] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2013]] <- 1

govMatrix[,2][epst > ElectionWeeklyDates[Elections$Year == 2013] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2017]] <- 1
govMatrix[,1][epst > ElectionWeeklyDates[Elections$Year == 2013] & 
                epst <= ElectionWeeklyDates[Elections$Year == 2017]] <- 1

govMatrix[,2][epst > ElectionWeeklyDates[Elections$Year == 2017]] <- 1
govMatrix[,1][epst > ElectionWeeklyDates[Elections$Year == 2017]] <- 1

govMatrix <- t(govMatrix)

AllDates = match(X[,"Datum"], epst)
BTWDates = X[BTWIndikator == 0, 1]

BTWIndikator2 <- rep(1, YTOTAL)
BTWIndikator2[match(X[BTWIndikator == 0, "Datum"], epst) + 1] <- 0
BTWIndikator3 <- rep(1, YTOTAL)
BTWIndikator3[match(X[BTWIndikator == 0, "Datum"], epst)] <- 0


logistic <- function(x) exp(x) / (exp(x) + 1)

poll <- t(poll)

r <- stan_model(file = "~/Netzfreigaben/Git_TEX/BTW/btwXIV.stan")
f <- sampling(r, iter= 733, warmup = 400, chains = 3, cores = 3, seed = 124567,
              control = list(max_treedepth = 14, adapt_delta = 0.875))
save(f, file = "Test251016.RData")

fx <- rstan::extract(f)
samples <- 667:999
#Veranschaulichung
par(mfrow = c(3,2))
for(party in 1:NParties){
  plot(fx$y[samples,party,] %>% logistic %>% colMeans ~ epst, type = "l",
       ylim = c(0.03, 0.47), main = rownames(poll)[party], xlim=c(2000,2500))
  lines((fx$y[samples,party,] + fx$pollError[samples,party,]) %>% logistic %>% colMeans ~ epst, col ="orange")
  abline(v=BTWDates, lwd=1)
  lines(apply(fx$y[samples,party,] %>% logistic, 2, quantile, 0.025) ~ epst, lty = 2, col ="red")
  lines(apply(fx$y[samples,party,] %>% logistic, 2, quantile, 0.975) ~ epst, lty = 2, col ="red")
  points(logistic(poll[party,]) ~ x, cex = 0.25, col = "dodgerblue4", pch = 15)
  abline(v = max(x) + 1, lty = 2)
  abline(v = max(x) + 26, lty = 2)
}

apply(rstan::extract(f)$y[,1:NParties,epst == max(x) + 9] %>% logistic,2,mean)

plotData <- lapply(1:NParties, function(x){
  data.frame(estimate = fx$y[samples,x,] %>% logistic %>% colMeans,
             estimatePolls = (fx$y[samples,x,] + fx$pollError[samples,x,]) %>% logistic %>% colMeans,
             lower = apply(fx$y[samples,x,] %>% logistic, 2, quantile, 0.025),
             upper = apply(fx$y[samples,x,] %>% logistic, 2, quantile, 0.975),
             time = as.POSIXct(epst*60*60*24*7, origin = "1970-01-01"),
             party =rownames(poll)[x])
}) %>% bind_rows()
library(tidyr)
plotPollData <- data.frame(t(poll) %>% logistic,
                           time = as.POSIXct(epst[AllDates]*60*60*24*7, origin = "1970-01-01"))

plotPollData <- plotPollData %>% as_tibble %>% gather(key = "party", value = "proportion", -time)

ggplot(data = plotData, aes(x = time, y = estimate, group = party, colour = factor(party))) + geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(party)), alpha = 0.3, colour = NA) + 
  xlim(as.POSIXct(c("2017-01-01", "2017-09-30"))) + ylim(0,0.42) + 
  geom_vline(xintercept = as.POSIXct("2017-09-25")) + 
  geom_vline(xintercept = as.POSIXct(Date)) + 
  geom_point(data = plotPollData, aes(x = time, y = proportion, group = party))


ggplot(data = plotData, aes(x = time, y = estimate, group = party, colour = factor(party))) + geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = factor(party)), alpha = 0.3, colour = NA) + 
  xlim(as.POSIXct(c("2017-01-01", "2017-09-30"))) + ylim(0,0.18) + 
  geom_vline(xintercept = as.POSIXct("2017-09-25")) + 
  geom_vline(xintercept = as.POSIXct(Date)) + 
  geom_point(data = plotPollData, aes(x = time, y = proportion, group = party))



apply(extract(f)$y[,1:NParties,epst == max(x) + 4] %>% logistic,2,mean)
plot((extract(f)$pollError %>% colMeans) ~ epst, type = "l", col ="orange", ylim = c(-0.2, 0.2))
lines(apply(extract(f)$pollError, 2, quantile, 0.975) ~ epst, type = "l", col ="orange", lty = 2)
lines(apply(extract(f)$pollError, 2, quantile, 0.025) ~ epst, type = "l", col ="orange", lty = 2)
points((extract(f)$nu %>% colMeans) ~ epst, col = "red", type = "l")
abline(h = 0)
abline(v=BTWDates, lwd=1)

dev <- sapply(1:nrow(extract(f)$z), function(x) sum(((extract(f)$z[,AllDates][x,] %>% logistic) - logistic(w))^2) %>% sqrt)

pairs(f, pars = c("sigma_pollbias", "alpha","theta","theta2","sigma_shift" ,"lp__"))
