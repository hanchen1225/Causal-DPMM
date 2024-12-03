source('cln_eda.R')
library(latex2exp)
library(MCMCpack)
library(mvtnorm)
library(pgdraw)
### Gibbs sampling #############################################################
Bern_likelihood <- function(x, y, beta) {
  exp((x %*% beta)[1, 1]) ^ y / (1 + exp((x %*% beta)[1, 1]))
}
#
Gibbs_sampler <- function(X, y, alpha = 5, m = 1, mu0 = NULL, Sigma0 = NULL, S = 1e5, B = 9e4) {
  n <- length(y)
  p <- ncol(X)
  if(is.null(mu0)) mu0 <- rep(0, p)
  if(is.null(Sigma0)) Sigma0 <- diag(1, p, p)
  rho <- rep(1, n)
  beta <- t(t(rmvnorm(1, mu0, Sigma0)))
  rho.mcmc <- matrix(0, S, n)
  beta.mcmc <- vector("list", S)
  for (s in 1:S) {
    if (s %% 100 == 0) print(paste0('s = ', s))
    # updating rho
    for (i in 1:n) {
      beta.star <- NULL
      counts <- table(rho)
      c <- rho[i]
      counts[c] <- counts[c] - 1
      if (counts[c] > 0) {
        beta.star <- rmvnorm(m, mu0, Sigma0)
      } else {
        rho[rho > c] <- rho[rho > c] - 1
        counts <- counts[-c]
        beta.star <- t(as.matrix(beta[c, ]))
        beta <- beta[-c, ]
        if (!is.matrix(beta)) beta <- t(as.matrix(beta))
        if (m > 1) beta.star <- rbind(beta.star, rmvnorm(m - 1, mu0, Sigma0))
      }
      probs.exist <- counts
      for(k in 1:nrow(beta)) {
        probs.exist[k] <- probs.exist[k] * Bern_likelihood(as.matrix(X[i,]), y[i], beta[k, ])
      }
      if (m == 1) {
        probs.new <- alpha / m * Bern_likelihood(as.matrix(X[i, ]), y[i], t(beta.star))
      } else{
        probs.new <- apply(beta.star, 1, function(x) alpha / m * Bern_likelihood(as.matrix(X[i, ]), y[i], x))
      }
      probs <- c(probs.exist, probs.new)
      probs <- probs / sum(probs)
      rho[i] <- sample(1:length(probs), 1, prob = probs)
      if (rho[i] > nrow(beta)) {
        if (m == 1) {
          beta <- rbind(beta, beta.star)
        } else{
          beta <- rbind(beta, beta.star[rho[i] - nrow(beta), ])
        }
        rho[i] <- nrow(beta)
      }
    }
    rho.mcmc[s, ] <- rho
    # updating beta
    for (c in 1:nrow(beta)) {
      idx <- rho == c
      Xc <- as.matrix(X[idx, ])
      yc <- y[idx]
      if (sum(idx) > 1) {
        omega <- rep(0, length(yc))
        for (k in 1:length(yc)) {
          omega[k] <- pgdraw(1, (Xc[k, ] %*% t(beta[c, ]))[1, 1])
        }
        V <- solve(t(Xc) %*% diag(omega) %*% Xc + solve(Sigma0))
      } else {
        omega <- pgdraw(1, (Xc %*% beta[c, ]))
        V <- solve(omega * t(Xc) %*% Xc + solve(Sigma0))
      }
      mo <- V %*% (t(Xc) %*% (yc - 1 / 2) + solve(Sigma0, mu0))
      beta[c, ] <- rmvnorm(1, mo, V)
    }
    beta.mcmc[[s]] <- beta
  }
  rho.mcmc <- rho.mcmc[(B + 1) : S, ]
  beta.mcmc <- beta.mcmc[(B + 1) : S]
  
  return(list(rho = rho.mcmc, beta = beta.mcmc))
}
#
direct.idx <- df.survey.cmp$delay == 0
direct.X <- cbind(rep(1), df.survey.cmp[direct.idx, c("age", "agpop", "disaster_prob", "intensive", "risk_averse")])
direct.y <- df.survey.cmp$takeup_survey[direct.idx]
system.time(
  direct.dpm.mcmc <- Gibbs_sampler(direct.X, direct.y)
)
direct.rho.gs <- mcmc(direct.dpm.mcmc$rho)
summary(direct.rho.gs)
direct.mcmc.size <- effectiveSize(direct.rho.gs)
plot(1:length(direct.y), direct.mcmc.size, xlab = 'Household', ylab = 'Effective sample size')
ggplot(data = data.frame(direct.mcmc.size), mapping = aes(x = direct.mcmc.size)) +
  geom_histogram()
par(mfrow = c(3, 4))
idx <- sample(1:length(direct.y), 12)
for (i in sort(idx)) {
  plot(direct.dpm.mcmc$rho[, i], xlab = 'Iteration', ylab = paste('rho', i), type = 'l')
  acf(direct.dpm.mcmc$rho[, i], ylab = paste("rho", i), main = '')
}
plot(1:1e4, direct.dpm.mcmc$rho[, 1], type = 'l')
direct.rho.max <- max(direct.dpm.mcmc$rho)
direct.weight <- NULL
for (c in 1:direct.rho.max) {
  weight <- apply(direct.dpm.mcmc$rho, 1, function(x) mean(x == c))
  direct.weight <- cbind(direct.weight, weight)
}
direct.weight <- data.frame(direct.weight)
direct.weight.m <- apply(direct.weight, 2, max)
direct.weight.cut <- direct.weight[, direct.weight.m > .5]
matplot(x = 1:1e4, y = direct.weight.cut, type = "l", ylim = c(-1, 1))
autocorr.plot(direct.weight)
#
inter.idx <- (df.survey.cmp$delay == 1) & (df.survey.cmp$intensive == 0)
inter.X <- cbind(rep(1), df.survey.cmp[inter.idx, c("age", "agpop", "disaster_prob", "risk_averse")], friend.inter[inter.idx])
inter.y <- df.survey.cmp$takeup_survey[inter.idx]
system.time(
  inter.dpm.mcmc <- Gibbs_sampler(inter.X, inter.y)
)
inter.rho.gs <- mcmc(inter.dpm.mcmc$rho)
summary(inter.rho.gs)
inter.mcmc.size <- effectiveSize(inter.rho.gs)
plot(1:length(inter.y), inter.mcmc.size, xlab = 'Household', ylab = 'Effective sample size')
ggplot(data = data.frame(inter.mcmc.size), mapping = aes(x = inter.mcmc.size)) +
  geom_histogram()
inter.rho.max <- max(inter.dpm.mcmc$rho)
inter.weight <- NULL
for (c in 1:inter.rho.max) {
  weight <- apply(inter.dpm.mcmc$rho, 1, function(x) mean(x == c))
  inter.weight <- cbind(inter.weight, weight)
}
inter.weight <- data.frame(inter.weight)
autocorr.plot(inter.weight)
effectiveSize(inter.weight)
acf(inter.weight)
### analysis ###################################################################
direct.dpm.mcmc <- readRDS(file = 'direct.RData')
direct.X.n <- direct.X.y <- direct.X
direct.X.n[, 5] <- rep(0)
direct.X.y[, 5] <- rep(1)
direct.n <- length(direct.y)
S <- 5e3
direct.rho <- direct.dpm.mcmc$rho
direct.beta <- direct.dpm.mcmc$beta
direct_effect <- matrix(0, S, direct.n)
system.time(
  for (s in 1:S) {
    if(s %% 100 == 0) print(paste0('s = ', s))
    for (i in 1:direct.n) {
      c <- direct.rho[s, i]
      en <- exp((direct.beta[[s]][c, ] %*% t(direct.X.n[i, ]))[1, 1])
      ey <- exp((direct.beta[[s]][c, ] %*% t(direct.X.y[i, ]))[1, 1])
      pn <- en / (1 + en)
      py <- ey / (1 + ey)
      direct_effect[s, i] <- py - pn
    }
  }
)
global_direct_effect <- rowMeans(direct_effect)
mean(global_direct_effect) # 0.1329567
sqrt(var(global_direct_effect)) # 0.0240605
ggplot(data.frame(global_direct_effect), aes(x = global_direct_effect)) + 
  geom_histogram(fill = 'steelblue', color = 'deepskyblue4') +
  geom_segment(x = mean(global_direct_effect), y = 0, 
               xend = mean(global_direct_effect), yend = 700, 
               color = 'red', lty = 2) +
  labs(x = '', y = '', title = 'Global direct effect') +
  theme(text = element_text(size = 20))
hist(c(direct_effect))
mean(direct_effect) # .13
var(c(direct_effect)) #.03
#
inter.X.n <- inter.X.y <- inter.X
inter.X.n[, 6] <- rep(0)
inter.X.y[, 6] <- rep(1)
inter.n <- length(inter.y)
S <- 5e3
inter.rho <- inter.dpm.mcmc$rho
inter.beta <- inter.dpm.mcmc$beta
inter_effect <- matrix(0, S, inter.n)
system.time(
  for (s in 1:S) {
    if(s %% 100 == 0) print(paste0('s = ', s))
    for (i in 1:inter.n) {
      c <- inter.rho[s, i]
      en <- exp((inter.beta[[s]][c, ] %*% t(inter.X.n[i, ]))[1, 1])
      ey <- exp((inter.beta[[s]][c, ] %*% t(inter.X.y[i, ]))[1, 1])
      pn <- en / (1 + en)
      py <- ey / (1 + ey)
      inter_effect[s, i] <- py - pn
    }
  }
)
global_inter_effect <- rowMeans(inter_effect)
mean(global_inter_effect) # -0.005976781
sqrt(var(global_inter_effect)) # 0.0276668
ggplot(data.frame(global_inter_effect), aes(x = -global_inter_effect)) + 
  geom_histogram(fill = 'steelblue', color = 'deepskyblue4') +
  geom_segment(x = -mean(global_inter_effect), y = 0, 
               xend = -mean(global_inter_effect), yend = 700, 
               color = 'red', lty = 2) +
  labs(x = '', y = '', title = 'Global spill over effect') +
  theme(text = element_text(size = 20))
hist(c(inter_effect))
mean(inter_effect) 
