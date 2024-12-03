source('cln_eda.R')
library(MCMCpack)
library(mvtnorm)
library(rstan)
library(rstanarm)
################################################################################
# additive model
# direct: Takeup_ij ~ Bern(p1) or Bern(p0), depending on treatment or control
mean(df.survey$takeup_survey[df.survey.cmp$intensive == 1 & friend.inter == 0]) -
  mean(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 0]) # -0.009787351
mean(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 1]) -
  mean(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 0]) # -0.02368157
n10 <- sum(df.survey.cmp$intensive == 1 & friend.inter == 0) # 1531
n01 <- sum(df.survey.cmp$intensive == 0 & friend.inter == 1) # 908
n00 <- sum(df.survey.cmp$intensive == 0 & friend.inter == 0) # 1537
sqrt(var(df.survey$takeup_survey[df.survey.cmp$intensive == 1 & friend.inter == 0]) +
       var(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 0])) # 0.7042962
sqrt(var(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 1]) +
       var(df.survey$takeup_survey[df.survey.cmp$intensive == 0 & friend.inter == 0])) # 0.7032313
################################################################################
# logistic regression 
cmb.first.X <- cbind(df.survey.cmp[direct.idx, c('age', 'agpop', 'disaster_prob', 'risk_averse', 'intensive')], 
                     exposure = rep(0))
cmb.second.X <- cbind(df.survey.cmp[df.survey.cmp$delay == 1, c('age', 'agpop', 'disaster_prob', 'risk_averse', 'intensive')],
                      exposure = friend.interate[df.survey.cmp$delay == 1])
cmb.X <- rbind(cmb.first.X, cmb.second.X)
cmb.y <- c(df.survey.cmp$takeup_survey[direct.idx], df.survey.cmp$takeup_survey[df.survey.cmp$delay == 1])
cmb.df <- data.frame(cmb.X, takeup_survey = cmb.y)
#
additive_fit <- glm(takeup_survey ~ ., family = binomial(), data = cmb.df)
summary(additive_fit)
#
interactive_fit <- glm(takeup_survey ~ . + intensive * (age + agpop + disaster_prob + risk_averse) + exposure * (age + agpop + disaster_prob + risk_averse),
                       family = binomial(), data = cmb.df)
summary(interactive_fit)
################################################################################
# hierachical model
#
hier_village_fit <- stan_glmer(
  takeup_survey ~ intensive + exposure + (age + agpop + disaster_prob + risk_averse | village),
  data = data.frame(cmb.df, 
                    village = c(df.survey.cmp$village[df.survey.cmp$delay == 0], c(df.survey.cmp$village[df.survey.cmp$delay == 1]))
  ),
  family = binomial(),
  prior = student_t(1, 0, 1),
  chains = 4,
  iter = 5e3,
  cores = 4
)
summary(hier_village_fit, digits = 4)
#
hier_region_fit <- stan_glmer(
  takeup_survey ~ intensive + exposure + (age + agpop + disaster_prob + risk_averse | region),
  data = data.frame(cmb.df, 
                    region = c(df.survey.cmp$region[df.survey.cmp$delay == 0], c(df.survey.cmp$region[df.survey.cmp$delay == 1]))
  ),
  family = binomial(),
  prior = student_t(1, 0, 1),
  chains = 4,
  iter = 5e3,
  cores = 4
)
summary(hier_region_fit, digits = 4)

