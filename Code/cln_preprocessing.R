library(ggplot2)
library(cowplot)
library(GGally)
library(corrplot)
library(latex2exp)
library(haven)
library(readr)
library(purrr)
library(dplyr)
library(tidyverse)
library(magrittr)
theme_set(theme_light())
################################################################################
# four randomizations
# - two at the household level
#   - Simple1, Intensive1, Simple2, Intensive 2
#   - Simple/Intensive2-Noinfo, Simple/Intensive2-Overall, Simple/Intensive2-Individual
# - two at the village level
#   - price variation = no / yes
#   - no price variation, 1st round default = buy / not buy   % IV
################################################################################
# The resulsts may be biased, since all rice-producing households were invited to 
# one of the sessions, but only 90 percent of them attended. Those attended and 
# those not attended may be two distinct populations.
################################################################################
# reading data
df.allinforawnet <- read_csv("../data/0422allinforawnet.csv", 
                             col_types = cols(...1 = col_skip(), region = col_character(), 
                                              id = col_character(), network_id = col_character()))
df.allinforawnet$network_id <- sapply(df.allinforawnet$network_id, 
                                      function(x) gsub('\\..*', '', x))
df.allinforawnet$network_id[df.allinforawnet$network_id == '99'] <- NA
summary(df.allinforawnet)
# ***variables: region, address, village, id, network_id, network_village, network_address,
#               takeup_survey, delay, intensive, understanding, network_missname
df.structure <- read_csv("../data/0422structure_all.csv", 
                         col_types = cols(...1 = col_skip(), id = col_character()))
summary(df.structure)
# ***variables: id, eigenvector, indegree, path_in_ind, path_out_ind
df.survey <- read_csv("../data/0422survey.csv", 
                      col_types = cols(...1 = col_skip(), id = col_character(),
                                       region = col_character()))
summary(df.survey)
# ***variables: region, address, village, id, takeup_survey, age, agpop, educ, rice_inc, 
#               ricearea_2010, disaster_loss, insurance_repay, insurance_buy,
#               disaster_prob, general_trust, male, default, delay, intensive, 
#               info_takeup_rate, info_takeup_list, info_none, understanding, day,
#               risk_averse, literacy, educ_good, reveal, disaster_yes
# Education (0 = illiteracy, 1 = primary, 2 = secondary, 3 = high school, 4 = college)
################################################################################
# pre-processing
all_id <- unique(df.allinforawnet$id) # all household id, 4984
survey_id <- unique(df.survey$id)
all_network_id <- unique(df.allinforawnet$network_id[(df.allinforawnet$network_id != '99') & (!is.na(df.allinforawnet$network_id))]) # 7246
no_friends_in_id <- all_id[!all_id %in% all_network_id] # 460
having_friends_out_id <- df.allinforawnet %>%
  filter(!is.na(network_id)) %>%
  filter(network_id != '99') %>%
  pull(., id) %>%
  unique() # 4660
no_friends_out_id <- all_id[!all_id %in% having_friends_out_id] 
friends_in <- df.allinforawnet %>%
  filter(network_id %in% all_id) %>%
  group_by(network_id) %>%
  summarise(in_degree = length(unique(id))) %>%
  rbind(., data.frame(network_id = no_friends_in_id, 
                      in_degree = rep(0)))
friends_out <- df.allinforawnet %>%
  filter(network_id != '99') %>%
  filter(!is.na(network_id)) %>%
  group_by(id) %>%
  summarise(out_degree = length(unique(network_id))) %>%
  rbind(., distinct(data.frame(id = no_friends_out_id, 
                               out_degree = rep(0))))
friends_summary <- merge(friends_in, friends_out, by.x = 'network_id', by.y = 'id')
# data frame for EDA
df.eda.NOimp <- df.survey[, -which(names(df.survey) %in% c('id', 'disaster_loss', 'reveal'))]
df.eda.NOimp <- df.eda.NOimp[complete.cases(df.eda.NOimp), ]
df.eda.NOimp %<>% mutate_at(c(4, 7, 10, 11, 13:21, 23:26), factor)
df.eda.NOimp$understanding[df.eda.NOimp$understanding == '1.1920929e-08'] <- factor('0.2')
df.eda.NOimp$risk_averse[df.eda.NOimp$risk_averse == '0.19999999'] <- factor('0.2')
levels(df.eda.NOimp$risk_averse) <- c(levels(df.eda.NOimp$risk_averse), '0.4')
df.eda.NOimp$risk_averse[df.eda.NOimp$risk_averse == '0.39999998'] <- factor('0.4')
df.eda.NOimp <- droplevels(df.eda.NOimp)
# 
# data frame containing only complete covariates
df.survey.mis <- df.survey[, -which(names(df.survey) %in% c('disaster_loss', 
                                                            'insurance_repay', 
                                                            'insurance_buy', 
                                                            'general_trust', 
                                                            'day', 
                                                            'literacy', 
                                                            'educ_good', 
                                                            'reveal')), ]
df.survey.mis <- df.survey.mis %>%
  filter(!is.na(age)) %>%
  filter(!is.na(agpop)) %>%
  filter(!is.na(male))
df.survey.cmp <- df.survey.mis[, colSums(is.na(df.survey.mis)) == 0]
################################################################################
# Adjacency matrix
friend.mis.list <- lapply(1:length(df.survey.mis$id), 
                          function(x) df.allinforawnet$network_id[df.allinforawnet$id == df.survey.mis$id[x]])
friend.mis.asym <- sapply(friend.mis.list, function(x) as.numeric(df.survey.mis$id %in% x))
colnames(friend.mis.asym) <- rownames(friend.mis.asym) <- df.survey.mis$id
friend.mis.sym <- matrix(as.numeric((friend.mis.asym + t(friend.mis.asym)) > 0), nrow(df.survey.mis))
colnames(friend.mis.sym) <- rownames(friend.mis.sym) <- df.survey.mis$id
friend.internum <- friend.mis.sym %*% (df.survey.mis$intensive * (df.survey.mis$delay == 0))
friend.internum[df.survey.mis$delay == 0] <- 0
friend.num <- rowSums(friend.mis.sym)
friend.inter <- ifelse(friend.internum > 0, 1, 0)
friend.interate <- ifelse(friend.num > 0, friend.internum / friend.num, 0)
