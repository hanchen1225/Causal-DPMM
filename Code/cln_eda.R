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
library(ggVennDiagram)
library(naniar)
library(VIM)
library(factoextra)
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
sum(survey_id %in% all_id) # 4902
all_network_id <- unique(df.allinforawnet$network_id[(df.allinforawnet$network_id != '99') & (!is.na(df.allinforawnet$network_id))]) # 7246
df.allinforawnet %>%
  filter((is.na(network_id)) | (network_id == '99')) %>%
  nrow() # 623
nrow(distinct(df.structure)) # 4661
sum(all_id %in% all_network_id) # 4524
sum(all_network_id %in% all_id) # 4524
ggVennDiagram(list(id = all_id, network_id = all_network_id), label_alpha = 0) +
  coord_flip()
no_friends_in_id <- all_id[!all_id %in% all_network_id] # 460
having_friends_out_id <- df.allinforawnet %>%
  filter(!is.na(network_id)) %>%
  filter(network_id != '99') %>%
  pull(., id) %>%
  unique() # 4660
no_friends_out_id <- all_id[!all_id %in% having_friends_out_id] # 324
ggVennDiagram(list(no_friends_in = no_friends_in_id, no_friends_out = no_friends_out_id),
              label_alpha = 0) + coord_flip()
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
#
# village v.s. clusters
df.allinforawnet %>%
  filter(!is.na(network_village)) %>%
  filter(network_village != village) %>%
  select(village, id, network_village) %>%
  distinct() # 118 rows
df.allinforawnet %>%
  filter(!is.na(network_address)) %>%
  filter(network_address != address) %>%
  select(address, id, network_address) %>%
  distinct() # 1253 rows
#
# missing pattern in df.survey
vis_miss(df.survey)
vis_miss(df.survey[, c('disaster_yes', 'disaster_loss', 'reveal')])
n_var_miss(df.survey) # 11
gg_miss_upset(df.survey, nsets = 11, nintersects = 55)
gg_miss_case_cumsum(df.survey, breaks = 500)
gg_miss_var_cumsum(df.survey)
gg_miss_which(df.survey)
gg_miss_case(df.survey)
gg_miss_case(df.survey, facet = takeup_survey)
gg_miss_case(df.survey, facet = insurance_repay)
gg_miss_fct(x = df.survey, fct = reveal)
gg_miss_fct(x = df.survey, fct = disaster_loss)
marginplot(df.survey[, c('disaster_loss', 'educ')]) # MAR
marginplot(df.survey[, c('disaster_loss', 'rice_inc')]) # MNAR ???
marginplot(df.survey[, c('disaster_loss', 'ricearea_2010')]) # MAR
marginplot(df.survey[, c('disaster_loss', 'understanding')]) # MAR
marginplot(df.survey[, c('disaster_loss', 'reveal')]) # MAR
marginplot(df.survey[, c('disaster_loss', 'disaster_yes')])
sum(df.survey$id[is.na(df.survey$disaster_yes)] %in% df.survey$id[is.na(df.survey$disaster_loss)]) # 449
sum(df.survey$id[is.na(df.survey$disaster_loss)] %in% df.survey$id[is.na(df.survey$reveal)]) # 1636
# data frame for EDA (remove delete reveal & disaster_loss)
df.eda.NOimp <- df.survey[, -which(names(df.survey) %in% c('id', 'disaster_loss', 'reveal'))]
df.eda.NOimp <- df.eda.NOimp[complete.cases(df.eda.NOimp), ]
df.eda.NOimp %<>% mutate_at(c(4, 7, 10, 11, 13:21, 23:26), factor)
df.eda.NOimp$understanding[df.eda.NOimp$understanding == '1.1920929e-08'] <- factor('0.2')
df.eda.NOimp$risk_averse[df.eda.NOimp$risk_averse == '0.19999999'] <- factor('0.2')
levels(df.eda.NOimp$risk_averse) <- c(levels(df.eda.NOimp$risk_averse), '0.4')
df.eda.NOimp$risk_averse[df.eda.NOimp$risk_averse == '0.39999998'] <- factor('0.4')
df.eda.NOimp <- droplevels(df.eda.NOimp)
summary(df.eda.NOimp)
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
hist(friend.internum)
friend.num <- rowSums(friend.mis.sym)
friend.inter <- ifelse(friend.internum > 0, 1, 0)
friend.interate <- ifelse(friend.num > 0, friend.internum / friend.num, 0)
hist(friend.interate)
################################################################################
# EDA
length(unique(df.allinforawnet$address)) # 176
length(unique(df.allinforawnet$region)) # 4, '1', '2', '3', NA
length(unique(df.allinforawnet$village)) # 50
length(unique(df.survey$address)) # 173
length(unique(df.survey$region)) # 3, '1', '2', '3'
length(unique(df.survey$village)) # 47
df.survey.nNA <- df.survey[, -which(names(df.survey) %in% c("disaster_loss", "reveal", "disaster_yes"))]
df.survey.nNA <- df.survey.nNA[complete.cases(df.survey.nNA), ]
ggpairs(df.survey.nNA[, -(1:4)]) # the plot is too large...
corrplot(cor(df.survey.nNA[, -c(1:5, 19:21, 25:26)]), method = 'square', order = 'AOE', 
         addCoef.col = 'black', tl.pos = 'n', cl.pos = 'n', col = COL2('PiYG'))
corrplot(cor(df.survey.nNA[df.survey.nNA$delay == 1, c(5:16, 18:24)]), 
         method = 'square', order = 'AOE', 
         addCoef.col = 'black', tl.pos = 'n', cl.pos = 'n', col = COL2('PiYG'))
df.survey.nNA %>%
  filter(delay == 0) %>%
  select(c('info_none', 'info_takeup_rate', 'info_takeup_list')) %>%
  summary()
df.survey %>%
  filter(reveal == 1) %>%
  View()
df.survey %>%
  group_by(region) %>%
  count() # 2897, 1101, 904
df.survey %>%
  group_by(village) %>%
  count() %>%
  summary() # 11 ~ 104.3 ~ 272
df.survey %>%
  group_by(village) %>%
  count() %>%
  ggplot(aes(x = n)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 15, fill = 'skyblue') + 
  geom_density(colour = 'red')
df.survey %>% 
  group_by(address) %>%
  count() %>%
  ggplot(aes(x = n)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = 'skyblue') +
  geom_density(colour = 'red')
#
# regular EDA without imputation
ggplot(data = df.eda.NOimp, aes(x = takeup_survey, fill = takeup_survey)) +
  geom_bar() + 
  labs(title = "Insurance takeup", 
       subtitle = "(Group by region)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = intensive, fill = intensive)) +
  geom_bar() + 
  labs(title = "Participation in intensive session", 
       subtitle = "(Group by region)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = delay, fill = delay)) +
  geom_bar() + 
  labs(title = "Participation in second round", 
       subtitle = "(Group by region)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = educ, fill = educ)) +
  geom_bar() + 
  labs(title = "Education level", 
       subtitle = "(Group by region, 0 = illiteracy, 1 = primary, 2 = secondary, 3 = high school, 4 = college)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue", "yellow", "springgreen3", "violet")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = male, fill = male)) +
  geom_bar() + 
  labs(title = "Gender", 
       subtitle = "(Group by region, 0 = female, 1 = male)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = default, fill = default)) +
  geom_bar() + 
  labs(title = "Defaul choice", 
       subtitle = "(Group by region, 0 = not buy, 1 = buy)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = info_none, fill = info_none)) +
  geom_bar() + 
  labs(title = "No information about the first round takeup", 
       subtitle = "(Group by region, 0 = false, 1 = true)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = info_takeup_rate, fill = info_takeup_rate)) +
  geom_bar() + 
  labs(title = "Rate of the first round takeup provided", 
       subtitle = "(Group by region, 0 = false, 1 = true)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = info_takeup_list, fill = info_takeup_list)) +
  geom_bar() + 
  labs(title = "List of name of the first round takeup provided", 
       subtitle = "(Group by region, 0 = false, 1 = true)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = understanding, fill = understanding)) +
  geom_bar() + 
  labs(title = "Post-session insurance knowledge score", 
       subtitle = "(Group by region)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue", "yellow", "springgreen3", "violet", "orange")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = risk_averse, fill = risk_averse)) +
  geom_bar() + 
  labs(title = "Risk aversion", 
       subtitle = "(Group by region, 0–1, 0 as risk loving and 1 as risk averse)",
       y = "Count", x = "") + 
  facet_wrap(~ region) +
  scale_fill_manual(values = c("red", "skyblue", "yellow", "springgreen3", "violet", "orange")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
#
ggplot(data = df.eda.NOimp, aes(x = disaster_yes, fill = disaster_yes)) +
  geom_bar() + 
  labs(title = "Any disaster happened last year (1 = yes, 0 = no)", 
       subtitle = "(Group by village)",
       y = "Count", x = "") + 
  facet_wrap(~ village) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
#
ggplot(data = df.eda.NOimp, aes(x = takeup_survey, fill = intensive)) + 
  geom_bar(position = "dodge") +
  labs(title = "Survey takeup",
       subtitle = "(Left: first round; right: second round)",
       y = "Count", x = "") +
  facet_wrap(~ delay) +
  scale_fill_manual(values = c("red", "skyblue")) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'right') # evidence for spill-over effect
#
ggplot(data = df.eda.NOimp, aes(x = age)) +
  geom_density() + 
  labs(title = "Age", 
       subtitle = "(Group by region)",
       y = "Density", x = "") + 
  facet_wrap(~ region) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = agpop)) +
  geom_density() + 
  labs(title = "Household size", 
       subtitle = "(Group by region)",
       y = "Density", x = "") + 
  facet_wrap(~ region) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = rice_inc)) +
  geom_density() + 
  labs(title = "Share of rice income in total income (percent)", 
       subtitle = "(Group by region)",
       y = "Density", x = "") + 
  facet_wrap(~ region) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = ricearea_2010)) +
  geom_density() + 
  labs(title = "Area of rice production (mu, 1 mu = 1/15 hectare)", 
       subtitle = "(Group by region)",
       y = "Density", x = "") + 
  facet_wrap(~ region) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')
ggplot(data = df.eda.NOimp, aes(x = disaster_prob)) +
  geom_density() + 
  labs(title = "Perceived probability of future disasters (percent)", 
       subtitle = "(Group by region)",
       y = "Density", x = "") + 
  facet_wrap(~ region) +
  theme(panel.background = element_rect(fill = 'gray98'), 
        panel.grid = element_blank(),
        legend.position = 'none')


