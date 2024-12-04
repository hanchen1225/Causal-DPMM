source('cln_preprocessing.R')
library(magrittr)
library(ggVennDiagram)
library(naniar)
library(VIM)
library(factoextra)
theme_set(theme_light())
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
################################################################################
# Adjacency matrix
hist(friend.internum)
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


