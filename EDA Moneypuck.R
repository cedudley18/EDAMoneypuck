library(tidyverse)

skaters <- read_csv("skaters.csv")
shots <- read_csv("shots_2020.csv")

shots

# Logistic regression plots
shots %>%
  mutate(
         bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom")


hometeam <- subset(shots, team == "HOME")
# HOME TEAM
hometeam %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom")


#AWAY TEAM

awayteam <- subset(shots, team == "AWAY")
awayteam %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") 

# EVEN STRENGTH
# get number of attempts on graph not overlapping points
evenstrength <-  shots %>%
  filter(homeSkatersOnIce == 5,
        awaySkatersOnIce == 5)

evenstrength %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") 

# POWER PLAYS
powerplay <-  shots %>%
  filter(homeSkatersOnIce == 5,
         awaySkatersOnIce == 4,
         isHomeTeam == 1)

powerplay %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") +
  geom_text(aes(label = n_attempts))

penaltykill <-  shots %>%
  filter(homeSkatersOnIce == 4,
         awaySkatersOnIce == 5,
         isHomeTeam == 1)

penaltykill %>%
  mutate(bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") +
  geom_text(aes(label = n_attempts))


lefthand <- subset(shots, shooterLeftRight == "L")

lefthand %>%
  mutate(
    bin_pred_prob = round(xGoal / 0.05) * 0.05) %>%
  group_by(bin_pred_prob) %>%
  summarize(n_attempts = n(),
            bin_actual_prob = mean(goal)) %>%
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) + 
  geom_point(aes(size = n_attempts)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(intercept = 0, slope = 1, color = "darkred",
              linetype = "dashed") + 
  theme_bw() + 
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0, 1)) + 
  theme(legend.position = "bottom") 





# xG model logistic regression --------------------------------------------
shots %>%
  ggplot(aes(x = shotAngleAdjusted)) + 
  geom_histogram()

# Goals
shots <-
  shots %>% 
  mutate(shotDistance = as.numeric(shotDistance),
         goal = as.numeric(goal),
         shotAngleAdjusted = as.numeric(shotAngleAdjusted))
  
init_logit <- glm(goal ~ shotDistance + shotAngleAdjusted,
                  data = shots,
                  family = "binomial")
summary(init_logit)

pred_shot_outcome <- 
  ifelse(init_logit$fitted.values >= 0.5, "Goal", "NonGoal")

head(pred_shot_outcome)
table(pred_shot_outcome)

init_logit$fitted.values

shots %>%
ggplot(aes(x = init_logit$fitted.values)) +
  geom_histogram()


# Rebounds
shots <-
  shots %>% 
  mutate(shotDistance = as.numeric(shotDistance),
         goal = as.numeric(goal),
         shotAngleAdjusted = as.numeric(shotAngleAdjusted))

logit2 <- glm(shotRebound ~ shotDistance + shotAngleAdjusted,
                  data = shots,
                  family = "binomial")

pred_shot_outcome <- 
  ifelse(logit2$fitted.values >= 0.5, "Rebound", "NonRebound")
head(pred_shot_outcome)
table(pred_shot_outcome)

logit2$fitted.values

shots %>%
  ggplot(aes(x = logit2$fitted.values)) +
  geom_histogram()

# slide on ways to assess accuracy of logistic regression model - like brier score
# brier score
# bootstrap


# xG coefficient trends ---------------------------------------------------
shots2011 <- read_csv("shots_data/shots_2011.csv")
shots2012 <- read_csv("shots_data/shots_2012.csv")
shots2013 <- read_csv("shots_data/shots_2013.csv")
shots2014 <- read_csv("shots_data/shots_2014.csv")
shots2015 <- read_csv("shots_data/shots_2015.csv")
shots2016 <- read_csv("shots_data/shots_2016.csv")
shots2017 <- read_csv("shots_data/shots_2017.csv")
shots2018 <- read_csv("shots_data/shots_2018.csv")
shots2019 <- read_csv("shots_data/shots_2019.csv")
shots2020 <- read_csv("shots_2020.csv")

shots_list <- list(shots2011, shots2012, shots2013, shots2014, shots2015, 
                   shots2016, shots2017, shots2018, shots2019, shots2020)

for(i in 1:length(Listdf) {
  mutate(shotDistance = as.numeric(shotDistance),
         goal = as.numeric(goal),
         shotAngleAdjusted = as.numeric(shotAngleAdjusted))
  
  init_logit <- glm(goal ~ shotDistance + shotAngleAdjusted,
                    data = shots,
                    family = "binomial")
})

  
  recent_season <- read.csv("data/2013-2019.csv")

# Sample the data
library(data.table)
set.seed(2021)

recent_season_w <- recent_season %>% sample_frac(0.05)

# Split the data
dat_list = split(recent_season, recent_season_w$season)
dat_list

#Run logistic regression for each season
lm <- function(data){
  init_logit <- glm(goal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                    data = data,
                    family = binomial("logit"))
}

map_dfr(dat_list, lm)

shots2020 %>%
  group_by(shotType) %>%
  summarise(n = n())


