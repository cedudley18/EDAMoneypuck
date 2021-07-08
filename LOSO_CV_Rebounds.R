shots <- read.csv("shots_2007-2019.csv")


shots %>%
  mutate(arenaAdjustedShotDistance = as.numeric(shotDistance),
         shotGeneratedRebound = as.numeric(shotGeneratedRebound),
         shotAngleAdjusted = as.numeric(shotAngleAdjusted)) 


init_logit_rebounds <- glm(shotGeneratedRebound ~ arenaAdjustedShotDistance + shotAngleAdjusted,
                           data = shots,
                           family = "binomial")

pred_rebound_outcome <-
  ifelse(init_logit_rebounds$fitted.values >= 0.5, "No Rebound", "Rebound")
head(pred_rebound_outcome)
table(pred_rebound_outcome)

shots %>%
  ggplot(aes(init_logit_rebounds$fitted.values)) +
  geom_histogram() +
  theme_bw()


nhl_rebounds_loyo_cv_preds <-
  map_dfr(unique(shots_2013_2019$season),
          function(test_season) {
            
            # Separate out the test and training data:
            test_data <- shots %>%
              filter(season == test_season)
            
            train_data <- shots %>%
              filter(season != test_season)
            
            rebound_model <- glm(shotGeneratedRebound ~ shotDistance + shotAngleAdjusted,
                                 data = train_data,
                                 family = "binomial")
            
            tibble(test_pred_probs = predict(rebound_model, 
                                             newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$shotGeneratedRebound,
                   test_season = test_season) %>%
              return()
            
          })



nhl_rebounds_loyo_cv_preds %>%
  mutate(test_pred = as.numeric(test_pred_probs >= 0.5)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual))

nhl_rebounds_loyo_cv_preds %>%
  summarize(brier_score = mean((test_actual - test_pred_probs)^2))


nhl_rebounds_loyo_cv_preds %>%
  mutate(test_pred = as.numeric(test_pred_probs >= 0.5)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual)) %>%
  ggplot(aes(x = test_season, y = mcr)) +
  geom_bar(stat = "identity", width = .1) +
  geom_point(size = 5) +
  theme_bw() +
  scale_x_continuous(breaks = unique(nhl_rebounds_loyo_cv_preds$test_season))

summary(init_logit_rebounds)
