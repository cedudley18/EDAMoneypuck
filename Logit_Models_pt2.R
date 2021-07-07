
# Logistic Model for Plays in Zone ----------------------------------------


init_logit_Inside <- glm(shotPlayContinuedInZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                  data = shots,
                  family = binomial) 


#init_logit_2 <- glm(shotPlayContinuedInZone ~ shotAngleAdjusted+shotDistance,
 #                   data = shots,
  #                  family = "binomial")


#init_logit_3 <- glm(shotPlayContinuedInZone ~ shotAngle+shotDistance,
   #                 data = shots,
    #                family = binomial) 




# Logistic Model for shots frozen -----------------------------------------

init_logit_Froze <- glm(shotGoalieFroze ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                    data = shots,
                    family = "binomial")

init_logit_Froze_shotAngle <- glm(shotGoalieFroze ~ shotAngle+arenaAdjustedShotDistance,
                    data = shots,
                    family = "binomial")



# Logistic Model for Plays Outside Zone -----------------------------------

init_logit_Outside <- glm(shotPlayContinuedOutsideZone ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                        data = shots,
                        family = "binomial")


summary(init_logit_Froze)
summary(init_logit_Froze_shotAngle)



shots %>%
  mutate(pred_prob = init_logit_Froze$fitted.values) %>%
  ggplot(aes(x = init_logit_Froze$linear.predictors)) +
  geom_line(aes(y = pred_prob), color = "blue") +
  geom_point(aes(y = shotGoalieFroze), alpha = 0.25,
             color = "darkorange") +
  theme_bw()



pred_frozen_outcome <-
  ifelse(init_logit$fitted.values >= 0.5, "No Freeze", "Freeze")
head(pred_frozen_outcome)
table(pred_frozen_outcome)
#  Made  Miss 
# 10656   155 

table("Predictions" = pred_frozen_outcome, 
      "Observed" = shots$shotGoalieFroze)

mean(ifelse(init_logit_Froze$fitted.values >= 0.5, 1, 0) != shots$shotGoalieFroze)

mean(as.numeric(init_logit_Froze$fitted.values >= 0.5) != shots$shotGoalieFroze)

mean((shots$shotGoalieFroze - fitted(init_logit_Froze))^2)




