# 4. ----------------------------------------------------------------------

library(tidyverse)
recent_season <- read.csv("../CMSACamp_NHL_Project/data/2013-2019.csv")

# Sample the data
library(data.table)
set.seed(2021)
recent_season_w <- tibble(recent_season %>% sample_frac(0.05))

# Split the data
dat_list = split(recent_season, recent_season_w$season)

#Run logistic regression for each season
init_logit <- map(dat_list, ~glm(goal ~ shotAngleAdjusted+arenaAdjustedShotDistance,
                                 data = .x,
                                 family = binomial("logit")) )


distance<- rep(NA,7)
angle<-rep(NA,7)

for (i in 1:7){
  distance[i] <- init_logit[[i]]$coefficients[3]
  angle[i]<- init_logit[[i]]$coefficients[2]
}

coefficients<- data.frame(season=2013:2019,distance_c= distance, angle_c = angle)

ggplot(coefficients,aes(x=season,y=distance_c))+
  geom_point()+
  geom_line()+
  labs(title = "Distance")
  
ggplot(coefficients,aes(x=season,y=angle_c))+
  geom_point()+
  geom_line()+
  labs(title = "Angle")

# Printing ggplot within for-loop
for(i in 1:7) {                             
  print(dat_list[[i]] %>%
          mutate(pred_prob =  init_logit[[i]]$fitted.values,
                 bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
          # Group by bin_pred_prob:
          group_by(bin_pred_prob) %>%
          # Calculate the calibration results:
          summarize(n_attempts = n(),
                    bin_actual_prob = mean(goal)) %>%
          ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
          geom_point(aes(size = n_attempts)) +
          geom_smooth(method = "loess", se = FALSE) +
          geom_abline(slope = 1, intercept = 0, 
                      color = "black", linetype = "dashed") +
          coord_equal() + 
          scale_x_continuous(limits = c(0,1)) + 
          scale_y_continuous(limits = c(0,1)) + 
          labs(size = "Number of attempts",
               x = "Estimated make probability",
               y = "Observed make probability",
               title = as.character(i+2012)) + 
          theme_bw() +
          theme(legend.position = "bottom"))
  
  Sys.sleep(2)
}