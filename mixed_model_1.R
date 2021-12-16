install.packages("lme4")
install.packages("lmerTest")
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)

gender_height_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/gender_height_data.csv")
#use mutate to turn subject and gender collum to factors
gender_height_data <- gender_height_data %>%
  mutate(subject = factor(subject),
         gender = factor(gender))
head(gender_height_data)
#plot data to check linear relationship
set.seed(1234)
gender_height_data %>%
  ggplot(aes(x = gender, y = height)) +
  geom_jitter(width = .1) +
  theme_minimal() +
  labs(x = "Gender", y = "Height (cm)") +
  scale_x_discrete(labels = c("Female", "Male"))

height_model <- lm(height ~ gender, data = gender_height_data)
summary(height_model)
#see from output that mean height of females is 165 and mean height of males is 12.5cm more
#p=0.017

#now we have 2 continuous variables- age and height
age_height_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/age_height_data.csv")

age_height_data %>%
  ggplot(aes(x = age, y = height)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Age (years)", y = "Height (cm)")

age_model <- lm(height ~ age, data = age_height_data)
summary(age_model)
#interpret parameters differently with continuous predicitors so the intercept 
#= someones height at 0 and 2.398 for age corresponds to avg growth per year- but the model reall only captures linear relationship 
#for age and height for the age range we have data for as relationship between height and age is not linear across life

#mixed models
mixed_model_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/15_mixed_models_pt1/master/data/mixed_model_data.csv")

mixed_model_data <- mixed_model_data %>%
  mutate(subject = factor(subject),
         item = factor(item),
         condition = factor(condition))
head(mixed_model_data)
#condition = large or small,also random effects of items and subjects- dv = RT
mixed_model_data %>% 
  group_by(condition) %>%
  summarise(mean_rt = mean(rt), sd_rt = sd(rt))

#****build mixed model as fixed effect and subject and item as random effecrs
mixed_model <- lmer(rt ~ condition + (1 | subject) + (1 | item), 
                    data = mixed_model_data)
summary(mixed_model)

# intercept is at 854.14= mean RT for large condition
##-49.78 corresponds between difference in large and small- small is 49ms faster

#design is fine for one factor- but not with factorial designs with interaction effects
#for that we would need to use contrast coding

#use liklihood ratio test LRT- determine whether model containing fixed effect of condition is better than model only with random effects
mixed_model_null <- lmer(rt ~ (1 | subject) + (1 | item), 
                         data = mixed_model_data)
#compare the two models
anova(mixed_model, mixed_model_null)
#models do differ- model with fixed effect of condition explains more variablility in data

#now build a model modelling slope of random effects
mixed_model_slopes <- lmer(rt ~ condition + (1 + condition | subject)
                           + (1 + condition | item), data = mixed_model_data)
summary(mixed_model_slopes)

#We can see that with a more complex random effect structure 
#(i.e., random slopes as well as intercepts), the effect of our fixed effect of condition is still clearly there (and it is significant).
#next model looks at when there is a factor with 3 levels