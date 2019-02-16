# Final Project for 301-1
# Initial EDA R script
# John Lee

# Load packages
library(tidyverse)
library(forcats)
library(modelr)
library(haven)
library(plm)


# Part 1: Import the data ------------------------------------------------------------------------------

getwd()
complete_gss_df <- readRDS("data/processed/complete_gss.rds")

# Create subsets of the data - one for men, one for women. This will be used during the modeling process
only_men <- complete_gss_df %>%
  filter(sex == 1)

only_women <- complete_gss_df %>%
  filter(sex == 2)


# Part 2: Check the distributions of continuous variable ----------------------------------------------------

# If the distribution is skewed, I may need to log the variable 

# Code to center the plot title as the default option
theme_update(plot.title = element_text(hjust = 0.5))

names(complete_gss_df)

# Check the dist of age
complete_gss_df %>% 
  ggplot(aes(x = age, y = ..density.., fill = wave_fct)) +
  geom_histogram() + 
  facet_wrap(~wave_fct) +
  ggtitle("Dist. of Age") +
  labs(x = "Age", y = "Density", fill = "Wave #")

# Check the dist of educ
complete_gss_df %>% 
  ggplot(aes(x = educ, y = ..density.., fill = wave_fct)) +
  geom_histogram() + 
  facet_wrap(~wave_fct) +
  ggtitle("Dist. of Years of Education") +
  labs(x = "Years of Education", y = "Density", fill = "Wave #")

# Check the dist of ln(family income)
complete_gss_df %>% 
  ggplot(aes(x = ln_famincome, y = ..density.., fill = wave_fct)) +
  geom_histogram() + 
  facet_wrap(~wave_fct) +
  ggtitle("Dist. of Ln(Family Income)") +
  labs(x = "Ln(Family Income)", y = "Density", fill = "Wave #")

# Check the dist of conserv_ideology
complete_gss_df %>% 
  ggplot(aes(x = conserv_ideol, y = ..density.., fill = wave_fct)) +
  geom_histogram() + 
  facet_wrap(~wave_fct) +
  ggtitle("Dist. of Conservative Ideology") +
  labs(x = "Conservative Ideology", y = "Density", fill = "Wave #")

# Check the dist of repub_partyid
complete_gss_df %>% 
  ggplot(aes(x = repub_partyid, y = ..density.., fill = wave_fct)) +
  geom_histogram() + 
  facet_wrap(~wave_fct) +
  ggtitle("Dist. of Repub. Party ID") +
  labs(x = "Republican Party ID", y = "Density", fill = "Wave #")


# Part 3a: Bivariate analyses (time-variant personal factors) --------------------------------------------

# i.e., of the time-variant predictors, which are more correlated with the DVs?

# personal factors list: religiosity, marital status 

# Plot 1a: rel. between religiosity and conservative ideology
complete_gss_df %>%
  ggplot(aes(x = religiosity_text, y = conserv_ideol, color = gender_text)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ gender_text) +
  ggtitle("Religiosity and Conservative Ideology") +
  labs(x = "Religiosity", y = "Conservative Ideology", color = "Gender")

# Plot 1b: rel. between religiosity and party ID
complete_gss_df %>%
  ggplot(aes(x = religiosity_text, y = repub_partyid, color = gender_text)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ gender_text) +
  ggtitle("Religiosity and Republican Party ID") +
  labs(x = "Religiosity", y = "Republican Party ID", color = "Gender")

# Plot 2a: rel. between marital status and conservative ideology
complete_gss_df %>%
  ggplot(aes(x = marital_text, y = conserv_ideol, color = gender_text)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ gender_text) +
  ggtitle("Marital Status and Conservative Ideology") +
  labs(x = "Marital Status", y = "Conservative Ideology", color = "Gender")

# Plot 2b: rel. between marital status and Republican party ID
complete_gss_df %>%
  ggplot(aes(x = marital_text, y = repub_partyid, color = gender_text)) + 
  geom_boxplot() +
  coord_flip() +
  facet_wrap(~ gender_text) +
  ggtitle("Marital Status and Republican Party ID") +
  labs(x = "Marital Status", y = "Republican Party ID", color = "Gender")


# Part 3b: Bivariate analyses (time-variant SES factors) --------------------------------------------

# i.e., of the time-variant predictors, which are more correlated with the DVs?

# Plot 1a: rel. between education and conservative ideology
complete_gss_df %>%
  ggplot(aes(x = educ, y = conserv_ideol, color = gender_text)) + 
  geom_jitter(alpha = .4) +
  geom_smooth(color = "black") +
  facet_wrap(~ gender_text) +
  ggtitle("Education and Conservative Ideology") +
  labs(x = "Years of Education", y = "Conservative Ideology", color = "Gender")

# Plot 1b: rel. between education and republican party ID
complete_gss_df %>%
  ggplot(aes(x = educ, y = repub_partyid, color = gender_text)) + 
  geom_jitter(alpha = .4) +
  geom_smooth(color = "black") +
  facet_wrap(~ gender_text) +
  ggtitle("Education and Republican Party ID") +
  labs(x = "Years of Education", y = "Republican", color = "Gender")


# Plot 2a: rel. between family income and conservative ideology
complete_gss_df %>%
  ggplot(aes(x = ln_famincome, y = conserv_ideol, color = gender_text)) + 
  geom_jitter(alpha = .4) +
  geom_smooth(color = "black") +
  facet_wrap(~ gender_text) +
  ggtitle("Ln(Family Income) and Conservative Ideology") +
  labs(x = "Ln(Family Income)", y = "Conservative Ideology", color = "Gender")

# Plot 2b: rel. between family income and republican party ID
complete_gss_df %>%
  ggplot(aes(x = ln_famincome, y = repub_partyid, color = gender_text)) + 
  geom_jitter(alpha = .4) +
  geom_smooth(color = "black") +
  facet_wrap(~ gender_text) +
  ggtitle("Ln(Family Income) and Republican Party ID") +
  labs(x = "Ln(Family Income)", y = "Republican Party ID", color = "Gender")

# Plot 3a: rel. between class identity and political ideology
complete_gss_df %>%
  ggplot(aes(x = class_text, y = conserv_ideol, color = gender_text)) + 
  geom_boxplot() +
  facet_wrap(~ gender_text) +
  ggtitle("Class Identity and Conservative Ideology") +
  labs(x = "Subjective Class Identity", y = "Conservative Ideology", color = "Gender")

# Plot 3b: rel. between class identity and republican party ID
complete_gss_df %>%
  ggplot(aes(x = class_text, y = repub_partyid, color = gender_text)) + 
  geom_boxplot() +
  facet_wrap(~ gender_text) +
  ggtitle("Class Identity and Republican Party ID") +
  labs(x = "Subjective Class Identity", y = "Republican Party ID", color = "Gender")

# Check class averages for polviews
new_gss_df %>%
  group_by(class) %>%
  summarize(avg_ideology = mean(polviews, na.rm = TRUE))


# Part 4a: Examine changes over time in the DV #1: polviews ----------------------------------------------

# In this section, I want to look at changes in the values of the DVs over time: first, for polviews; next, for partyid
#Note: I arranged by subject ID so that I can perform operations on the vectors of interest

# Visualize the changes over time (notice that it's almost impossible to tell what's going on -- except that there's quite a bit of movement)
complete_gss_df %>% 
  ggplot(aes(x = wave_fct, y = conserv_ideol, group = subject.ID, color = gender_text)) +
  geom_line(alpha = .5) +
  facet_wrap(~ gender_text) +
  ggtitle("Change Over Time: Conservative Ideology") +
  labs(x = "Wave", y = "Conservative Ideology", color = "Gender") 

# First, look at change over time for polviews

polviews_w1 <- complete_gss_df %>%
  filter(wave == 1) %>%
  select(subject.ID, polviews, gender_text) %>%
  arrange(subject.ID)

polviews_w2 <- complete_gss_df %>%
  filter(wave == 2) %>%
  select(subject.ID, polviews, gender_text) %>%
  arrange(subject.ID)

polviews_w3 <- complete_gss_df %>%
  filter(wave == 3) %>%
  select(subject.ID, polviews, gender_text) %>%
  arrange(subject.ID)

# Make sure the arrange worked (the result of the code below should be 0)
mean(polviews_w2$subject.ID - polviews_w1$subject.ID)
mean(polviews_w3$subject.ID - polviews_w2$subject.ID)

# Change in polviews from w1 to w2 
polviews_w1_to_w2 <- polviews_w2 %>%
  mutate(polviews_change = polviews_w2$polviews - polviews_w1$polviews) %>%
  select(subject.ID, polviews_change, gender_text)

# Change in polviews from w2 to w3 
polviews_w2_to_w3 <- polviews_w3 %>%
  mutate(polviews_change = polviews_w3$polviews - polviews_w2$polviews) %>%
  select(subject.ID, polviews_change, gender_text)

# Histogram of the changes in polviews from w1 to w2 
polviews_w1_to_w2 %>%
  ggplot(aes(x = polviews_change, y = ..density.., fill = gender_text)) +
  geom_histogram() +
  facet_wrap(~ gender_text) +
  ggtitle("Change Over Time (W1 to W2): Conservative Ideology") +
  labs(x = "Change in Conservative Ideology (W1 to W2)", y = "Density", fill = "Gender") 

# Histogram of the changes in polviews from w2 to w3
polviews_w2_to_w3 %>%
  ggplot(aes(x = polviews_change, y = ..density.., fill = gender_text)) +
  geom_histogram() +
  facet_wrap(~ gender_text) +
  ggtitle("Change Over Time (W2 to W3): Conservative Ideology") +
  labs(x = "Change in Conservative Ideology (W2 to W3)", y = "Density", fill = "Gender") 

# Avg change in polviews from w1 to w2 (abs diff)
mean(abs(polviews_w1_to_w2$polviews_change))

# Avg change in polviews from w2 to w3 (abs diff)
mean(abs(polviews_w2_to_w3$polviews_change))


# Part 4b: Examine changes over time in the DV #2: partyid ----------------------------------------------

# First, look at change over time for partyid

partyid_w1 <- complete_gss_df %>%
  filter(wave == 1) %>%
  select(subject.ID, partyid, gender_text) %>%
  arrange(subject.ID)

partyid_w2 <- complete_gss_df %>%
  filter(wave == 2) %>%
  select(subject.ID, partyid, gender_text) %>%
  arrange(subject.ID)

partyid_w3 <- complete_gss_df %>%
  filter(wave == 3) %>%
  select(subject.ID, partyid, gender_text) %>%
  arrange(subject.ID)

# Make sure the arrange worked (the result of the code below should be 0)
mean(partyid_w2$subject.ID - partyid_w1$subject.ID)
mean(partyid_w3$subject.ID - partyid_w2$subject.ID)

# Change in partyid from w1 to w2 
partyid_w1_to_w2 <- partyid_w2 %>%
  mutate(partyid_change = partyid_w2$partyid - partyid_w1$partyid) %>%
  select(subject.ID, partyid_change, gender_text)

# Change in partyid from w2 to w3 
partyid_w2_to_w3 <- partyid_w3 %>%
  mutate(partyid_change = partyid_w3$partyid - partyid_w2$partyid) %>%
  select(subject.ID, partyid_change, gender_text)

# Histogram of the changes in partyid from w1 to w2 
partyid_w1_to_w2 %>%
  ggplot(aes(x = partyid_change, y = ..density.., fill = gender_text)) +
  geom_histogram() +
  facet_wrap(~ gender_text) +
  ggtitle("Change Over Time (W1 to W2): Repub. Party ID") +
  labs(x = "Change in Republican Party ID (W1 to W2)", y = "Density", fill = "Gender") 

# Histogram of the changes in partyid from w2 to w3
partyid_w2_to_w3 %>%
  ggplot(aes(x = partyid_change, y = ..density.., fill = gender_text)) +
  geom_histogram() +
  facet_wrap(~ gender_text) +
  ggtitle("Change Over Time (W2 to W3): Repub. Party ID") +
  labs(x = "Change in Republican Party ID (W2 to W3)", y = "Density", fill = "Gender") 

# Avg change in partyid from w1 to w2 (abs diff)
mean(abs(partyid_w1_to_w2$partyid_change))

# Avg change in partyid from w2 to w3 (abs diff)
mean(abs(partyid_w2_to_w3$partyid_change))


# Part 5a: models for conserv_ideol ------------------------------------------------------------------

# note: all models are linear FE models using the plm package 
# format: mod1 = full dataset; mod2 = only men; mod3 = only women. Model specifications are otherwise the same

# conserv_ideol mod1: full dataset
conserv_mod1 <-plm(conserv_ideol ~ divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = complete_gss_df, index=c("subject.ID", "wave"), model="within")
conserv_mod1 %>%
  summary()

# conserv_ideol mod2: only men
conserv_mod2 <-plm(conserv_ideol ~ divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = only_men, index=c("subject.ID", "wave"), model="within")
conserv_mod2 %>%
  summary()

# conserv_ideol mod3: only women
conserv_mod3 <-plm(conserv_ideol ~ divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = only_women, index=c("subject.ID", "wave"), model="within")
conserv_mod3 %>%
  summary()


# Part 5b: models for repub_partyid ------------------------------------------------------------------

# note: all models are linear FE models using the plm package 
# format: mod1 = full dataset; mod2 = only men; mod3 = only women. Model specifications are otherwise the same

# repub_partyid mod1: full dataset
repub_mod1 <-plm(repub_partyid ~ conserv_ideol + divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = complete_gss_df, index=c("subject.ID", "wave"), model="within")
repub_mod1 %>%
  summary()

# repub_partyid mod2: only men
repub_mod2 <-plm(repub_partyid ~ conserv_ideol + divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = only_men, index=c("subject.ID", "wave"), model="within")
repub_mod2 %>%
  summary()

# repub_partyid mod3: only women
repub_mod3 <-plm(repub_partyid ~ conserv_ideol + divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = only_women, index=c("subject.ID", "wave"), model="within")
repub_mod3 %>%
  summary()

mean(only_men$repub_partyid)

# Part 6(a): Assess model quality: for repub_mod1 (full sample) -------------------------------------

# Note: Since there are only statistically significant findings for the repub_partyid models, I'll just assess model fit for those. 

# Save the residuals for mod1
repub_mod1_check <- complete_gss_df %>%
  mutate(repub_mod1_resid = residuals(repub_mod1))

# Add the predicted (i.e., fitted) values 
repub_mod1_check <- repub_mod1_check %>%
  mutate(pred_values = repub_partyid - repub_mod1_resid)

# Plot the distribution of the residuals
repub_mod1_check %>%
  ggplot(aes(x = repub_mod1_resid)) + 
  geom_histogram()  + 
  ggtitle("Model 1 Residuals") +
  labs(x = "Residuals", y = "Count")

# Plot the residuals wrt ideology, which is the primary predictor of interest
repub_mod1_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_mod1_resid)) + 
  geom_jitter(alpha = .4) +
  geom_ref_line(h = 0) +
  ggtitle("Model 1 Residuals") +
  labs(x = "Conservative Ideology", y = "Residuals")

# Plot the residuals wrt ideology, which is the primary predictor of interest (by gender)
repub_mod1_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_mod1_resid, color = gender_text)) + 
  geom_jitter(alpha = .4) +
  geom_ref_line(h = 0) +
  facet_wrap(~ gender_text) +
  ggtitle("Model 1 Residuals") +
  labs(x = "Conservative Ideology", y = "Residuals", color = "Gender")

# Plot predicted values v. actuals (black: actuals, purple: predicted)
repub_mod1_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_partyid)) + 
  geom_jitter(alpha = .4) +
  geom_jitter(aes(y = pred_values), color = "purple", alpha = .4) +   
  ggtitle("Model 1 (All): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")

# Plot predicted values v. actuals (using boxplots)
grid_mod1 <- repub_mod1_check %>% 
  group_by(conserv_ideol) %>%
  summarize(avg_pred_DV = mean(pred_values))

repub_mod1_check %>%
  ggplot(aes(x = as.factor(conserv_ideol), y = repub_partyid)) + 
  geom_boxplot() +
  geom_point(aes(y = avg_pred_DV), data = grid_mod1, color = "purple", size = 3) +   
  ggtitle("Model 1 (All): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")

# predicted v. actuals (using boxplots) - segment by gender
repub_mod1_check %>%
  ggplot(aes(x = as.factor(conserv_ideol), y = repub_partyid, color = gender_text)) + 
  geom_boxplot() +
  facet_wrap(~ gender_text) +
  geom_point(aes(y = avg_pred_DV), data = grid_mod1, color = "purple", size = 3) +   
  ggtitle("Model 1 (All): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID", color = "Gender")


# Part 6(b): Assess model quality: for repub_mod2 (men only) -------------------------------------

# Note: Since there are only statistically significant findings for the repub_partyid models, I'll just assess model fit for those. 

# Save the residuals for mod2
repub_mod2_check <- only_men %>%
  mutate(repub_mod2_resid = residuals(repub_mod2))

# Add the predicted (i.e., fitted) values 
repub_mod2_check <- repub_mod2_check %>%
  mutate(pred_values = repub_partyid - repub_mod2_resid)

# Plot the distribution of the residuals
repub_mod2_check %>%
  ggplot(aes(x = repub_mod2_resid)) + 
  geom_histogram() + 
  ggtitle("Model 2 (Men Only) Residuals") +
  labs(x = "Residuals", y = "Count")

# Plot the residuals wrt ideology, which is the primary predictor of interest
repub_mod2_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_mod2_resid)) + 
  geom_jitter(alpha = .4) +
  geom_ref_line(h = 0) +
  ggtitle("Model 2 (Men Only) Residuals") +
  labs(x = "Conservative Ideology", y = "Residuals")

# Plot predicted values v. actuals (black: actuals, purple: predicted)
repub_mod2_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_partyid)) + 
  geom_jitter(alpha = .4) +
  geom_jitter(aes(y = pred_values), color = "purple", alpha = .4) +   
  ggtitle("Model 2 (Men Only): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")

# Plot predicted values v. actuals (using boxplots)
grid_mod2 <- repub_mod2_check %>% 
  group_by(conserv_ideol) %>%
  summarize(avg_pred_DV = mean(pred_values))

repub_mod2_check %>%
  ggplot(aes(x = as.factor(conserv_ideol), y = repub_partyid)) + 
  geom_boxplot() +
  geom_point(aes(y = avg_pred_DV), data = grid_mod2, color = "purple", size = 3) +   
  ggtitle("Model 2 (Men Only): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")


# Part 6(c): Assess model quality: for repub_mod3 (women only) -------------------------------------

# Note: Since there are only statistically significant findings for the repub_partyid models, I'll just assess model fit for those. 

# Save the residuals for mod3
repub_mod3_check <- only_women %>%
  mutate(repub_mod3_resid = residuals(repub_mod3))

# Add the predicted (i.e., fitted) values 
repub_mod3_check <- repub_mod3_check %>%
  mutate(pred_values = repub_partyid - repub_mod3_resid)

# Plot the distribution of the residuals
repub_mod3_check %>%
  ggplot(aes(x = repub_mod3_resid)) + 
  geom_histogram() + 
  ggtitle("Model 3 (Women Only) Residuals") +
  labs(x = "Residuals", y = "Count")

# Plot the residuals wrt ideology, which is the primary predictor of interest
repub_mod3_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_mod3_resid)) + 
  geom_jitter(alpha = .4) +
  geom_ref_line(h = 0) +
  ggtitle("Model 3 (Women Only) Residuals") +
  labs(x = "Conservative Ideology", y = "Residuals")

# Plot predicted values v. actuals (black: actuals, purple: predicted)
repub_mod3_check %>%
  ggplot(aes(x = conserv_ideol, y = repub_partyid)) + 
  geom_jitter(alpha = .4) +
  geom_jitter(aes(y = pred_values), color = "purple", alpha = .4) +   
  ggtitle("Model 3 (Women Only): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")

# Plot predicted values v. actuals (using boxplots)
grid_mod3 <- repub_mod3_check %>% 
  group_by(conserv_ideol) %>%
  summarize(avg_pred_DV = mean(pred_values))

repub_mod3_check %>%
  ggplot(aes(x = as.factor(conserv_ideol), y = repub_partyid)) + 
  geom_boxplot() +
  geom_point(aes(y = avg_pred_DV), data = grid_mod3, color = "purple", size = 3) +   
  ggtitle("Model 3 (Women Only): Predicted v. Actual") +
  labs(x = "Conservative Ideology", y = "Republican Party ID")



test_mod <-plm(repub_partyid ~ conserv_ideol + age + divorced + no_religion + educ + ln_famincome + lower_classid + unempl + wave, data = only_women, index=c("subject.ID", "wave"), model="within")
test_mod %>%
  summary()
