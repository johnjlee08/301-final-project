# Final Project for 301-1
# Data cleaning R script
# John Lee

# Load packages
library(tidyverse)
library(forcats)
library(modelr)
library(haven)
library(xlsx)

# Check wd
getwd()


## Part 1: Import and Reshape the data ------------------------------------------------------------

# Import the df from Stata 
gss_df <- read_dta("data/unprocessed/GSS_panel2010w123_R6 - stata.dta") 

# Check the contents of the file 
head(gss_df)

# Add a var for subject ID
gss_df <- gss_df %>%
  mutate(subject.ID = row_number()) %>%
  # Reorder the variables, so subject ID appears first
  select(subject.ID, everything())

# Note: after col 15, 3 cols per variable (1 per wave). Need to be reshaped 
head(gss_df[1:15])
head(gss_df[16:25])

# Reshape cols after col 15 from wide to long
gss_pt2_df <- select(.data = gss_df, subject.ID, names(gss_df[16:length(gss_df)])) %>%
  gather(contains("_"), key = "wave", value = "var.value") %>% 
  separate(col = wave, into = c("var.name", "wave"), sep = "_") %>%
  spread(key = var.name, value = var.value)
head(gss_pt2_df)

# Merge with pt 1 of the original df (first 15 cols) 
new_gss_df <- gss_df %>%
  select(names(gss_df[1:15])) %>%
  inner_join(gss_pt2_df, by = "subject.ID")

# Only select variables of interest
new_gss_df <- new_gss_df %>%
  select(subject.ID, wave, age, sex, race, marital, childs, relig, reliten, educ, wrkstat, income06, class, polviews, partyid)
head(new_gss_df)


# Part 2: Recode the values of the variables as needed -----------------------------------------------

# Recode values of age (98 = Don't know, 99 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(age = ifelse(age == 98 | age == 99, NA, age))
new_gss_df %>%
  filter(age > 85) %>%
  count(age)

# Recode values of marital status (9 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(marital = ifelse(marital == 9, NA, marital)) 
new_gss_df %>%
  count(marital)

# Generate marital status text function
gen_marital_text <- function(x){
  case_when(
    x == 1 ~ "Married",
    x == 2 ~ "Widowed",
    x == 3 ~ "Divorced",
    x == 4 ~ "Separated",
    x == 5 ~ "Never Married"
  )
}

# Create marital status var w/ text values 
new_gss_df <- new_gss_df %>%
  mutate(marital_text = gen_marital_text(marital)) %>%
  # Modify factor level order 
  mutate(marital_text = fct_relevel(marital_text, "Never Married", "Married", "Separated", "Divorced", "Widowed"))
new_gss_df %>%
  count(marital_text)

# Recode values of # of kids (9 = don’t know or NA)
new_gss_df <- new_gss_df %>%
  mutate(childs = ifelse(childs == 9, NA, childs)) 
new_gss_df %>%
  count(childs)

# Recode values of religion (98 = Don't know, 99 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(relig = ifelse(relig == 98 | relig == 99, NA, relig)) 
new_gss_df %>%
  count(relig)

# Recode values of religiosity (8 = Don't know, 9 = No answer, 0 = Not applicable)
new_gss_df <- new_gss_df %>%
  mutate(reliten = ifelse(reliten > 4 | reliten == 0, NA, reliten)) 
new_gss_df %>%
  count(reliten)

# Generate religiosity text function
gen_religiosity_text <- function(x){
  case_when(
    x == 1 ~ "Strong",
    x == 2 ~ "Not Very Strong",
    x == 3 ~ "Somewhat Strong",
    x == 4 ~ "No Religion"
  )
}

# Create religiosity var w/ text values 
new_gss_df <- new_gss_df %>%
  mutate(religiosity_text = gen_religiosity_text(reliten)) %>%
  # Modify factor level order 
  mutate(religiosity_text = fct_relevel(religiosity_text, "No Religion", "Somewhat Strong", "Not Very Strong", "Strong"))
new_gss_df %>%
  count(religiosity_text)

# Recode values of years of education (98 = Don't know, 99 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(educ = ifelse(educ == 98 | educ == 99, NA, educ)) 
new_gss_df %>%
  count(educ)

# Recode values of employment status (9 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(wrkstat = ifelse(wrkstat == 9, NA, wrkstat)) 
new_gss_df %>%
  count(wrkstat)

# Convert to midpoint function
convert_midpoint <- function(x){
  case_when(
    x == 1 ~ 500,
    x == 2 ~ 2000,
    x == 3 ~ 3500,
    x == 4 ~ 4500,
    x == 5 ~ 5500,
    x == 6 ~ 6500,
    x == 7 ~ 7500,
    x == 8 ~ 9000,
    x == 9 ~ 11250,
    x == 10 ~ 13750,
    x == 11 ~ 16250,
    x == 12 ~ 18750,
    x == 13 ~ 21250,
    x == 14 ~ 23750,
    x == 15 ~ 27500,
    x == 16 ~ 32500,
    x == 17 ~ 37500,
    x == 18 ~ 45000,
    x == 19 ~ 55000,
    x == 20 ~ 67500,
    x == 21 ~ 82500,
    x == 22 ~ 100000,
    x == 23 ~ 120000,
    x == 24 ~ 140000,
    x == 25 ~ 150000
  )
}

# Recode values of family income (26 = Refused, 98 = Don't know, 0 = Not applicable)
new_gss_df <- new_gss_df %>%
  mutate(income06 = convert_midpoint(income06)) 
new_gss_df %>%
  count(income06)

# Recode values of class identity (5 = No class, 8 = Don't know, 9 = No answer, 0 = Not applicable)
new_gss_df <- new_gss_df %>%
  mutate(class = ifelse(class > 4, NA, class)) 
new_gss_df %>%
  count(class)

# Generate class text function
gen_class_text <- function(x){
  case_when(
    x == 1 ~ "Lower",
    x == 2 ~ "Working",
    x == 3 ~ "Middle",
    x == 4 ~ "Upper"
  )
}

# Create class identity var w/ text values 
new_gss_df <- new_gss_df %>%
  mutate(class_text = gen_class_text(class)) %>%
# Modify factor level order 
  mutate(class_text = fct_relevel(class_text, "Lower", "Working", "Middle", "Upper"))
new_gss_df %>%
  count(class_text)

# Recode values of party ID (7 = Other party, 8 = Don't know, 9 = No answer)
new_gss_df <- new_gss_df %>%
  mutate(partyid = ifelse(partyid > 6, NA, partyid)) %>%
  # Add 1 to each value, to keep the 1-7 point scale consistent with the other DV
  mutate(partyid = partyid + 1)
new_gss_df %>%
  count(partyid)

# Recode values of political ideology (8 = Don't know, 9 = No answer, 0 = Not applicable)
new_gss_df <- new_gss_df %>%
  mutate(polviews = ifelse(polviews > 7 | polviews == 0, NA, polviews)) 
new_gss_df %>%
  count(polviews)


# Part 3: Create an updated df w/ no missing data + only subjects who completed all 3 waves ----------------

# Compute the % of observations with no missing values
mean(complete.cases(new_gss_df))

# Delete the observations with missing values 
no_na_df <- new_gss_df[complete.cases(new_gss_df),]

# Identify the subjects for whom we have complete data for all 3 waves 
completed_w1 <- no_na_df %>%
  filter(wave == 1) %>%
  select(subject.ID) %>% 
  as.list()

completed_w2 <- no_na_df %>%
  filter(wave == 2) %>%
  select(subject.ID) %>%
  as.list()

completed_w3 <- no_na_df %>%
  filter(wave == 3) %>%
  select(subject.ID) %>%
  as.list()

# Create a variable that indicates if the subject has complete data for all 3 waves 
complete_gss_df <- no_na_df %>%
  mutate(complete_3w = ifelse(subject.ID %in% unlist(completed_w1) & subject.ID %in% unlist(completed_w2) 
                              & subject.ID %in% unlist(completed_w3), 1, 0))

# Only include the subjects with complete data for all 3 waves
complete_gss_df <- complete_gss_df %>% 
  filter(complete_3w == 1)

# Check to make sure that the number of waves are equal 
complete_gss_df %>%
  count(wave)


# Part 4: Final round of data cleaning before the EDA ----------------------------------------------

# Some of these variables were created to use during the modeling stage. 
# Note that I also create subsets of the data: one with men only and one with women only 

# Create a numeric version of wave for modeling
complete_gss_df <- complete_gss_df %>%
  mutate(wave = as.numeric(wave))

# Keep a factor version of wave for the EDA
complete_gss_df <- complete_gss_df %>%
  mutate(wave_fct = as.factor(wave))

# Rename the DVs - so it's clear what a higher score means
complete_gss_df <- complete_gss_df %>%
  mutate(conserv_ideol = polviews)

complete_gss_df <- complete_gss_df %>%
  mutate(repub_partyid = partyid)

# Log the family income variable 
complete_gss_df <- complete_gss_df %>%
  mutate(ln_famincome = log(income06))

# Create a dummy variable for divorce
complete_gss_df <- complete_gss_df %>%
  mutate(divorced = ifelse(marital_text == "Divorced", 1, 0))

# Create a dummy variable for very religious 
complete_gss_df <- complete_gss_df %>%
  mutate(very_religious = ifelse(religiosity_text == "Strong", 1, 0))

# Create a dummy variable for no religion 
complete_gss_df <- complete_gss_df %>%
  mutate(no_religion = ifelse(religiosity_text == "No Religion", 1, 0))

# Create a dummy variable for lower class subjective identity
complete_gss_df <- complete_gss_df %>%
  mutate(lower_classid = ifelse(class_text == "Lower" | class_text == "Working", 1, 0))

# Create a dummy variable for unemployed or temp not working
complete_gss_df <- complete_gss_df %>%
  mutate(unempl = ifelse(wrkstat == 3 | class_text == 4, 1, 0))

# Create a factor variable for gender (text) -- this will be used in the EDA
complete_gss_df <- complete_gss_df %>%
  mutate(gender_text = ifelse(sex == 1, "Men", "Women"))

# Check the values of the new variables
complete_gss_df %>%
  count(lower_classid)

# Check the full list of variables in the complete dataset 
names(complete_gss_df)

# Create subsets of the data - one for men, one for women. This will be used during the modeling process
only_men <- complete_gss_df %>%
  filter(sex == 1)

only_women <- complete_gss_df %>%
  filter(sex == 2)


# Part 5: Save/Export the data ----------------------------------------------

# Export the cleaned/processed version of the dataset so it can be used in the EDA
# Note: I prefer to export as an excel file so that the variable types will be preserved 

write_rds(complete_gss_df, "data/processed/complete_gss.rds")
write.xlsx(complete_gss_df, "data/processed/complete_gss.xlsx")
