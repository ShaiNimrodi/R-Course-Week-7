# R course for beginners
# Week 6
# Assignment by Shai Nimrodi 318516465

### PART A- loading and merging files ###

# 1. Merging files using files

getwd()
setwd("~/Desktop/Rcourse/Week 6")


files_names <- dir("stroop_data")

df <- NULL

for(i in files_names){
  temp1 <- read.csv(paste0("stroop_data/", i))
  df <- rbind(df, temp1)
}

# 2. Counting number of subjects and conditions

library(dplyr)

df |>
  summarize(
  num_subjects = n_distinct(subject),
  num_conditions = n_distinct(condition)
  )

# 3. Count and percent of NA reaction time
df |>
  summarize(
    rt_missing = sum(is.na(rt)),
    percent_rt_missing = mean(is.na(rt) *100)
  )

# 4. Histogram for cong and incog rt 

library(ggplot2)

df |>
  filter(condition == "congruent") |>
  ggplot(aes(x = rt)) + 
  geom_histogram()

df |>
  filter(condition == "incongruent") |>
  ggplot(aes(x = rt)) + 
  geom_histogram()


### PART B- Cleaning data and outliers ###

# 1. Detecting outlier RT

mark_outliers <- function(rt_vector){
  mean <- mean(rt_vector)
  sd <- sd(rt_vector)
  rt_vector > mean+ 2*sd
}

df <- df |>
  group_by(subject) |>
  mutate(outlier = mark_outliers(rt))

sum(df$outlier) #checking how many outliers there are

# 2. Marking outliers with functions (I didn't underatand this part.. used AI to help but still unsure)

subjects <- unique(df$subject)

outliers_all <- c()

for (s in subjects){
  rt_subject <- df$rt[df$subject == s]
  outlier_subject <- mark_outliers(rt_subject)
  outliers_all <- c(outliers_all, outlier_subject)
}

df$outlier_loop <- outliers_all


### PART C- visual representation ###

# 1. Count and print all outliers for each participant

df |>
  group_by(subject) |>
  summarize(num_outliers = sum(outlier))


df_no_outliers <- df |>
  filter(outlier == FALSE)

# 2. Print histograms without outliers

library(ggplot2)

df_no_outliers |>
  filter(condition == "congruent") |>
  ggplot(aes(x = rt)) + 
  geom_histogram()

df_no_outliers |>
  filter(condition == "incongruent") |>
  ggplot(aes(x = rt)) + 
  geom_histogram()

