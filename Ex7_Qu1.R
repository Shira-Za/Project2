# R course for beginners
# Week 7
# assignment by Shira Zadok, id 318958311

#This script uses the total collected data from a stroop experiment and cleans it
#to make is possible to use an raw data.

#This code was written by Shira Zadok on December 18th, 2024.

#### Creating Raw_Data ----
library(dplyr)
file_names <- dir("collected_data")

df <- NULL
for (file in file_names) {
  temp_df <- read.csv(file.path("collected_data", file))
  df <- rbind(df, temp_df)
}

#saving only relevant columns:
df <- df |> 
  mutate(
    task = ifelse(grepl("word", condition), "word_reading", "ink_naming"),
    congruency = ifelse(grepl("incong", condition), "Incongruent", "Congruent"),
    acc = (correct_response == participant_response)*1,
    
    #making sure we have the right type:
    subject = as.factor(subject),
    task = factor(task, levels = c("word_reading", "ink_naming")),
    congruency = factor(congruency, levels = c("Congruent", "Incongruent")),
    block = as.numeric(block),
    trial = as.numeric(trial),
    acc = as.numeric(acc),
    rt = as.numeric(rt)
  ) |>
  select(subject, task, congruency, block, trial, acc, rt)

summary(df)

#checking contrasts:
contrasts(df$task) <- c(0,1)
contrasts(df$task)
contrasts(df$congruency)

#save
save(df, file = "./raw_data.rdata")
