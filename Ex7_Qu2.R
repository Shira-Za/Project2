#### Creating Filtered Data: ----
load("./raw_data.rdata")
library(tidyverse)
library(dplyr)

cat("The number of participants it:", length(unique(df$subject))) 

df <- df |>
  na.omit(df) |>
  filter(rt >= 300 & rt <= 3000)

#calculating total trials number:
per_sum <- df |>
  group_by(subject) |>
  summarise(percentage = 1 - (n()/400))
  
#mean and std for total sample:
cat("The mean percent of trial removal is:", mean(per_sum$percentage),
    "and the sd is:", sd(per_sum$percentage))

#save:
save(df, file = "./filtered_data.rdata")