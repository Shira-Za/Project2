#### Descriptive Statistics: ----
load("./filtered_data.rdata")

df_sum <- df |>
  group_by(task, congruency) |>
  summarise(number = n(),
            mean_acc = mean(acc),
            mean_rt = mean(rt),
            sd_acc = sd(acc),
            sd_rt = sd(rt))

#plotting:
library(ggplot2)
library(patchwork)

p1 = ggplot(df_sum, aes(x = task, y = mean_acc, fill = congruency)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_acc - sd_acc, ymax = mean_acc + sd_acc), 
                position = position_dodge(0.9),
                width = 0.3) +
  labs(title = "Mean Accuracy per Task and Congruency", 
       x = "Task", y = "Accuracy (%)") +
  theme_minimal()

p2 = ggplot(df, aes(x = task, y = rt, color = congruency)) +
  geom_jitter(position = position_jitter(width = 0.3), 
              size = 2, alpha = 0.2) +
  geom_point(data = df_sum, aes(x = task, y = mean_rt, group = congruency,
                                color = congruency),
             position = position_dodge(1), size = 3) +
  geom_errorbar(data = df_sum, aes(x = task, y = mean_rt, 
                ymin = mean_rt - sd_rt, ymax = mean_rt + sd_rt,
                color = congruency), position = position_dodge(1),
                width = 0.2) +
  labs(title = "RT per Task", x = "Task", y = "RT (msec)") +
  theme_minimal()


#### Regression ----
library(lme4)
model <- lmer(rt ~ congruency + task + (1|subject), data = df)
summary(model)
