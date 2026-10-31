
## Function for data cleaning

clean_data <- function(res, accuracy_cutoff) {
  res %>%
    filter(is.na(block) | block != -999) %>% # remove practice trials, keep rows with missing block
    filter(!is.na(congruency)) %>% # remove rows with NA in congruency
    # set RT and accuracy of trials with RTs outside the specified range NA
    mutate(
    accuracy = if_else(rt >= 2.5 | rt <= 0.2, NA_real_, accuracy),
    rt = if_else(rt >= 2.5 | rt <= 0.2, NA_real_, rt)
    ) %>%
    # calculate mean accuracy for each subject and condition
    group_by(subject, congruency) %>%
    mutate(cond_accuracy = mean(accuracy, na.rm = TRUE)) %>%
    group_by(subject) %>%
    # use the lowest condition mean for exclusion of subjects
    mutate(min_cond_accuracy = min(cond_accuracy, na.rm = TRUE)) %>%
    # trials of subjects with min_cond_accuracy < accuracy_cutoff become NA
    # accuracy_cutoff will be set to .50 for tasks with 4 or 3 possible answers (chance level for correct answer of .25 and .33, respectively)
    # accuracy_cutoff will be set to .75 for tasks with 2 possible answers (chance level for correct answer of .50)
    mutate(
      accuracy = if_else(min_cond_accuracy < accuracy_cutoff, NA_real_, accuracy),
      rt = if_else(min_cond_accuracy < accuracy_cutoff, NA_real_, rt)
    ) %>%
    # calculate proportion of NA trials for each subject
    mutate(na_proportion = mean(is.na(rt) | is.na(accuracy))) %>%
    # remove subjects with >= 50% NA trials
    filter(na_proportion < 0.50) %>%
    filter(!is.na(rt), !is.na(accuracy)) %>%   # drop excluded trials
    ungroup()
}

# mean number of trials per subject and condition (for plot labels)
trial_label <- function(res, conditions = c("congruent", "incongruent")) {
  counts <- res %>%
    filter(congruency %in% conditions) %>%
    group_by(subject, congruency) %>%
    summarise(n_trials = n(), .groups = "drop") %>%
    group_by(congruency) %>%
    summarise(mean_n = mean(n_trials), sd_n = sd(n_trials), .groups = "drop")
  
  paste0(
    "Mean number of trials per subject: ",
    paste0(counts$congruency, " = ", round(counts$mean_n, 1))
  )
}

## Functions for plotting
# all plots display data from incongruent conditions (except for plot_effect() and plot_rt_acc())

# group-level association between speed and accuracy
plot_group_corr <- function(res, plot_title) {
  sub_desc <- res %>%
  filter(congruency == "incongruent") %>%
    group_by(subject) %>%
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      mean_accuracy = mean(accuracy, na.rm = TRUE)
    )

  # red for positive, blue for negative correlation
  r <- cor(sub_desc$mean_rt, sub_desc$mean_accuracy, use = "complete.obs")
  line_color <- if_else(r > 0, "red", "blue")
  
  ggplot(sub_desc,
         aes(x = mean_rt, y = mean_accuracy)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = line_color) +
    labs(x = "Mean RT (s)", y = "Mean accuracy", title = plot_title, subtitle = trial_label(res, "incongruent")) +
    theme_minimal()
}


# group-level association between the effect of congruency on RT and the effect on accuracy:
plot_effect <- function(res, plot_title) {
  effects <- res %>%
    group_by(subject, congruency) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE),
              mean_acc = mean(accuracy, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(congruency)) %>%
    group_by(subject) %>%
    filter(all(c("congruent", "incongruent") %in% congruency)) %>%
    summarise(
      rt_effect = mean_rt[congruency == "incongruent"] -
        mean_rt[congruency == "congruent"],
      # RT effect = RT in incongruent conditions - RT in congruent conditions
      acc_effect = mean_acc[congruency == "incongruent"] -
        mean_acc[congruency == "congruent"]
      # accuracy effect = accuracy in incongruent conditions - accuracy in congruent conditions
    )
  
  
  ggplot(effects, aes(x = rt_effect, y = acc_effect)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    geom_hline(yintercept = 0, color = "black") +
    geom_vline(xintercept = 0, color = "black") +
    labs(x = "RT effect (incongruent - congruent)", y = "Accuracy effect (incongruent - congruent)", title = plot_title,
         subtitle = trial_label(res %>% filter(subject %in% effects$subject))) +
    theme_minimal()
}


# association between speed and accuracy across sections of up to 20 trials for each subject
plot_speed_acc_lm <- function(res, plot_title) {
  section_data <- res %>%
    filter(congruency == "incongruent") %>%
    group_by(subject) %>%
    mutate(
      overall_trials = row_number(),
      n_trials = n(),
      section = ntile(overall_trials, ceiling(n_trials[1] / 20))
    ) %>%
    group_by(subject, section) %>%
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      mean_accuracy = mean(accuracy, na.rm = TRUE),
      .groups = "drop"
    )

  corr_data <- section_data %>%
    group_by(subject) %>%
    summarise(
      speed_acc_corr = cor(mean_rt, mean_accuracy, use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(sign_corr = case_when(
      is.na(speed_acc_corr) ~ NA_character_,
      speed_acc_corr > 0 ~ "positive",
      speed_acc_corr < 0 ~ "negative",
      speed_acc_corr == 0 ~ "zero"
    ))
  
  plot_data <- section_data %>%
    left_join(corr_data, by = c("subject"))
  
  ggplot(plot_data, aes(x = mean_rt, y = mean_accuracy, group = subject)) +
    geom_smooth(aes(group = subject, color = sign_corr),
                method = "lm",
                se = FALSE) +
    labs(x = "Mean RT (s)", y = "Mean accuracy", title = plot_title, subtitle = trial_label(res %>% filter(subject %in% plot_data$subject))) +
    scale_color_manual(
      name = "speed-accuracy correlation",
      values = c(
      "positive" = scales::alpha("red", 0.3),
      "negative" = scales::alpha("blue", 0.3),
      "zero" = scales::alpha("green", 0.3) # correlations equal to 0 probably won't occur
    ),
    na.value = scales::alpha("black", 0.3),
  )  +
  ylim(0, 1)+
  theme_minimal()
}


### RT for trials with correct and incorrect answers
plot_rt_acc <- function(res, plot_title) {
  acc_data <- res %>%
    group_by(subject, accuracy) %>%
    summarise(mean_rt = mean(rt, na.rm = TRUE), .groups = "drop") %>%
    mutate(accuracy = recode(as.character(accuracy), "1" = "Correct", "0" = "Incorrect")) %>%
    filter(!is.na(accuracy))
  
  ggplot(acc_data, aes(y = mean_rt, x = accuracy, group = subject)) +
    geom_line(alpha = 0.3, color = "blue") +
    geom_point(alpha = 0.6, color = "blue") +
    labs(x = "Accuracy", y = "Mean reaction time (s)", title = plot_title,  subtitle = trial_label(res)) +
    theme_minimal()
}


## association between the proportion of subjects with accuracy > .99 and the sign of the correlation between speed and accuracy
ceiling_sign_summary <- function(res, name) {
  subject_data <- res %>%
    filter(congruency == "incongruent") %>%
    group_by(subject) %>%
    summarise(
      mean_accuracy = mean(accuracy, na.rm = TRUE),
      mean_rt = mean(rt, na.rm = TRUE),
      .groups = "drop"
    )
  
  subject_data %>%
    summarise(
      dataset = name,
      proportion_high_accuracy =
        mean(mean_accuracy > 0.99, na.rm = TRUE),
      correlation_rt_accuracy =
        cor(mean_rt, mean_accuracy, use = "complete.obs")
    )
}
