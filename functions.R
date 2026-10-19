
## Function for data cleaning

clean_data <- function(res, accuracy_cutoff) {
  res %>%
    filter(block != -999) %>% # remove practice trials
    # set RTs outside the specified range NA
    mutate(rt = if_else(rt >= 2.5 | rt <= 0.2, NA_real_, rt)) %>%
    # calculate mean accuracy for each subject
    group_by(subject) %>%
    mutate(mean_accuracy = mean(accuracy, na.rm = TRUE)) %>%
    # trials of subjects with mean accuracy < accuracy_cutoff become NA
    # accuracy_cutoff will be set to .50 for tasks with 4 or 3 possible answers (chance level for correct answer of .25 and .33, respectively)
    # accuracy_cutoff will be set to .75 for tasks with 2 possible answers (chance level for correct answer of .50)
    mutate(
      accuracy = if_else(mean_accuracy < accuracy_cutoff, NA_real_, accuracy),
      rt = if_else(mean_accuracy < accuracy_cutoff, NA_real_, rt)
    ) %>%
    ungroup() %>%
    # calculate proportion of NA trials for each subject
    group_by(subject) %>%
    mutate(na_proportion = mean(is.na(rt) | is.na(accuracy))) %>%
    # remove subjects with >= 50% NA trials
    filter(na_proportion < 0.50) %>%
    ungroup()
}


## Functions for plotting
# all plots display data from incongruent conditions (except for plot_effect())

# group-level association between speed and accuracy
plot_group_corr <- function(res, plot_title) {
  sub_desc <- res %>%
    group_by(subject, congruency) %>%
    summarise(
      mean_rt = mean(rt, na.rm = TRUE),
      sd_rt = sd(rt, na.rm = TRUE),
      mean_accuracy = mean(accuracy, na.rm = TRUE),
      sd_accuracy = sd(accuracy, na.rm = TRUE)
    )
  
  ggplot(sub_desc %>% filter(congruency == "incongruent"),
         aes(x = mean_rt, y = mean_accuracy)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(x = "Mean RT (s)", y = "Mean accuracy", title = plot_title) +
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
    labs(x = "RT effect (incongruent − congruent)", y = "Accuracy effect (incongruent − congruent)", title = plot_title) +
    theme_minimal()
}


# association between speed and accuracy across sections of 20 trials for each subject
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
    filter(sd(mean_rt, na.rm = TRUE) > 0, sd(mean_accuracy, na.rm = TRUE) > 0) %>%
    group_by(subject) %>%
    summarise(
      speed_acc_corr = cor(mean_rt, mean_accuracy, use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(sign_corr = if_else(speed_acc_corr > 0, "positive", "negative"))
  
  plot_data <- section_data %>%
    left_join(corr_data, by = c("subject")) %>%
    filter(!is.na(sign_corr))
  
  ggplot(plot_data, aes(x = mean_rt, y = mean_accuracy, group = subject)) +
    geom_smooth(aes(group = subject, color = sign_corr),
                method = "lm",
                se = FALSE) +
    labs(x = "Mean RT (s)", y = "Mean accuracy", title = plot_title) +
    scale_color_manual(
      name = "speed-accuracy correlation",
      values = c(
      "positive" = scales::alpha("red", 0.3),
      "negative" = scales::alpha("blue", 0.3)
    )) +
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
    labs(x = "Accuracy", y = "Mean reaction time (s)", title = plot_title) +
    theme_minimal()
}


## association between the proportion of subjects with accuracy = 1 and the sign of the correlation between speed and accuracy
ceiling_sign_summary <- function(res, name) {
  subject_data <- res %>%
    filter(congruency == "incongruent")
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
        sum(mean_accuracy > 0.99, na.rm = TRUE) / n_distinct(res$subject),
      correlation_rt_accuracy =
        cor(mean_rt, mean_accuracy, use = "complete.obs")
    )
}
