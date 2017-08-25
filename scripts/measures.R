# HERE I CALCULATE ALL NECESSARY MEASURES FOR ANALYSIS

# Item percentages of familiar object choices, separated by list
item_means.list = data %>%
  group_by(config_profile, word, rule, context) %>%
  summarise(mean = mean(fam), n = n()) %>%
  ungroup()

# Item percentages, not separated by list
item_means.all = data %>%
  group_by(word, rule, context) %>%
  summarise(mean = mean(fam), n = n()) %>%
  ungroup()

# Subject percentages
subject.percentage.familiar = data %>%
  group_by(subject_id, config_profile, training_list, rule, context, age, sex, n.training) %>%
  summarise(perc.F = mean(fam)*100) %>%
  ungroup()

# Subject percentage difference (viable - unviable), per rule (long format)
subject.diff = subject.percentage.familiar %>%
  spread(context,perc.F) %>%
  mutate(diff = AV-AU)

# Subject percentage difference (viable - unviable), per rule (wide format)
subject.diff.wide = subject.diff %>%
  mutate(AU=NULL,AV=NULL) %>%
  spread(rule,diff)

# Mean percentage of familiar choice per condition (2x2, rule, context)
condition_means = subject.percentage.familiar %>%
  group_by(rule,context) %>%
  summarise(mean = mean(perc.F), sd = sd(perc.F), n = n()) %>%
  mutate(se = sd/sqrt(n), condition = ifelse(rule=='place'&context=='AU','Place Unviable', ifelse(rule=='place'&context=='AV','Place Viable', ifelse(rule=='voicing'&context=='AV', 'Voicing Viable', 'Voicing Unviable')))) %>%
  ungroup()

# Mean percentage of familiar choice per condition, separated by list
condition_means.list = subject.percentage.familiar %>%
  group_by(config_profile,rule,context) %>%
  summarise(mean = mean(perc.F), sd = sd(perc.F), n = n()) %>%
  mutate(se = sd/sqrt(n), condition = ifelse(rule=='place'&context=='AU','Place Unviable', ifelse(rule=='place'&context=='AV','Place Viable', ifelse(rule=='voicing'&context=='AV', 'Voicing Viable', 'Voicing Unviable')))) %>%
  ungroup()

# Uninteresting measure: mean response times per condition (2x2, rule, context)
response_times = data %>%
  group_by(rule, context) %>%
  summarise(meanRT = mean(object_touched_at)) %>%
  ungroup()