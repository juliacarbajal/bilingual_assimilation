# GET INFO FROM TRIAL NAME
data = data %>%
  separate(object_asked,c("language","expe_part","rule","word","context"),"_") %>%
  mutate(fam = ifelse(context=='AV' & correct==1,1,ifelse(context=='AU' & correct==0,1,0)))

data$rule = as.factor(data$rule)
data$word = as.factor(data$word)
data$context = as.factor(data$context)


# Relevel data
#data$context = factor(data$context, levels=c('AV','AU'))
#data$rule = factor(data$rule, levels=c('voicing','place'))

# TRAINING PASSING CRITERION

# Calculate number of training trials
training.trials = training.data %>%
  mutate(training_list = config_profile) %>%
  filter(level_name != 'pretraining') %>%
  group_by(subject_id, training_list) %>%
  summarise(n.training = n()) %>%
  ungroup()

# Calculate maximum number of repetitions of a same item
training.reps = training.data %>%
  filter(level_name != 'pretraining') %>%
  mutate(training_list = config_profile) %>%
  group_by(subject_id,object1_name,training_list) %>%
  summarise(n.reps = n()) %>%
  mutate(wrong = ifelse(n.reps>1,1,0)) %>%
  ungroup() %>%
  group_by(subject_id,training_list) %>%
  summarise(max.reps = max(n.reps), n.items.wrong = sum(wrong)) %>%
  filter(max.reps <= 3) %>% # FILTER OUT CHILDREN WHO GOT THE SAME ITEM WRONG 3 TIMES
  filter(n.items.wrong <=2) %>% # FILTER CHILDREN WHO GOT MORE THAN 2 ITEMS WRONG
  ungroup()


# SELECT DATA FROM CHILDREN WHO PASSED THE TRAINING
data = merge(data, age_sex)
data = merge(data, training.trials)
data = merge(data, training.reps)
