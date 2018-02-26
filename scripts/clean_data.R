# GET INFO FROM TRIAL NAME
data = data %>%
  separate(object_asked,c("language","expe_part","rule","word","context"),"_") %>%
  mutate(fam = ifelse(context=='AV' & correct==1,1,ifelse(context=='AU' & correct==0,1,0))) %>%
  mutate(block = level_name)

data$block = as.numeric(data$block)
data$rule = as.factor(data$rule)
data$word = as.factor(data$word)
data$context = as.factor(data$context)

training.data = training.data %>%
  filter(level_name != 'pretraining') %>%
  separate(object1_name,c("language","expe_part","rule","word"),"_")


if (subject.group == 1){
  data.all = data
  
  training.data = merge(training.data, vocab.B)

  knows.all = training.data %>%
    group_by(subject_id) %>%
    summarise(knows.all = ifelse(min(knows) == 0, 0, 1)) %>%
    ungroup()

  training.data = merge(training.data, knows.all) %>%
    filter(knows.all == 1) # FILTER OUT CHILDREN WHO DON'T KNOW SOME ITEM FROM THE TRAINING LIST
}
  
# Relevel data
#data$context = factor(data$context, levels=c('AV','AU'))
#data$rule = factor(data$rule, levels=c('voicing','place'))

# TRAINING PASSING CRITERION

# Calculate number of training trials
training.trials = training.data %>%
  mutate(training_list = config_profile) %>%
  group_by(subject_id, training_list) %>%
  summarise(n.training = n()) %>%
  ungroup()

# Calculate maximum number of repetitions of a same item
training.reps = training.data %>%
  mutate(training_list = config_profile) %>%
  group_by(subject_id,training_list,expe_part,rule,word) %>%
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
data = subset(data, subject_id != '1624') # Child excluded because age was 3SD above mean monolinguals age
if (subject.group == 1) {
  data = merge(data, vocab.B)
  N.trials = nrow(data)
  data = data %>%
    filter(knows != 0)
  trials.discarded = N.trials - nrow(data)
} else {
  trials.discarded = 0
}

N.included = length(unique(data$subject_id))