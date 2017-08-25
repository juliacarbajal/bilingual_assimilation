###################################################################
# Script for analysis of Bilingual Assimilation test
# Created by: Julia Carbajal (LSCP - Ecole Normale Superieure) 2017
#
# To Do: Add trial exclusion based on vocabulary questionnaires
###################################################################

# LOAD LIBRARIES ####
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(coin) # Needed for calculation of Exact Wilcoxon test
library(car) # Needed to obtain p-values

# Select group to analyse:
subject.group = 0 # Set to 0 for Monolinguals, 1 for Bilinguals

# LOAD DATA ####
if (subject.group == 0) {
  data.folder = 'data_M'
} else if (subject.group == 1) {
  data.folder = 'data_B'
}
source("scripts/load_data.R")

# TIDY UP DATA ####
source("scripts/clean_data.R")
data$subject_id = as.factor(data$subject_id)


# Total number of subjects included for analysis:
length(unique(data$subject_id))

# CALCULATE RELEVANT MEASURES ####
source("scripts/measures.R")


# MAIN PLOTS ####

# 1.1 Grouped barplots (rule x context, all subjects)
ggplot(condition_means,aes(x=rule, y=mean,fill=context))+
  geom_bar(colour = "black", position='dodge',stat='identity') +
  geom_errorbar(position=position_dodge(width = .9),aes(ymin=mean-se, ymax=mean+se),width=0.2) +
  ylim(0,100) + 
  xlab("Assimilation Rule") +
  ylab("Mean Percentage of Familiar word choice") +
  geom_text(aes(label = round(mean, digits = 1)), position = position_dodge(width = .9), vjust = -4) +
  theme(text = element_text(size = 16))

# 1.2 Barplots per counterbalancing list
ggplot(condition_means.list,aes(x=rule, y=mean,fill=context))+
  geom_bar(colour = "black", position='dodge',stat='identity') +
  geom_errorbar(position=position_dodge(width = .9),aes(ymin=mean-se, ymax=mean+se),width=0.2) +
  ylim(0,100) + facet_wrap(~config_profile) +
  xlab("Assimilation Rule") +
  ylab("Mean Percentage of Familiar word choice") +
  #geom_text(aes(label = round(mean, digits = 1)), position = position_dodge(width = .9), vjust = -4) +
  theme(text = element_text(size = 16)) 

# 2.1 Grouped boxplots (rule x context, all subjects)

#subject.percentage.familiar$context = factor(subject.percentage.familiar$context, levels=c('AV','AU'), labels=c("viable","unviable"))
#subject.percentage.familiar$rule = factor(subject.percentage.familiar$rule, levels=c('voicing','place'))

ggplot(subject.percentage.familiar, aes(x=rule, y=perc.F,fill=context))+
  geom_boxplot(colour = "black", position='dodge') +
  ylim(0,100) + 
  xlab("Assimilation Rule") +
  ylab("Percentage of Familiar word choice") +
  theme(text = element_text(size = 22)) +
  scale_fill_manual(values=c('#00bfc4','#ff6666')) # This line changes the order of the colours.


# 2.2 Boxplots per list
ggplot(subject.percentage.familiar, aes(x=rule, y=perc.F,fill=context))+
  geom_boxplot(colour = "black", position='dodge') +
  ylim(0,100) + facet_wrap(~config_profile) +
  xlab("Assimilation Rule") +
  ylab("Percentage of Familiar word choice") +
  theme(text = element_text(size = 16))


# WILCOXON TESTS FOR PLACE AND VOICING EFFECT ####

# Using Wilcoxon test because the distributions are not normal (bounded by 0) so t-test is not appropriate.
# Note: These tests are just as reference, the full glmer model (logistic regression) should be used instead.

# Wilcoxon test for voicing:
voicing.effect = wilcox.test(perc.F ~ context, data = subset(subject.percentage.familiar, rule=='voicing'), paired = T, alternative = 'less')

# Wilcoxon test for place:
place.effect = wilcox.test(perc.F ~ context, data = subset(subject.percentage.familiar, rule=='place'), paired = T, alternative = 'less')

# Exact Wilcoxon tests (using coin package):
voicing.AU = subset(subject.percentage.familiar, rule=='voicing' & context=='AU')$perc.F
voicing.AV = subset(subject.percentage.familiar, rule=='voicing' & context=='AV')$perc.F
place.AU = subset(subject.percentage.familiar, rule=='place' & context=='AU')$perc.F
place.AV = subset(subject.percentage.familiar, rule=='place' & context=='AV')$perc.F

wilcoxsign_test(voicing.AV ~ voicing.AU, alternative = 'greater')
wilcoxsign_test(place.AV ~ place.AU, alternative = 'greater')



# LOGISTIC REGRESSION ####

# The analysis must be done with a logistic regression over raw data since the 
# %fam can only have 1 of 7 values (not continuous, then can't be used in lmer)
# Instead I use logistic regression directly on my binary 0-1 answers and mark
# the family as binomial.

# Set variable coding: baseline = voicing viable
contrasts(data$rule)    = c(1,0)
contrasts(data$context) = c(1,0)

# Define interaction term (useful for stepwise model comparison tests)
data$interaction<-contrasts(data$rule)[data$rule]*
  contrasts(data$context)[data$context]

# FULL MODEL :
data.glm1 = glmer(fam ~ rule*context + (rule*context|subject_id) + (1+context|word), data=data, family="binomial", control=glmerControl('bobyqa'))
summary(data.glm1)


# SIGNIFICANCE TESTS:

# Significance of the full model (comparison against null model):
model.null = glmer(fam ~ 1 + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.null,data.glm1)

# Significance of main effect of context:
model.nocontext = glmer(fam ~ 1 + rule + interaction + (rule+context+interaction|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.nocontext,data.glm1) 

# Significance of main effect of rule:
model.norule = glmer(fam ~ 1 + context + interaction + (rule+context+interaction|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.norule,data.glm1) 

# Significance of interaction rule*context:
model.nointeraction = glmer(fam ~ 1 + rule + context + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.nointeraction,data.glm1)



# ADDITIONAL PLOTS ####

# 3.1 Density of viable - unviable difference for place and voicing
ggplot(subject.diff,aes(x=diff,fill=rule))+geom_density(alpha=0.4, adjust=0.75) +
  geom_vline(xintercept=0) +
  xlab('diff (% viable - % unviable)') + xlim(-100,100)

# 3.2 Difference per subject
ggplot(subject.percentage.familiar,aes(x=context,y=perc.F,group=subject_id,color=as.factor(subject_id)))+
  geom_line(position=position_jitter(w=0, h=1))+facet_wrap(~rule)


## Barplots per child:

# 4.1 Barplots per child, ordered by id
ggplot(subject.percentage.familiar,aes(x=rule, y=perc.F, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,100) + facet_wrap(~subject_id) +
  xlab("Assimilation Rule") + ylab("Percentage of Familiar word choice") +
  geom_text(aes(label = round(perc.F, digits = 1)), position = position_dodge(width = .9), vjust = -0.5)

# 4.2 Barplots per child, ordered by number of training items
ggplot(subject.percentage.familiar,aes(x=rule, y=perc.F, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,100) + facet_wrap(~n.training + subject_id) +
  xlab("Assimilation Rule") + ylab("Mean Percentage of Familiar word choice") +
  geom_text(aes(label = round(perc.F, digits = 1)), position = position_dodge(width = .9), vjust = -0.5)

# 4.3 Barplots per child, ordered by age
ggplot(subject.percentage.familiar,aes(x=rule, y=perc.F, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,100) + facet_wrap(~age + subject_id) +
  xlab("Assimilation Rule") + ylab("Percentage of Familiar word choice") +
  geom_text(aes(label = round(perc.F, digits = 1)), position = position_dodge(width = .9), vjust = -0.5)

# 4.4 Barplots per child, ordered by test list
ggplot(subject.percentage.familiar,aes(x=rule, y=perc.F, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,100) + facet_wrap(~config_profile + subject_id) +
  xlab("Assimilation Rule") + ylab("Mean Percentage of Familiar word choice") +
  geom_text(aes(label = round(perc.F, digits = 1)), position = position_dodge(width = .9), vjust = -0.5)

# 4.5 Barplots per child, ordered by training list
ggplot(subject.percentage.familiar,aes(x=rule, y=perc.F, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,100) + facet_wrap(~training_list + config_profile + subject_id) +
  xlab("Assimilation Rule") + ylab("Mean Percentage of Familiar word choice") +
  geom_text(aes(label = round(perc.F, digits = 1)), position = position_dodge(width = .9), vjust = -0.5)



## Barplots per item:

# 5.1 Barplots per item, rule==place
ggplot(subset(item_means.all,rule=='place'),aes(x=context, y=mean, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,1) + facet_wrap(~word) +
  xlab("Context") + ylab("Mean Percentage of Familiar word choice") +
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(width = .9), vjust = -0.5)

# 5.2 Barplots per item, rule==voicing
ggplot(subset(item_means.all,rule=='voicing'),aes(x=context, y=mean, fill=context))+
  geom_bar(colour = "black", position='dodge', stat='identity') +
  ylim(0,1) + facet_wrap(~word) +
  xlab("Context") + ylab("Percentage of Familiar word choice") +
  theme(text = element_text(size = 16)) +
  geom_text(aes(label = round(mean, digits = 2)), position = position_dodge(width = .9), vjust = -0.5)



##### OTHER ANALYSES #####

# FAMILIAR OBJECT BIAS VS AGE ####
subject.fam.bias = data %>%
  group_by(subject_id) %>%
  summarise(N.familiar = sum(fam)) %>%
  ungroup()

subject.fam.bias = merge(subject.fam.bias,age_sex)
subject.fam.bias$subject_id = as.factor(subject.fam.bias$subject_id)

# Plot
ggplot(subject.fam.bias,aes(x=age,y=N.familiar*100/24)) + geom_point() + geom_smooth(method='lm')+
  ylim(0,100) +
  xlab("Age (months)") +
  ylab("Total % of familiar object responses") +
  theme(text = element_text(size = 16))

# Correlation
cor.test(subject.fam.bias$age,subject.fam.bias$N.familiar, alternative = 'less')


# RELATIONSHIP BETWEEN AGE AND TRAINING DIFFICULTY ####

training.age = merge(training.trials,age_sex)
ggplot(training.age,aes(x=age,y=n.training)) + geom_point()
ggplot(training.age,aes(x=sex,y=n.training)) + geom_boxplot()

# Children who failed:
failed = subset(training.age,!(subject_id %in% subject.diff$subject_id)) %>%
  mutate(passed = 'no')
succeeded = subset(training.age,(subject_id %in% subject.diff$subject_id)) %>%
  mutate(passed = 'yes')

pass.data = rbind(failed,succeeded)
ggplot(pass.data,aes(x=age,fill=passed))+geom_density(alpha=0.4) +
  theme(text = element_text(size = 16)) +
  xlab('Age (months)')

ggplot(pass.data, aes(x=sex, fill=passed)) + geom_histogram(stat='count')

# VOICING EFFECT VS AGE (There doesn't seem to be an effect of age)
ggplot(subset(subject.diff,rule == "voicing"),aes(x=age,y=diff)) + geom_point()+geom_smooth(method='lm') +
  xlab('Age (months)') +
  ylab('Voicing difference (Viable - Unviable)') +
  theme(text = element_text(size = 16))

# Correlation
cor.test(subset(subject.diff,rule == "voicing")$age,subset(subject.diff,rule == "voicing")$diff)

# PLACE EFFECT VS AGE 
ggplot(subset(subject.diff,rule == "place"),aes(x=age,y=diff)) + geom_point()+geom_smooth(method='lm')

# CORRELATION BETWEEN PLACE AND VOICING EFFECT?
# WEIRDEST THING EVER: JITTER DOESNT WORK UNLESS NUMBERS ARE ROUNDED
ggplot(subject.diff.wide, aes(x= round(voicing,2), y=round(place,2))) +
  geom_jitter() + geom_smooth(method = 'lm') +
  xlim(-100,100) + ylim(-100,100)

cor.test(subset(subject.diff,rule == "voicing")$diff,subset(subject.diff,rule == "place")$diff)
