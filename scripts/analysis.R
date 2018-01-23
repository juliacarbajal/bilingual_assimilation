###################################################################
# Script for analysis of Bilingual Assimilation test
# Created by: Julia Carbajal (LSCP - Ecole Normale Superieure) 2017
###################################################################

# LOAD LIBRARIES ####
library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(car)


# Select group to analyse:
subject.group = 1 # Set to 0 for Monolinguals, 1 for Bilinguals


# LOAD DATA ####
if (subject.group == 0) {
  data.folder = 'data_M'
} else if (subject.group == 1) {
  data.folder = 'data_B'
}
source("scripts/load_data.R")

# Total number of subjects tested:
N.tested = length(unique(data$subject_id))

# TIDY UP DATA ####
source("scripts/clean_data.R")
data$subject_id = as.factor(data$subject_id)

# Total number of subjects tested:
cat("Total N tested subjects: ", N.tested)

# Total number of subjects included for analysis:
cat("Total N included subjects: ", N.included)

# N Rejected subjects:
cat("Total N rejected subjects: ", N.tested - N.included)

# Rejected trials:
cat("N discarded trials from included subjects: ", trials.discarded)

# CALCULATE RELEVANT MEASURES ####
source("scripts/measures.R")

# Group report (age, sex)
included.agesex = subset(age_sex,subject_id %in% data$subject_id)
summary(included.agesex)


# MAIN PLOTS ####

condition_means$context = factor(condition_means$context, levels=c('AV','AU'), labels=c("viable","unviable"))
condition_means$rule = factor(condition_means$rule, levels=c('voicing','place'))

# Group title (mono, bili) and colours for plotting:
if (subject.group == 0) {
  group.title   = 'Monolinguals'
  group.colours = c('#5ab345','#bde19a')
} else {
  group.title   = 'Bilinguals'
  group.colours = c('#3d98da','#c2e5ff')
}

# 1. Grouped barplots (rule x context, all subjects)
ggplot(condition_means,aes(x=rule, y=mean,fill=context))+
  geom_bar(colour = "black", position='dodge',stat='identity') +
  geom_errorbar(position=position_dodge(width = .9),aes(ymin=mean-se, ymax=mean+se),width=0.2) +
  ylim(0,100) + 
  xlab("Assimilation Rule") +
  ylab("Percentage of Familiar object choice") +
  ggtitle(group.title) +
  theme(text = element_text(size = 23)) +
  scale_fill_manual(values = group.colours) +
  theme(legend.position = c(.85, .85))
# Note: I'm using greens and blues for my BUCLD poster. Otherwise, default colours: '#00bfc4','#ff6666'


# 2. Grouped boxplots (rule x context, all subjects)

subject.percentage.familiar$context = factor(subject.percentage.familiar$context, levels=c('AV','AU'), labels=c("viable","unviable"))
subject.percentage.familiar$rule = factor(subject.percentage.familiar$rule, levels=c('voicing','place'))

ggplot(subject.percentage.familiar, aes(x=rule, y=perc.F,fill=context))+
  geom_boxplot(colour = "black", position='dodge') +
  ylim(0,100) + 
  xlab("Assimilation Rule") +
  ylab("Percentage of Familiar object choice") +
  theme(text = element_text(size = 22)) +
  scale_fill_manual(values = group.colours)
# Note: I'm using greens and blues for my BUCLD poster. Otherwise, default colours: '#00bfc4','#ff6666'


# LOGISTIC REGRESSION ####

# The analysis must be done with a logistic regression over raw data since the 
# %fam can only have 1 of 7 values (not continuous, then can't be used in lmer)
# Instead I use logistic regression directly on my binary 0-1 answers and mark
# the family as binomial.

# Set variable coding: baseline = voicing viable
contrasts(data$rule)    = c(1,0)
contrasts(data$context) = c(1,0)

# FULL MODEL :
data.glm1 = glmer(fam ~ rule*context + (rule*context|subject_id) + (1+context|word), data=data, family="binomial", control=glmerControl('bobyqa'))
summary(data.glm1)


# SIGNIFICANCE TESTS:

# Significance of the full model (comparison against null model):
model.null = glmer(fam ~ 1 + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.null,data.glm1)

# P-values obtained using car package:
Anova(data.glm1, type="III")


# STEP-WISE MODEL COMPARISONS (not used in current analysis)
# Note: Some of the reduced models do not converge so can't be used for all effects,
# we're using the car package to obtain p-values instead.

# Define interaction term (useful for stepwise model comparison tests)
data$interaction<-contrasts(data$rule)[data$rule]*
  contrasts(data$context)[data$context]

# Significance of main effect of context:
model.nocontext = glmer(fam ~ 1 + rule + interaction + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.nocontext,data.glm1) 

# Significance of main effect of rule:
model.norule = glmer(fam ~ 1 + context + interaction + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.norule,data.glm1) 

# Significance of interaction rule*context:
model.nointeraction = glmer(fam ~ 1 + rule + context + (rule*context|subject_id) + (1+context|word), data=data, control=glmerControl('bobyqa'), family="binomial")
anova(model.nointeraction,data.glm1)


# CHECK FOR EFFECT OF BLOCK:
model.block = glmer(fam ~ rule*context+block + (rule*context|subject_id) + (1+context|word), data=data, family="binomial", control=glmerControl('bobyqa'))
summary(model.block)
anova(data.glm1,model.block)


