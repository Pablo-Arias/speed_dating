cat("\f")
rm(list=ls())

# Init
graphics.off()
library("MASS")
library(ez)
library(afex)
library(phia)
library(doBy)
library(effsize)
library(lmerTest);
library(srm);
library(ggplot2)
library(TripleR)
library(Rmisc)
library(tidyverse)
library(truncnorm)
library(scales)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("summary_SE_functions.R")

library(data.table)
file_name = "data/au_df_30hz.csv"


options(datatable.fread.datatable=FALSE)
data = fread(file_name)

head(data)

data$smile                      <- (data$AU12)
smile                           <-as.numeric(data$smile)

data$happiness                  <- (data$happiness)
happiness                       <-as.numeric(data$happiness)

frame                           <- as.factor(data$frame)
file_name                       <- as.factor(data$file_name)
manipulated                     <- as.factor(data$manipulated)
file_tag                        <- as.factor(data$file_tag)
participant_nb                  <- as.factor(data$participant_nb)
interacting_partner             <- as.factor(data$interacting_partner)
N                               <- as.factor(data$N)
day                             <- as.factor(data$day)
female_condition                <- as.factor(data$female_condition)
female_video                    <- as.factor(data$female_video)
individual_date                 <- as.factor(data$individual_date)
male_condition                  <- as.factor(data$male_condition)
male_video                      <- as.factor(data$male_video)
sex                             <- as.factor(data$sex)
other_sex                       <- as.factor(data$other_sex)
participant_condition           <- as.factor(data$participant_condition)
other_condition                 <- as.factor(data$other_condition)
participant_video               <- as.factor(data$participant_video)
other_video                     <- as.factor(data$other_video)
dyad                            <- as.factor(data$dyad)


clean_data <- data.frame(frame, file_name, manipulated, file_tag, participant_nb
                         , interacting_partner, N, day, female_condition
                         , female_video, individual_date, male_condition
                         , male_video, sex, other_sex
                         , participant_condition, other_condition, participant_video, other_video, dyad
                         , smile, happiness
)

head(clean_data)
reduced_df = clean_data %>%
  group_by(participant_nb, participant_condition, other_condition, manipulated) %>%
  summarise_at(c("smile"), mean, na.rm = TRUE) # change to smile/hapiness, mean/SD depending on facial mimicry or SD analysis


# ----------------------------------------------------
# ----------------------------------------------------
# Facial mimicry GLMM analysis for paper -------------
# ----------------------------------------------------
# ----------------------------------------------------
#First level
df = reduced_df[reduced_df$manipulated=="FALSE",]
nul<- lmer( smile ~ 1            	     	            + (1 | participant_nb), data= df, REML = FALSE)
v0 <- lmer( smile ~ participant_condition           + (1 | participant_nb), data= df, REML = FALSE)
v1 <- lmer( smile ~ other_condition                 + (1 | participant_nb), data= df, REML = FALSE)


aov.out = anova(nul,v0)
aov.out
summary(v0)

aov.out = anova(nul,v1)
aov.out


#interaction
nul <- lmer( smile ~ participant_condition+other_condition + (1 | participant_nb), data= df, REML = FALSE)
v0  <- lmer( smile ~ participant_condition*other_condition + (1 | participant_nb), data= df, REML = FALSE)
aov.out = anova(nul,v0)
aov.out

summary(v0)



# ----------------------------------------------------
# ----------------------------------------------------
# Plot for paper - Facial mimicry --------------------
# ----------------------------------------------------
# ----------------------------------------------------

reduced_df = clean_data %>%
  group_by(participant_nb, participant_condition, other_condition, manipulated) %>%
  summarise_at(c("smile"), mean, na.rm = TRUE)

# Take only what participants actual facial production
reduced_df = reduced_df[reduced_df$manipulated=="FALSE",]

dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "smile",
                                withinvars = c("participant_condition", "other_condition"),
                                idvar = "participant_nb"
)

# New facet label names for supp variable
manipulated.labs <- c("What participants produce", "What participants see")
names(manipulated.labs) <- c("False", "True")

theme_set(theme_bw())
ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = smile))+ 
  facet_grid(~factor(manipulated, levels=c("True", "False")), labeller=labeller(manipulated = manipulated.labs)) + 
  theme(family="Helvetica Neue") +
  #geom_point(aes(smile, color=other_condition),size=3, position=position_dodge(.5)) + 
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = smile, color=other_condition), size=3, position=position_dodge(.5))+
  geom_errorbar(aes(y = smile, ymin = smile - ci, ymax = smile + ci), width = 0.2, size=2, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), size=2) + 
  geom_jitter(data=reduced_df, aes(color = factor(other_condition)), alpha = 0.3, position = position_jitterdodge(.1)) +
  labs(x = "Participant condition", y = "Smiling activity (a.u.)") +
  theme_classic() + 
  #scale_color_manual(values=c("#FF7F0E", "#2077B4")) +
  scale_color_manual(values=c("#FF0000", "#0000FF")) +
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased")) + 
  theme(text = element_text(size = 15, color = "black")) +
  theme(axis.text.x = element_text(size = 12, color = "black")) +
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) #+
  #ylim(0.4, 0.6)



ggsave("plots/EMG_mimicry.pdf", width = 10, height = 10, units = "cm")


# ----------------------------------------------------
# ----------------------------------------------------
# Plot for paper - STD happiness ---------------------
# ----------------------------------------------------
# ----------------------------------------------------

reduced_df = clean_data %>%
  group_by(participant_nb, participant_condition, other_condition, manipulated) %>%
  summarise_at(c("happiness"), sd, na.rm = TRUE)


reduced_df = reduced_df[reduced_df$manipulated=="FALSE",]

dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "happiness",
                                withinvars = c("participant_condition", "other_condition", "manipulated"),
                                idvar = "participant_nb"
)

# New facet label names for supp variable
manipulated.labs <- c("What participants produce", "What participants see")
names(manipulated.labs) <- c("False", "True")

theme_set(theme_bw())
ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = happiness))+ 
  facet_grid(~factor(manipulated, levels=c("True", "False")), labeller=labeller(manipulated = manipulated.labs)) + 
  theme(family="Helvetica Neue") +
  #geom_point(aes(happiness, color=other_condition),size=3, position=position_dodge(.5)) + 
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = happiness, color=other_condition), size=5, position=position_dodge(.5))+
  geom_errorbar(aes(y = happiness, ymin = happiness - ci, ymax = happiness + ci), width = 0.5, size=2, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), size=2) + 
  labs(x = "Participant condition", y = "Standard deviation of happiness activity (a.u.)") +
  theme_classic() + 
  #scale_color_manual(values=c("#FF7F0E", "#2077B4")) +
  scale_color_manual(values=c("#FF0000", "#0000FF")) +
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased")) + 
  theme(text = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  ylim(0.31, 0.38)



ggsave("plots/sd_EMG_mimicry.pdf", width = 10, height = 11, units = "cm")
