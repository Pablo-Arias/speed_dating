cat("\f")
rm(list=ls())

# Init
graphics.off()
library("MASS")
library(afex)
library(phia)
library(doBy)
library(effsize)
library(lmerTest);
library(dplyr);
library(ggplot2)
library("ggpubr")

library("sjPlot")
setwd("/Users/arias/Desktop/Projects/speed_dating/physical_experiment/submission_scripts")
source("summary_SE_functions.R")


#Choose what data to import and condition to analyse
file_name = "data/audio_analyses_all_data_df.csv"

#read data
data = read.table(file_name, header=TRUE,sep=',')
head(data)

#defining outcomes
F1			    <-as.numeric(data$F1)
F2			    <-as.numeric(data$F2)
F3			    <-as.numeric(data$F3)
pitch			  <-as.numeric(data$participant_pitch)


#defining predictors (categorical)
participant_condition 	<- as.factor(data$participant_condition)
participant_condition   <- relevel(participant_condition, "U")
other_condition 		    <- as.factor(data$other_condition)
other_condition         <- relevel(other_condition, "U")
participant_nb			    <- as.factor(data$participant_nb)
interacting_partner			<- as.factor(data$interacting_partner)
question_content		    <- as.factor(data$question_content)
dyad		                <- as.factor(data$dyad)
sex		                  <- as.factor(data$sex)



#create new dataframe
clean_data=data.frame( F1, F2,F3, pitch, participant_condition , interacting_partner, dyad, other_condition, participant_nb, question_content, sex )
head(clean_data)

reduced_df = clean_data %>%
  group_by(participant_nb, participant_condition, interacting_partner, dyad, other_condition, sex) %>%
  summarise_at(c("F1", "F2", "F3", "pitch"), mean, na.rm = TRUE)


# --------------------------
# --------------------------
# Pitch  - Plot for paper
# --------------------------
# --------------------------

dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "pitch",
                                withinvars = c("participant_condition", "other_condition"),
                                betweenvars = "sex",
                                idvar = "participant_nb"
)

# New facet label names for supp variable
sex.labs <- c("Female", "Male")
names(sex.labs) <- c("F", "M")

theme_set(theme_bw())
pitch <- ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = pitch))+ 
  facet_wrap(~factor(sex, levels=c("F", "M")), labeller=labeller(sex = sex.labs), nrow = 2) + 
  theme(family="Helvetica Neue") +
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = pitch, color=other_condition), size=3, position=position_dodge(.5))+
  geom_errorbar(aes(y = pitch, ymin = pitch - ci, ymax = pitch + ci), width = 0.2, size=0.7, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), linewidth=1) + 
  geom_jitter(data=reduced_df, aes(color = factor(other_condition)), alpha = 0.2, position = position_jitterdodge(.1)) +
  labs(x = "Participant condition", y = "Pitch (Hz)", color="Other condition") +
  theme_classic() + 
  #scale_color_manual(values=c("#FF7F0E", "#2077B4"), labels = c("decreased", "increased")) +
  scale_color_manual(values=c("#FF0000", "#0600FF")) + 
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased"), limits=c("S", "U")) + 
  theme(text = element_text(size = 11, color = "black")) +
  theme(axis.text.x = element_text(size = 9, color = "black")) +
  theme(axis.text.y = element_text(size = 9, color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0 , r = 5, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0))) 



ggsave("plots/pitch.pdf", width = 7, height = 12, units = "cm")




# --------------------------
# --------------------------
# F1  - Plot for paper
# --------------------------
# --------------------------
dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "F1",
                                withinvars = c("participant_condition", "other_condition"),
                                betweenvars = "sex",
                                idvar = "participant_nb"
)

# New facet label names for supp variable
sex.labs <- c("Female", "Male")
names(sex.labs) <- c("F", "M")

theme_set(theme_bw())
f1 <- ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = F1))+ 
  facet_wrap(~factor(sex, levels=c("F", "M")), labeller=labeller(sex = sex.labs), nrow = 2) + 
  theme(family="Helvetica Neue") +
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = F1, color=other_condition), size=3, position=position_dodge(.5))+
  geom_errorbar(aes(y = F1, ymin = F1 - ci, ymax = F1 + ci), width = 0.2, size=0.7, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), linewidth=1) + 
  geom_jitter(data=reduced_df, aes(color = factor(other_condition)), alpha = 0.2, position = position_jitterdodge(.1)) +
  labs(x = "participant condition", y = "F1 (Hz)", color="Other condition") +
  theme_classic() + 
  #scale_color_manual(values=c("#FF7F0E", "#2077B4"), labels = c("decreased", "increased")) +
  scale_color_manual(values=c("#FF0000", "#0600FF")) + 
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased"), limits=c("S", "U")) + 
  theme(text = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.text.y = element_text(size = 9))+
  theme(axis.title.y = element_text(margin = margin(t = 0 , r = 8, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0))) 



ggsave("plots/F1.pdf", width = 7, height = 12, units = "cm")





# --------------------------
# --------------------------
# F2  - Plot for paper
# --------------------------
# --------------------------
dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "F2",
                                withinvars = c("participant_condition", "other_condition"),
                                betweenvars = "sex",
                                idvar = "participant_nb"
)

# New facet label names for supp variable
sex.labs <- c("Female", "Male")
names(sex.labs) <- c("F", "M")

theme_set(theme_bw())
f2 <- ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = F2))+ 
  facet_wrap(~factor(sex, levels=c("F", "M")), labeller=labeller(sex = sex.labs), nrow = 2) + 
  theme(family="Helvetica Neue") +
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = F2, color=other_condition), size=3, position=position_dodge(.5))+
  geom_errorbar(aes(y = F2, ymin = F2 - ci, ymax = F2 + ci), width = 0.2, size=0.7, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), linewidth=1) + 
  geom_jitter(data=reduced_df, aes(color = factor(other_condition)), alpha = 0.2, position = position_jitterdodge(.1)) +
  labs(x = "participant condition", y = "F2 (Hz)") +
  theme_classic() + 
  scale_color_manual(values=c("#FF0000", "#0600FF")) +  #scale_color_manual(values=c("#FF7F0E", "#2077B4")) +
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased"), limits=c("S", "U")) + 
  theme(text = element_text(size = 11, color = "black")) +
  theme(axis.text.x = element_text(size = 9, color = "black")) +
  theme(axis.text.y = element_text(size = 9, color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0 , r = 8, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0))) 



ggsave("plots/F2.pdf", width = 7, height = 12, units = "cm")



# --------------------------
# --------------------------
# F3  - Plot for paper
# --------------------------
# --------------------------
dat_summary <- summarySEwithin2(reduced_df,
                                measurevar = "F3",
                                withinvars = c("participant_condition", "other_condition"),
                                betweenvars = "sex",
                                idvar = "participant_nb"
)

# New facet label names for supp variable
sex.labs <- c("Female", "Male")
names(sex.labs) <- c("F", "M")

theme_set(theme_bw())
f3 <- ggplot(dat_summary, aes(x =participant_condition, color= other_condition, y = F3))+ 
  facet_wrap(~factor(sex, levels=c("F", "M")), labeller=labeller(sex = sex.labs), nrow = 2) + 
  theme(family="Helvetica Neue") +
  geom_point(data=dat_summary,  mapping=aes(x = participant_condition, y = F3, color=other_condition), size=3, position=position_dodge(.5))+
  geom_errorbar(aes(y = F3, ymin = F3 - ci, ymax = F3 + ci), width = 0.2, size=0.7, data = dat_summary, position=position_dodge(.5)) + 
  geom_line(aes(colour=other_condition, group=other_condition), position=position_dodge(.5), linewidth=1) + 
  geom_jitter(data=reduced_df, aes(color = factor(other_condition)), alpha = 0.2, position = position_jitterdodge(.1)) +
  labs(x = "Participant condition", y = "F3 (Hz)", color="Other condition") +
  theme_classic() + 
  #scale_color_manual(labels = c("Decrease", "Increase"), values=c("#FF7F0E", "#2077B4")) +
  scale_color_manual(values=c("#FF0000", "#0600FF")) + 
  theme(legend.position = "none", strip.background.x = element_blank())+
  scale_x_discrete(breaks=c("S","U"), labels=c("increased", "decreased"), limits=c("S", "U")) + 
  theme(text = element_text(size = 11, color = "black")) +
  theme(axis.text.x = element_text(size = 9, color = "black")) +
  theme(axis.text.y = element_text(size = 9, color = "black"))+
  theme(axis.title.y = element_text(margin = margin(t = 0 , r = 8, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l = 0))) 



ggsave("plots/F3.pdf", width = 7, height = 12, units = "cm")



