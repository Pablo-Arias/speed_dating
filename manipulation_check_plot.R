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
source("summary_SE_functions.R")



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)
file_name = "data/au_df_30hz.csv"

options(datatable.fread.datatable=FALSE)
data = fread(file_name)

head(data)

data$smile                      <-data$AU12
smile                           <-as.numeric(data$smile)


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
                         , smile
                         )


head(clean_data)

##--------------------------------------------------------
##--------------------------------------------------------
## --------------- Plot for Paper ------------------------
##--------------------------------------------------------
##--------------------------------------------------------

## Box plot
# Create the box plot
# Rename the levels of "participant_condition" variable
reduced_df = clean_data %>%
  group_by(participant_nb, participant_condition, sex, manipulated) %>%
  summarise_at(c("smile"), mean, na.rm = TRUE)

#rename variables
reduced_df$participant_condition <- factor(reduced_df$participant_condition, levels = c("S", "U"), labels = c("increase", "decrease"))
reduced_df$sex <- factor(reduced_df$sex, levels = c("F", "M"), labels = c("Female data", "Male data"))

# Create a new variable to store the box colors based on conditions
reduced_df$box_color <- with(reduced_df, ifelse(interaction(manipulated, participant_condition) == "TRUE.decrease", "red",
                                                ifelse(interaction(manipulated, participant_condition) == "TRUE.increase", "blue", "black")))

# Convert the box_color variable to a factor with appropriate levels
reduced_df$box_color <- factor(reduced_df$box_color, levels = c("black", "red", "blue"))

ggplot(reduced_df, aes(x = participant_condition, y = smile, fill = box_color)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8), color = "darkgray", alpha = 0.8) +
  geom_jitter(aes(color = box_color), alpha = 0.7, position = position_jitterdodge()) +
  facet_grid(. ~ sex, scales = "free_x", space = "free_x") +
  labs(x = "Participant Manipulation", y = "Smiling Activity (a.u.)", fill = "",
       color = "") +  # Remove fill and color labels
  scale_fill_manual(values = c("black","#3399FF", "#FF3333")) + # Deeper red and blue shades for boxes
  scale_color_manual(values = c("black", "#336699", "#993333")) + # Darker shades for dots
  theme_minimal() +
  theme(
    text = element_text(size = 15, color = "black"),
    axis.text.x = element_text(size = 12, color="black"),
    axis.text.y = element_text(size = 12, color="black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
    legend.position = "none",
  ) +
  ylim(0.2, 0.95)

ggsave("plots/manipulation_check.pdf", width = 14, height = 10, units = "cm")



