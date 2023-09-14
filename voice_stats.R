cat("\f")
rm(list=ls())

# Import
graphics.off()
library("MASS")
library(afex)
library(phia)
library(doBy)
library(effsize)
library(lmerTest);
library(dplyr);
library("sjPlot")
setwd("/Users/arias/Desktop/Projects/speed_dating/physical_experiment/submission_scripts")


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


# ---------------------------- #
# ----- Pitch analysis ------- #
# ---------------------------- #

#First level
nul<- lmer( pitch ~ 1            	     	      + (1 | participant_nb)  , data= reduced_df, REML = FALSE )
v0 <- lmer( pitch ~ participant_condition     + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v2 <- lmer( pitch ~ sex                       + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# --> main effect of sex


#Interactions

nul<- lmer( pitch ~ sex            	     	                        + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ sex * other_condition                         + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out


nul<- lmer( pitch ~ sex            	     	                        + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ sex * participant_condition                         + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out



nul<- lmer( pitch ~ sex            	     	                        + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ sex + other_condition + participant_condition + (1 | participant_nb), data= reduced_df, REML = FALSE )

aov.out = anova(nul,v1)
aov.out

nul<- lmer( pitch ~ sex            	     	                        + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ sex + other_condition * participant_condition + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out

nul<- lmer( pitch ~ sex            	     	                        + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( pitch ~ sex * other_condition * participant_condition + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out

# best model only contains ONLY sex, nothing else nothing more.
plot_model( v1, type = "pred", terms = c("participant_condition", "other_condition", "sex") )
summary(v1)

library(emmeans)
em <- emmeans(v1, pairwise ~ other_condition*participant_condition*sex, adjust = "sidak", type="response")
em
eff_size(em, sigma = sigma(v1), edf = df.residual(v1))



# ---------------------------- #
# ----- F1 analysis ---------- #
# ---------------------------- #
nul<- lmer( F1 ~ 1            	     	      + (1 | participant_nb), data= reduced_df, REML = FALSE )
v0 <- lmer( F1 ~ participant_condition     + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1 <- lmer( F1 ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v2 <- lmer( F1 ~ sex                       + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# Effect of other_condition and sex

#Second level - Main effect of sex
nul <- lmer( F1 ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1  <- lmer( F1 ~ sex  + other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out


#Second level - Main effect of other_condition
nul <- lmer( F1 ~ sex  + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1  <- lmer( F1 ~ sex  + other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v1)
aov.out

# --> So we have two main effects : sex and other condition
nul <- lmer( F1 ~ sex + other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1 <- lmer( F1 ~ sex * other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v1)
aov.out

# --> Significant interation between sex and other condition

final_model <- lmer( F1 ~ sex * other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
plot_model( final_model, type = "pred", terms = c("sex", "other_condition") )

summary(final_model)

library(emmeans)
em <- emmeans(final_model, pairwise ~ sex*other_condition*sex, adjust = "sidak", type="response")
em

# ---------------------------- #
# ----- F2 analysis ---------- #
# ---------------------------- #
nul<- lmer( F2 ~ 1            	     	      + (1 | participant_nb), data= reduced_df, REML = FALSE )
v0 <- lmer( F2 ~ participant_condition     + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1 <- lmer( F2 ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v2 <- lmer( F2 ~ sex                       + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# --> First level : main effect of sex and marginal main effect of other condition.

#Second level - Main effect of sex
nul <- lmer( F2 ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1  <- lmer( F2 ~ sex  + other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out

#Second level - Main effect of other_condition
nul <- lmer( F2 ~ sex                       + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1  <- lmer( F2 ~ sex  + other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v1)
aov.out


#Check for interaction of sex and participant_condition
nul<- lmer( F2 ~ sex            	     	      + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1<- lmer( F2 ~ sex*participant_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out

#Check for interaction of sex and other_condition
nul <- lmer( F2 ~ sex+other_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1  <- lmer( F2 ~ sex*other_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out

# --> Marginal interaction sex*other condition
summary(v1)


# ---------------------------- #
# ----- F3 analysis ---------- #
# ---------------------------- #
nul<- lmer( F3 ~ 1            	     	     + (1 | participant_nb), data= reduced_df, REML = FALSE )
v0 <- lmer( F3 ~ participant_condition     + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v1 <- lmer( F3 ~ other_condition           + (1 | participant_nb) , data= reduced_df, REML = FALSE )
v2 <- lmer( F3 ~ sex                       + (1 | participant_nb) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# --> Only effect of sex and other condition
# Second level
nul<- lmer( F3 ~ other_condition          + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( F3 ~ other_condition + sex    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out


nul<- lmer( F3 ~ sex                      + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1 <- lmer( F3 ~ other_condition + sex    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out


# --> Main effect of sex and other condition

#Check for interaction of sex and participant_condition
nul <- lmer( F3 ~ sex + other_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1  <- lmer( F3 ~ sex * other_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out
# --> Significant interaction of sex and other_condition


#Check for interaction of sex and participant_condition
nul <- lmer( F3 ~ sex+participant_condition   + (1 | participant_nb), data= reduced_df, REML = FALSE )
v1  <- lmer( F3 ~ sex*participant_condition    + (1 | participant_nb), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v1)
aov.out
# --> no interaction at the second level

final_model <- lmer( F3 ~ sex * other_condition    + (1 | participant_nb) , data= reduced_df, REML = FALSE )
plot_model( final_model, type = "pred", terms = c("sex", "other_condition") )
summary(final_model)


