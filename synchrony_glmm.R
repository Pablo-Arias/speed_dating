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
library("sjPlot")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Choose what data to import and condition to analyse
file_name = "data/mi_df_py_feat_happiness.csv"


#read data
data = read.table(file_name, header=TRUE,sep=',')
head(data)



#defining outcomes
mi			          <-as.numeric(data$mi)
mean_corr			    <-as.numeric(data$mean_corr)
max_corr			    <-as.numeric(data$max_corr)


#defining predictors (categorical)
male_condition 	     <- as.factor(data$male_condition)
male_condition       <- relevel(male_condition, "U")
female_condition 		 <- as.factor(data$female_condition)
female_condition     <- relevel(female_condition, "U")
participant_nb			 <- as.factor(data$participant_nb)
interacting_partner	 <- as.factor(data$interacting_partner)
dyad		             <- as.factor(data$dyad)
sex		               <- as.factor(data$sex)
participant_nb		   <- as.factor(data$participant_nb)
other_manipulated		            <- as.factor(data$other_manipulated)
participant_manipulated		      <- as.factor(data$participant_manipulated)
male_id		          <- as.factor(data$male_id)
female_id		        <- as.factor(data$female_id)
u_cond		         <- paste(data$female_condition, data$male_condition)

clean_data=data.frame( female_id, male_id, male_condition, female_condition, dyad, sex, participant_manipulated, other_manipulated,  u_cond, mi, mean_corr, max_corr )
head(clean_data)

# Keep only what participants see and what they produce
clean_data = clean_data[clean_data$participant_manipulated=="False",] 
#clean_data = clean_data[clean_data$other_manipulated=="False",] 

reduced_df = clean_data %>%
  group_by(dyad, male_id, female_id, male_condition, female_condition, other_manipulated, participant_manipulated, u_cond) %>%
  summarise_at(c("mi", "mean_corr", "max_corr"), mean, na.rm = TRUE)


# ---------------------------- #
# ----- MI analysis ---------- #
# ---------------------------- #

#First level
nul<- lmer( mi ~ 1            	     + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v0 <- lmer( mi ~ male_condition      + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v1 <- lmer( mi ~ female_condition    + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v2 <- lmer( mi ~ other_manipulated    + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# --> Effect of female_condition and effect of male_condition at the first level

#Second level
nul<- lmer( mi ~ male_condition            	            + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0 <- lmer( mi ~ male_condition + female_condition      + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out

nul<- lmer( mi ~ female_condition            	          + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v0 <- lmer( mi ~ female_condition + male_condition      + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out


#--> Main effect of both the female condition and the male condition

#Check for interactions

nul <- lmer( mi  ~ female_condition + male_condition  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( mi  ~ female_condition * male_condition  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out

nul <- lmer( mi  ~ male_condition+other_manipulated   + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( mi  ~ male_condition*other_manipulated   + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out


nul <- lmer( mi  ~ female_condition+other_manipulated   + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( mi  ~ female_condition*other_manipulated   + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out

#Triple interactions
nul <- lmer( mi  ~ female_condition+other_manipulated                  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( mi  ~ male_condition*female_condition*other_manipulated   + (1 | male_id) + (1 | female_id), data= reduced_df, REML = FALSE )
aov.out = anova(nul,v0)
aov.out


# --> No interactions
# best fit model includes : female_condition + male_condition
bm <- lmer( mi  ~ female_condition + male_condition  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
plot_model( bm, type = "pred", terms = c("male_condition", "female_condition") )
summary(bm)

## 
## Comparison between congruent condition
reduced_df = clean_data %>%
  group_by(dyad, male_id, female_id, male_condition, female_condition, other_manipulated, participant_manipulated, u_cond) %>%
  summarise_at(c("mi", "mean_corr", "max_corr"), mean, na.rm = TRUE)

#Analysis for only congruent conditions
reduced_df <- reduced_df[reduced_df$u_cond != "U S",]
reduced_df <- reduced_df[reduced_df$u_cond!="S U",]

head(reduced_df)

nul <- lmer( mi  ~ 1                  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( mi  ~ u_cond             + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v1  <- lmer( mi  ~ other_manipulated  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )


aov.out = anova(nul,v0)
aov.out

aov.out = anova(nul,v1)
aov.out

# best model
bm <- lmer( mi  ~ u_cond  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
plot_model( bm, type = "pred", terms = c("u_cond") )
summary(bm)

# ---------------------------- #
# ----- max_corr analysis ---- #
# ---------------------------- #

#First level
nul<- lmer( max_corr ~ 1            	     + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v0 <- lmer( max_corr ~ male_condition      + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v1 <- lmer( max_corr ~ female_condition    + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )
v2 <- lmer( max_corr ~ other_manipulated   + (1 | male_id)+ (1 | female_id) , data= reduced_df, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out

# --> Effect of female_condition only.

## 
## Comparison between congruent condition
reduced_df = clean_data %>%
  group_by(dyad, male_id, female_id, male_condition, female_condition, other_manipulated, participant_manipulated, u_cond) %>%
  summarise_at(c("mi", "mean_corr", "max_corr"), mean, na.rm = TRUE)

#keep good conditions
reduced_df = reduced_df[reduced_df$participant_manipulated=="False",] 
reduced_df <- reduced_df[reduced_df$u_cond != "U S",]
reduced_df <- reduced_df[reduced_df$u_cond!="S U",]

head(reduced_df)

nul <- lmer( max_corr  ~ 1                  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v0  <- lmer( max_corr  ~ u_cond             + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
v1  <- lmer( max_corr  ~ other_manipulated  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )


aov.out = anova(nul,v0)
aov.out

aov.out = anova(nul,v1)
aov.out


# best model
bm <- lmer( max_corr  ~ u_cond  + (1 | male_id) + (1 | female_id) , data= reduced_df, REML = FALSE )
plot_model( bm, type = "pred", terms = c("u_cond") )
summary(bm)
