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

#Import data
file_name = "data/srm_post_norm_for_glmm.csv" # SRM relationship data of Q2-Q1 normalisation

#read data
data = read.table(file_name, header=TRUE,sep=',')
head(data)

#defining outcomes
relationship			    <-as.numeric(data$relationship)

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
all_data=data.frame( relationship, participant_condition , interacting_partner, dyad, other_condition, participant_nb, question_content, sex )

## -------------------------------------- ##
## -------------------------------------- ##
## GLMM Analysis ------------------------ ##
## -------------------------------------- ##
## -------------------------------------- ##

# Choose the question you want to anlayse
#question = "other_seeing_me_again" # Other seeing me again 
question = "good_conversation"    # Conversation quality data
#question = "seeing_again"         # Romantic attraction data
#question = "smile"                # Other smile data


clean_data <- all_data[all_data$question_content == question,]

head(clean_data)

#---- Linear Model, without random factors
nul<- lm( relationship ~ 1             	                         , data= clean_data)
v0 <- lm( relationship ~ participant_condition                   , data= clean_data)
v1 <- lm( relationship ~ other_condition                         , data= clean_data)
v2 <- lm( relationship ~ other_condition + participant_condition , data= clean_data)
v3 <- lm( relationship ~ other_condition * participant_condition , data= clean_data)

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out
aov.out = anova(nul,v3)
aov.out

aov.out = anova(v2,v3)
aov.out


## ---- With both participant_nb and interacting_partner as random factors
nul<- lmer( relationship ~ 1            	     	                        + (1 | participant_nb) + (1 | interacting_partner), data= clean_data, REML = FALSE )
v0 <- lmer( relationship ~ participant_condition                        + (1 | participant_nb) + (1 | interacting_partner), data= clean_data, REML = FALSE )
v1 <- lmer( relationship ~ other_condition                              + (1 | participant_nb) + (1 | interacting_partner), data= clean_data, REML = FALSE )
v2 <- lmer( relationship ~ other_condition + participant_condition      + (1 | participant_nb) + (1 | interacting_partner), data= clean_data, REML = FALSE )
v3 <- lmer( relationship ~ other_condition * participant_condition      + (1 | participant_nb) + (1 | interacting_partner), data= clean_data, REML = FALSE )

aov.out = anova(nul,v0)
aov.out
aov.out = anova(nul,v1)
aov.out
aov.out = anova(nul,v2)
aov.out
aov.out = anova(nul,v3)
aov.out

#Interaction
aov.out = anova(v2,v3)
aov.out

#summary
summary(v3)

#plot
plot_model( v3, type = "pred", terms = c("participant_condition", "other_condition") )


## ----  Corrected post-hocs
library(PairedData)

conds = c("U", "S")
bonferoni_alpha =  0.05/6 # for bonferoni correction
for (pc in conds){
  for (oc in conds){
    #choose one measure
    reduced = clean_data[clean_data$participant_condition==pc,]
    reduced = reduced[reduced$other_condition==oc,]
    
    #Choose another one
    for (pc2 in conds){
      for (oc2 in conds){
        reduced2 = clean_data[clean_data$participant_condition==pc2,]
        reduced2 = reduced2[reduced2$other_condition==oc2,]
        
        #choose one measure
        x = reduced$relationship
        y = reduced2$relationship
        res = t.test(x, y, paired = TRUE)
        
        if(is.nan(res$p.value)){
          next
        }
        if (res$p.value < bonferoni_alpha)
        {
          
          d = round(effectsize::cohens_d(x, y, paired = TRUE), digit=5)
          print(paste("pc : ", pc, " oc : ", oc , " vs ", "pc : ", pc2, " oc : ", oc2 ))
          print(paste("p = ", round(res$p.value, digit=5) , "--- t = " 
                      , round(res$statistic[[1]], digit=5)
                      , "d : ", d$Cohens_d ," df = ", res$parameter)
          )
          print("")  
        }
      }
    }
  }
}
