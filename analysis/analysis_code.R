library("lme4")
library("lmerTest")
library("ggplot2")
library("Rcpp")
library("lm.beta")
library("psych")
library('knitr')
library('tidyverse')
library(ggsci)
library(jtools)
library(gridExtra)

df <- read.csv('~/Box Sync/CurrentProjects_Penn/COVID-19/Analysis/Survey_data/covid19_study1_clean_wide.csv', stringsAsFactors = FALSE)

#calculate purpose average scores

df$purpose_2r <- ifelse(df$purpose_2 == 6,1,
                           ifelse(df$purpose_2 == 5,2,
                                  ifelse(df$purpose_2 == 4,3,
                                         ifelse(df$purpose_2 == 3,4,
                                                ifelse(df$purpose_2 == 2, 5,
                                                       ifelse(df$purpose_2 == 1,6,
                                                              NA))))))


df$purpose_4r <- ifelse(df$purpose_4 == 6,1,
                        ifelse(df$purpose_4 == 5,2,
                               ifelse(df$purpose_4 == 4,3,
                                      ifelse(df$purpose_4 == 3,4,
                                             ifelse(df$purpose_4 == 2, 5,
                                                    ifelse(df$purpose_4 == 1,6,
                                                           NA))))))

df$purpose_5r <- ifelse(df$purpose_5 == 6,1,
                        ifelse(df$purpose_5 == 5,2,
                               ifelse(df$purpose_5 == 4,3,
                                      ifelse(df$purpose_5 == 3,4,
                                             ifelse(df$purpose_5 == 2, 5,
                                                    ifelse(df$purpose_5 == 1,6,
                                                           NA))))))
df$purpose_6r <- ifelse(df$purpose_6 == 6,1,
                        ifelse(df$purpose_6 == 5,2,
                               ifelse(df$purpose_6 == 4,3,
                                      ifelse(df$purpose_6 == 3,4,
                                             ifelse(df$purpose_6 == 2, 5,
                                                    ifelse(df$purpose_6 == 1,6,
                                                           NA))))))

  

df$purpose = rowMeans(df[,c("purpose_1",  "purpose_2r", "purpose_3" , "purpose_4r",
                 "purpose_5r", "purpose_6r", "purpose_7")])




#calculate intention average scores


#all intention items (1-10)

df$intentions1_7r <- ifelse(df$intentions1_7 == 7,1,
                            ifelse(df$intentions1_7 == 6,2,
                                   ifelse(df$intentions1_7 == 5,3,
                                          ifelse(df$intentions1_7 == 4,4,
                                                 ifelse(df$intentions1_7 == 3, 5,
                                                        ifelse(df$intentions1_7 == 2,6,
                                                               ifelse(df$intentions1_7 == 1,7,
                                                                      NA)))))))
df$intentions1_8r <- ifelse(df$intentions1_8 == 7,1,
                            ifelse(df$intentions1_8 == 6,2,
                                   ifelse(df$intentions1_8 == 5,3,
                                          ifelse(df$intentions1_8 == 4,4,
                                                 ifelse(df$intentions1_8 == 3, 5,
                                                        ifelse(df$intentions1_8 == 2,6,
                                                               ifelse(df$intentions1_8 == 1,7,
                                                                      NA)))))))
df$intentions1_9r <- ifelse(df$intentions1_9 == 7,1,
                            ifelse(df$intentions1_9 == 6,2,
                                   ifelse(df$intentions1_9 == 5,3,
                                          ifelse(df$intentions1_9 == 4,4,
                                                 ifelse(df$intentions1_9 == 3, 5,
                                                        ifelse(df$intentions1_9 == 2,6,
                                                               ifelse(df$intentions1_9 == 1,7,
                                                                      NA)))))))





df$intentions = rowMeans(df[,c("intentions1_1" , "intentions1_2" , "intentions1_3" , "intentions1_4" , 
                               "intentions1_5" , "intentions1_6" , "intentions1_7r" ,  "intentions1_8r" ,  
                               "intentions1_9r" , "intentions1_10")])


#intention 1, social distancing items only: 2, 4, 6, 10
df$intentions_sd = rowMeans(df[,c("intentions1_2","intentions1_4", "intentions1_6", "intentions1_10")])

#intention non sd items: 1, 3, 5, 7r, 8r, 9r
df$intentions_nsd = rowMeans(df[,c("intentions1_1","intentions1_3", "intentions1_5", "intentions1_7r", "intentions1_8r", "intentions1_9r")])

#intention hand items: 1, 7r, 8r, 9r
df$intentions_hand = rowMeans(df[,c("intentions1_1","intentions1_7r", "intentions1_8r", "intentions1_9r")])

#intention home items: 3, 5
df$intentions_home = rowMeans(df[,c("intentions1_3", "intentions1_5")])


# norms

## norms_close all (1-10)

df$norms_close1_7r <- 100-df$norms_close1_7
df$norms_close1_8r <- 100-df$norms_close1_8
df$norms_close1_9r <- 100-df$norms_close1_9

df$norms_close = rowMeans(df[,c("norms_close1_1", "norms_close1_2", "norms_close1_3", "norms_close1_4", 
"norms_close1_5", "norms_close1_6", "norms_close1_7r", "norms_close1_8r",
"norms_close1_9r", "norms_close1_10")])


# norms_close social distancing only (2 4 6 10)
df$norms_close_sd = rowMeans(df[,c("norms_close1_2", "norms_close1_4", "norms_close1_6", "norms_close1_10")])

#norms_close non sd items: 1, 3, 5, 7r, 8r, 9r
df$norms_close_nsd = rowMeans(df[,c("norms_close1_1","norms_close1_3", "norms_close1_5", "norms_close1_7r", "norms_close1_8r", "norms_close1_9r")])


#norms_close hand items: 1, 7r, 8r, 9r
df$norms_close_hand = rowMeans(df[,c("norms_close1_1","norms_close1_7r", "norms_close1_8r", "norms_close1_9r")])

#norms_close home items: 3, 5
df$norms_close_home = rowMeans(df[,c("norms_close1_3", "norms_close1_5")])




#belief
## belief_norms (1, 2, 3 (took out 4 in the purpose prereg))
df$beliefs_norms_1r <- ifelse(df$beliefs_norms_1 == 7,1,
                              ifelse(df$beliefs_norms_1 == 6,2,
                                     ifelse(df$beliefs_norms_1 == 5,3,
                                            ifelse(df$beliefs_norms_1 == 4,4,
                                                   ifelse(df$beliefs_norms_1 == 3, 5,
                                                          ifelse(df$beliefs_norms_1 == 2,6,
                                                                 ifelse(df$beliefs_norms_1 == 1,7,
                                                                        NA)))))))


df$beliefs_norms = rowMeans(df[,c("beliefs_norms_1r","beliefs_norms_2","beliefs_norms_3")])
df$beliefs_safe_self = rowMeans(df[,c("beliefs_safe_self_1","beliefs_safe_self_2", "beliefs_safe_self_3", 
                                      "beliefs_safe_self_4", "beliefs_safe_self_5")])
df$beliefs_safe_others = rowMeans(df[,c( "beliefs_safe_others_1","beliefs_safe_others_2", "beliefs_safe_others_3",
                                       "beliefs_safe_others_4", "beliefs_safe_others_5", "beliefs_safe_others_6",
                                       "beliefs_safe_others_7", "beliefs_safe_others_8")])

#clean up the subjective SES scale
df <- within(df, {
  ses_subj <- ifelse(ses_subj_1 == "On", 1,
                     ifelse(ses_subj_2 == "On", 2,
                            ifelse(ses_subj_3 == "On", 3,
                                   ifelse(ses_subj_4 == "On", 4, 
                                          ifelse(ses_subj_5 == "On", 5,
                                                 ifelse(ses_subj_6 == "On", 6, 
                                                        ifelse(ses_subj_7 == "On", 7,
                                                               ifelse(ses_subj_8 == "On", 8, 
                                                                      ifelse(ses_subj_9 == "On", 9,
                                                                             ifelse(ses_subj_10 == "On", 10, NA))))))))))
})



#subset with available data
df_raw <- df
df =subset (df,lonely_current!='NA')
df =subset (df,purpose!='NA')
df =subset (df,intentions !='NA')


#z score variables
#df = df %>%
#  select(condition, SID, gender,race, age, intentions, purpose, lonely_current) %>%
#  mutate(intentions_z = scale(intentions),
#         purpose_z = scale(purpose),
#         lonely_current_z = scale(lonely_current),
#         age_z = scale(age))

#df_scaled = lapply(df_scaled, function(x) { attributes(x) <- NULL; x })





#write.csv(df,"~/Box Sync/CurrentProjects_Penn/COVID-19/Analysis/Yoona_purpose/analysis/df.csv")


#demographics
summary(df$age)
sd(df$age)
table (df$gender)
table (df$race)



# crombach's alpha
df_purpose <- df_raw[ , c("purpose_1", "purpose_2r", "purpose_3", "purpose_4r", "purpose_5r", "purpose_6r", "purpose_7")]    
alpha(df_purpose)

df_intention <- df_raw[ , c("intentions1_1" , "intentions1_2" , "intentions1_3" , "intentions1_4" , 
                               "intentions1_5" , "intentions1_6" , "intentions1_7r" ,  "intentions1_8r" ,  
                               "intentions1_9r" , "intentions1_10")]    
alpha(df_intention)

#H1a. Higher self-reported purpose in life will be associated with less perceived loneliness.
#Across participants, higher self-reported purpose in life was associated with lower feelings of loneliness
test <- lm(lonely_current ~ purpose+as.factor(condition), data=df)#in the main text
summ(test, digits = 3, confint = TRUE)


#H2. Higher self-reported purpose in life will be associated with more positive responses to COVID-19 related measures, including intentions, norms, and beliefs.
test <- lm(intentions~purpose+as.factor(condition), df) #in the main text
summ(test, digits = 3, confint = TRUE)

#H1b. Age will be tested as a potential moderator for H1a, such that higher purpose is more strongly associated with less loneliness for older adults compared to younger adults
test <- lm(lonely_current ~ purpose*age+as.factor(condition), data=df)#in the main text
summ(test, digits = 3, confint = TRUE)




###############################
###not preregistered post-hoc##
###############################

#lonely * purpose * age -->  intentions
test <-lm(intentions~ lonely_current * age * purpose + as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)

#ci.src(beta.k = lm.beta(test))


#simple slopes analysis
print(round((mean(df$age) - sd(df$age)),1))
print(round((mean(df$age)),1))
print(round((mean(df$age) + sd(df$age)),1))
print(round((mean(df$age) + 2*sd(df$age)),1))


#Following procedures by Aiken et al., (1991), all variables were mean centered for this portion of the analysis.
#mean center variables
df$age_cen = df$age - mean(df$age,na.rm=T)
df$intentions_cen = df$intentions- mean(df$intentions,na.rm=T)
#df$intentions_nsd_cen = df$intentions_nsd- mean(df$intentions_nsd,na.rm=T)
df$purpose_cen = df$purpose - mean(df$purpose,na.rm=T)
df$lonely_cen = df$lonely_current - mean(df$lonely_current,na.rm=T)

#+- 1 sd for age (for reasons I do not completely understand, SDs are added to mean for  younger, and subtracted for older slopes "moving the zeros around"?)
df$young <- df$age_cen + sd(df$age_cen, na.rm=T)
df$old <- df$age_cen - sd(df$age_cen, na.rm=T)
df$old2 <- df$age_cen - 2*sd(df$age_cen, na.rm=T)

summary(lm(intentions_cen~ young * lonely_cen + as.factor(condition), df))
summary(lm(intentions_cen~ age_cen * lonely_cen+ as.factor(condition), df))
summary(lm(intentions_cen~ old * lonely_cen + as.factor(condition), df))
summary(lm(intentions_cen~ old2 * lonely_cen + as.factor(condition), df))

# We also did an additional analysis by dividing the sample into three age groups including younger (18-35 years; n=265), middle-aged (36-55 years; n=183), and older adults (older than 55 years; n=49)
young_adults =subset(df,age<=35)
middle_aged=subset(df,age>36 & age <=55) 
older_adults = subset (df, age>55) 

#Consistent with the results from simple slopes analysis described above, higher loneliness was associated with lower preventative behavior intentions, including intentions to socially distance, only among older adults

test <-lm(intentions~lonely_current+as.factor(condition), older_adults) 
summ(test, digits = 3, confint = TRUE)

test <-lm(intentions~lonely_current+as.factor(condition), middle_aged)
summ(test, digits = 3, confint = TRUE)

test <-lm(intentions~lonely_current+as.factor(condition), young_adults) 
summ(test, digits = 3, confint = TRUE)


#Next, we parsed the three way interaction between age, purpose and loneliness on intentions to engage in COVID-19 prevention behaviors

#simple slopes analysis for three way interaction
print(purpose_low<- round((mean(df$purpose) - sd(df$purpose)),1))
print(purpose_mid<- round((mean(df$purpose)),1))
print(purpose_high <- round((mean(df$purpose) + sd(df$purpose)),1))


#+-sd for purpose
df$purpose_low <- df$purpose_cen + sd(df$purpose_cen, na.rm=T)
df$purpose_high <- df$purpose_cen - sd(df$purpose_cen, na.rm=T)

summary(lm(intentions_cen~ purpose_low * lonely_cen*age_cen+ as.factor(condition), df))
summary(lm(intentions_cen~ purpose_cen * lonely_cen*age+ as.factor(condition), df))
summary(lm(intentions_cen~ purpose_high * lonely_cen*age_cen+ as.factor(condition) , df))


#########################
##### PLOTS/FIGURES #####
#########################

#Figure 1
#age histogram

print(age_minus2sd <- round((mean(df$age) + 2*sd(df$age)),1))
print(age_minus1sd <- round((mean(df$age) + sd(df$age)),1))
print(age_mean<- round((mean(df$age)),1))
print(age_plus1sd <- round((mean(df$age) - sd(df$age)),1))


young_adults =subset(df,age<=35) 
middle_aged=subset(df,age>36 & age <=55) 
older_adults = subset (df, age>55) 


set.seed(1234)
ggplot(df, aes(x = age))+geom_histogram(aes(y = ..count..), colour="#2080F6",fill="#2080F6", binwidth = 1, alpha = 0.3) +
  theme_bw() + theme(panel.grid.minor = element_blank()) +
  ylim (-2, 30) + xlim(19, 74) + scale_x_continuous(breaks = seq(15, 75, by = 5))


#Figure 2A
ggplot (df,aes(purpose,lonely_current))+
  geom_point(shape=21,size=2.5,colour="white",fill="#2080F6")+
  geom_smooth(method=lm,color='red', size=.5)+
  theme(text=element_text(size=15))+
  xlab("purpose in life") + ylab ("loneliness") +
  theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="black"))

#Figure 2B
ggplot (df,aes(purpose,intentions))+
  geom_point(shape=21,size=2.5,colour="white",fill="#2080F6")+
  geom_smooth(method=lm,color='red', size=.5)+
  theme(text=element_text(size=15))+
  xlab("purpose in life") + ylab ("COVID prevention intentions") +
  theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.line=element_line(colour="black"))


#Figure 2C
#Raw scores are displayed for the ease of interpretation.
test <- lm(intentions ~ lonely_current * age +as.factor(condition) , df)
interact_plot(test, pred = lonely_current, modx = age, interval = TRUE, 
              int.type = "confidence", int.width = 0.95, modx.values = c(26.4, 37.7, 49, 60.3)) 

#Figure 2D
test <- lm(intentions ~ lonely_current * age *purpose +as.factor(condition) , df)
interact_plot(test, pred = lonely_current, modx = age, mod2 = purpose, interval = TRUE, 
              int.type = "confidence", int.width = 0.95, modx.values = c(26.4, 37.7, 49, 60.3)) 


#####################
####SUPPLEMENTALS####
#####################

#SI2. Associations between self-reported purpose in life and responses to COVID-19 related measures, including norms and beliefs. 
#Across participants, higher self-reported purpose in life was associated with higher perceived norms about COVID-19 preventative behaviors among social ties

test <-lm(norms_close~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)

#Further, higher purpose in life was associated with greater beliefs that taking COVID preventative measures will be approved by their social ties 
test <-lm(beliefs_norms~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)

test <-lm(beliefs_safe_others~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)

test <-lm(beliefs_safe_self~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)


#SI3. Exploratory: COVID-related outcomes specifically related to social distancing.
test <- lm(intentions_sd~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(intentions_hand~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(intentions_home~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)

test <- lm(norms_close_sd~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(norms_close_hand~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(norms_close_home~purpose+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)


test <- lm(intentions_sd~purpose*age*lonely_current+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(intentions_hand~purpose*age*lonely_current+as.factor(condition), df)
summ(test, digits = 3, confint = TRUE)
test <- lm(intentions_home~purpose*age*lonely_current+as.factor(condition), df)
summ(test, digits = 2, confint = TRUE)



summary(lm(norms_close_hand~purpose+as.factor(condition), df))
summary(lm(norms_close_home~purpose+as.factor(condition), df))
#summary(lm(norms_close_nsd~purpose+as.factor(condition), df))


#SI4. Exploratory: Potential covariates for analysis including loneliness. 

summary(lm(lonely_current ~ purpose+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, data=df))#continued to be sig
summary(lm(lonely_current ~ purpose*age+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, data=df))#continued to be ns
summary(lm(intentions~lonely_current+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, df))#continued to be ns
summary(lm(norms_close~lonely_current+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, df))#continued to be ns
summary(lm(beliefs_norms~lonely_current+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, df))#continued to be ns
summary(lm(beliefs_safe_self~lonely_current+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, df))#continued to be ns 
test <- lm(beliefs_safe_others~lonely_current+as.factor(condition)+house_size_curre+as.factor(gender)+ses_subj, df)#became significant in the opposite direction from expected
summ(test, digits = 3, confint = TRUE)

#SI5. Exploratory: Associations between perceived loneliness and responses to COVID-19 related measures.
summary(lm(intentions~lonely_current+as.factor(condition), df)) 
summary(lm(norms_close~lonely_current+as.factor(condition), df)) 
summary(lm(beliefs_norms~lonely_current+as.factor(condition), df)) 
summary(lm(beliefs_safe_others~lonely_current+as.factor(condition), df)) 
summary(lm(beliefs_safe_self~lonely_current+as.factor(condition), df)) 

#SI6. Exploratory: Subsetting the sample. 
df2 =subset (df,condition=='no message control')

test <-lm(lonely_current ~ purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(intentions~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(intentions_sd~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(norms_close~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(norms_close_sd~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(beliefs_norms~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(beliefs_safe_others~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(beliefs_safe_self~purpose, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(lonely_current ~ purpose*age, df2)
summ(test, digits = 3, confint = TRUE)

test <-lm(lonely_current ~ purpose+house_size_curre+as.factor(gender)+ses_subj, df2)#became ns
summ(test, digits = 3, confint = TRUE)

summary(lm(lonely_current ~ purpose*age+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns
summary(lm(intentions~lonely_current+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns
summary(lm(norms_close~lonely_current+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns
summary(lm(beliefs_norms~lonely_current+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns
summary(lm(beliefs_safe_self~lonely_current+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns 
summary(lm(beliefs_safe_others~lonely_current+house_size_curre+as.factor(gender)+ses_subj, df2))#continued to be ns 

