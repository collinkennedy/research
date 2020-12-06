#How are educational and occupational success associated with Longevity?

#variables of immediate concern and relevance:---------------------------------------------
#pid:	Unique person ID

#dmarried:	dummy = 1 if ever married

#dfem:	dummy = 1 if female
#dyr	death year (integer)
#byr:	birth year (integer)
#doxb:	dummy = 1 if attended Oxford or Cambridge
#duniv:	dummy = 1 if attended other university
#dlaw:	dummy = 1 if law qualification
#Deng:	dummy = 1 if engineering qualification
#dmed:	dummy = 1 if medical qualification
#dsandhurst:	dummy = 1 if attended military college
#dcleric:	dummy = 1 if ordained priest or minister
#daccountant:	dummy = 1 if accounting qualification
#ded:	dummy = 1 if some higher education qualification



#==========================================================================================#
#rm(list = ls())
library(tidyverse)
library(ggplot2)
library(haven)
library(estimatr)
library(lfe) #for fixed effects
library(stargazer) #for creating tables
rm(list=ls())

#get data from FOE database
foe_copy = read_dta("/Users/collinkennedy/Dropbox/Ecn 198 2020 Fall/FOE Database/foe.dta")


#men = foe_copy %>% 
  #filter(dfem == 0) #filter out the women so its only men

#reg = felm(dage ~ ded,data = men) #standard regression of death age on 
#summary(reg)

#reg2 = felm(dage ~ ded | pid_fath,data = men) #
#summary(reg2)

#stargazer(reg,reg2,type = "text", add.lines = list(c("Fixed Effects","No","Yes")))


#=========================================================================================#
#How is getting married associated with longevity:

#-----------------------------------------------------------------------------------------#

#subset foe to create tibble including variables I intend to include in my model(s)

#filter() retains all rows that satisfy the given condition


#add d40 variable (indicator{1-> lived to 40, 0-> otherwise})
foe_copy = foe_copy %>% 
  mutate(d40 = ifelse(dage >= 40, 1, 0))

  

foe_copy<-foe_copy %>% 
  mutate(count=1) %>% #
  group_by(pid) %>% 
  mutate(duplicates = sum(count))

table(foe_copy$duplicates) %>% print

# drop duplicates
foe_copy<-filter(foe_copy,duplicates==1)

# make sure no missing pids
stopifnot(nrow(filter(foe_copy,is.na(pid)))==0)






#Merge Wives
wives = foe_copy %>% 
  filter(dfem==1) %>%
  select(spouse_dyr = dyr,
         pid_spouse1 = pid,
         spouse_d40 = d40,
         spouse_byr = byr)






#create sons and fathers dataframes (tibbles)

sons = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>% 
  left_join(wives) %>%  #left joining sons and wives
  select(pid_son = pid, #personal id
         dage_son = dage,  #death age of the son
         dmarried_son = dmarried, #whether or not son married
         ded_son=ded,            #whether or not received higher education
         Occrank_son = Occrank, 
         d21_son = d21,         #whether or not lived to 21
         regbirth_son = regbirth, #region of birth 
         spouse_dyr_son = spouse_dyr,
         spouse_d40_son = spouse_d40,
         byr_son = byr,
         d40_son = d40,
         myr1_son = myr1, #year of first marriage
         pid_fath) #join predicate , father id




#don't care about father's birth year so not going to add it
fathers = foe_copy %>% 
  mutate(inherited_lnwealth = (lnw/nbirth)) %>% #CHECK THIS
  filter(dfem==0) %>% 
  select(pid_fath = pid, #personal id assigned as pid_fath, join predicate
         dage_fath = dage,  #death age of the son
         dmarried_fath = dmarried, #whether or not son married
         ded_fath=ded,            #whether or not received higher education
         Occrank_fath = Occrank, 
         d21_fath = d21,         #whether or not lived to 21
         regbirth_fath = regbirth, #region of birth 
         lnwealth_fath = lnw,
         inherited_lnwealth)



#merge sons and fathers
sons_and_fathers = inner_join(sons,fathers)





#create brothers dataframe, then merge to create the family dataframe
brothers = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>%         #keep all men
  left_join(wives) %>% #left join sons and wives
  select(pid_broth = pid, #personal id
         dage_broth = dage,  #death age of the brother
         dmarried_broth = dmarried, #whether or not brother married
         ded_broth=ded,            #whether or not received higher education
         Occrank_broth = Occrank, 
         d21_broth = d21,         #whether or not lived to 21
         regbirth_broth = regbirth, #region of birth 
         spouse_dyr_broth = spouse_dyr, #the death year of the brother's spouse
         spouse_d40_broth = spouse_d40,
         myr1_broth = myr1, #the year the brother got married
         byr_broth = byr,
         d40_broth = d40,
         pid_fath) #join predicate , father id


#create the family dataframe, will need to incorporate the inherited wealth variable later
family = inner_join(sons_and_fathers,brothers) #noticed there are duplicates?
#i know why, they are getting matched with themselves

family = family %>% 
  filter(pid_son<pid_broth) #make sure there is no matching of sons to themselves 





#=========================================================================================
#Ylongevity= β0+ β1Xdmarried+ β2Xded + β3XOccrank + 
#β4Xd21 + β5Xregbirt h +β6XinheritedLnwealth

#H0: dmarried = 0
#H1:dmarried != 0 
#=========================================================================================
#build linear model 1
ggplot(data=family, mapping = aes(x = dmarried_son, y = dage_son))+
  geom_point()

#use robust standard errors here, bring in sandwich and lmtest packages
model1 = lm(dage_son ~ dmarried_son + ded_son + Occrank_son
            + d40_son + regbirth_son + inherited_lnwealth, data = family)

summary(model1)


dfdff

#========================================================================================
#H0: ∆Xdmarried= 0 (dmarried_son - dmarried_broth)
#Ha: ∆Xdmarried6 != 0

#∆Ylongevity=β0+β1∆Xdmarried+β2∆Xded+β3∆XOccrank+β4∆Xd21+β5∆Xregbirth
#========================================================================================
#define variables:

#changed the filtering from requiring both son and brother to live 
#to at least 21, to having to live to at least age 40
regSample2 = family %>% filter(d40_son == 1 & d40_broth == 1) #only want to consider brothers that live until at 
                                                             #least 21


#only want to consider people to live to at least 40



#don't include people getting married after 40


delta_dage = regSample2$dage_son - regSample2$dage_broth
delta_dmarried = regSample2$dmarried_son - regSample2$dmarried_broth
delta_ded = regSample2$ded_son - regSample2$ded_broth
delta_Occrank = regSample2$Occrank_son - regSample2$Occrank_broth
delta_d21 = regSample2$d21_son - regSample2$d21_broth
same_regbirth = regSample2$regbirth_son == regSample2$regbirth_broth #categorical


#try some other death age cut offs.............

#?lm_robust()
#linear model 2
model2 = lm(delta_dage ~ delta_dmarried + delta_ded + 
              delta_Occrank + same_regbirth) #Am i picking up an upward trend in longevity that I'm
                                                          #not controlling for -> appropriate to use time fixed effects?

#decade of birth, control for

summary(model2)

stargazer(model2, type = "text")


#output interpretation: Reject the null hypothesis at the 5% significance level in favor
#of the alternative hypothesis, and conclude that delta_dmarried has a nonzero effect on the difference in 
#death age between brothers.

#when filtering on d21_son and d21_broth == 1
#In other words, for brothers who both live to at least 21, the brother who gets married lives about 13.6 years
#longer on average, holding difference in educational achievement, difference in occupation rank,.


#when filtering on d40_son and d40_broth == 1
#In other words, for brothers who both live to at least 40, the brother who gets married lives about 2.73 years
#longer on average, holding difference in educational achievement, difference in occupation rank,.



#dmarried in the first model /delta_dmarried in the second model coefficient(s) must be  picking up the effect of a factor I am not controlling for but
#i have no idea what it is

#=======================================================================================
#Comparing longevity between people who were married for (basically) their entire life
#versus people who married, but whose spouse died early in the marriage


#========================================================================================

regSample3 = family %>% filter(dmarried_broth == 1 & dmarried_son == 1)%>% 
  filter(d21_son == 1 & d21_broth == 1)#only want to consider when both brothers are married

delta_dage = regSample3$dage_son - regSample3$dage_broth
#delta_dmarried = regSample$dmarried_son - regSample$dmarried_broth, don't need
delta_ded = regSample3$ded_son - regSample3$ded_broth
delta_Occrank = regSample3$Occrank_son - regSample3$Occrank_broth
delta_d21 = regSample3$d21_son - regSample3$d21_broth
same_regbirth = regSample3$regbirth_son == regSample3$regbirth_broth

long_marriage_son = regSample3$spouse_dyr_son - regSample3$myr1_son > 10 #categorical variable- 
#if the marriage lasts at least 10 years before the spouse dies, then  ->1 , else 0

#same thing but for the brother
long_marriage_broth = regSample3$spouse_dyr_broth - regSample3$myr1_broth > 10

#if the difference is greater than 
delta_long_marriage = long_marriage_son - long_marriage_broth #
#1 if son has a long marriage but broth has a short marriage
#0 if son and brother have "equal length" marriage
#-1 if son has a SHORT marriage and broth has a long marriage

model3 = lm(delta_dage ~ delta_long_marriage + delta_ded + 
              delta_Occrank + same_regbirth)

summary(model3)

#current output: if the first brother's wife lives longer than 10 years after marriage, whereas 
#the second brother's wife does NOT, then the life expectancy of the first brother is about 
#2 years longer than the second brother's life expectancy, on average, ceteris parabus.



#=======================================================================================

#Model 3 Robustness Check

#=======================================================================================
#drop people who die in the same year as their wife
#add death year variable to son and broth

#in regsample3










