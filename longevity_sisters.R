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






#Merge husbands
husbands = foe_copy %>% 
  filter(dfem==0) %>%
  select(spouse_dyr = dyr,
         pid_spouse1 = pid,
         spouse_d40 = d40,
         spouse_byr = byr)






#create daughters and fathers dataframes (tibbles)

daughters = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==1) %>% 
  left_join(husbands) %>%  #left joining daughters and wives
  select(pid_daughter = pid, #perdaughteral id
         dage_daughter = dage,  #death age of the daughter
         dmarried_daughter = dmarried, #whether or not daughter married
         ded_daughter=ded,            #whether or not received higher education
         Occrank_daughter = Occrank, 
         d21_daughter = d21,         #whether or not lived to 21
         regbirth_daughter = regbirth, #region of birth 
         spouse_dyr_daughter = spouse_dyr,
         spouse_d40_daughter = spouse_d40,
         byr_daughter = byr,
         d40_daughter = d40,
         myr1_daughter = myr1, #year of first marriage
         pid_fath) #join predicate , father id




#don't care about father's birth year so not going to add it
fathers = foe_copy %>% 
  mutate(inherited_lnwealth = (lnw/nbirth)) %>% #CHECK THIS
  filter(dfem==0) %>% 
  select(pid_fath = pid, #perdaughteral id assigned as pid_fath, join predicate
         dage_fath = dage,  #death age of the daughter
         dmarried_fath = dmarried, #whether or not daughter married
         ded_fath=ded,            #whether or not received higher education
         Occrank_fath = Occrank, 
         d21_fath = d21,         #whether or not lived to 21
         regbirth_fath = regbirth, #region of birth 
         lnwealth_fath = lnw,
         inherited_lnwealth)



#merge daughters and fathers
daughters_and_fathers = inner_join(daughters,fathers)





#create sisters dataframe, then merge to create the family dataframe
sisters = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==1) %>%         #keep all men
  left_join(husbands) %>% #left join daughters and wives
  select(pid_sis = pid, #perdaughteral id
         dage_sis = dage,  #death age of the siser
         dmarried_sis = dmarried, #whether or not siser married
         ded_sis=ded,            #whether or not received higher education
         Occrank_sis = Occrank, 
         d21_sis = d21,         #whether or not lived to 21
         regbirth_sis = regbirth, #region of birth 
         spouse_dyr_sis = spouse_dyr, #the death year of the siser's spouse
         spouse_d40_sis = spouse_d40,
         myr1_sis = myr1, #the year the siser got married
         byr_sis = byr,
         d40_sis = d40,
         pid_fath) #join predicate , father id


#create the family dataframe, will need to incorporate the inherited wealth variable later
family = inner_join(daughters_and_fathers,sisters) #noticed there are duplicates?
#i know why, they are getting matched with themselves

family = family %>% 
  filter(pid_daughter<pid_sis) #make sure there is no matching of daughters to themselves 





#=========================================================================================
#Ylongevity= β0+ β1Xdmarried+ β2Xded + β3XOccrank + 
#β4Xd21 + β5Xregbirt h +β6XinheritedLnwealth

#H0: dmarried = 0
#H1:dmarried != 0 
#=========================================================================================
#build linear model 1
ggplot(data=family, mapping = aes(x = dmarried_daughter, y = dage_daughter))+
  geom_point()

plotData = family %>% mutate(dage_daughter = round(dage_daughter,0)) %>% 
  group_by(dage_daughter) %>% 
  summarize(dmarried_daughter = mean(as.numeric(dmarried_daughter),na.rm =TRUE))
plotData


ggplot(data = plotData, mapping = aes(x = dage_daughter,y = dmarried_daughter))+
  geom_point()


#use robust standard errors here, bring in sandwich and lmtest packages
model1 = lm(dage_daughter ~ dmarried_daughter + ded_daughter 
            + d40_daughter + regbirth_daughter + inherited_lnwealth, data = family)

summary(model1)




#========================================================================================
#H0: ∆Xdmarried= 0 (dmarried_daughter - dmarried_sis)
#Ha: ∆Xdmarried6 != 0

#∆Ylongevity=β0+β1∆Xdmarried+β2∆Xded+β3∆XOccrank+β4∆Xd21+β5∆Xregbirth
#========================================================================================
#define variables:

#changed the filtering from requiring both daughter and siser to live 
#to at least 21, to having to live to at least age 40
regSample2 = family %>% filter(d40_daughter == 1 & d40_sis == 1) #only want to consider sisters that live until at 
#least 21


#don't include people getting married after 40
regSample2 = regSample2 %>% 
  filter((myr1_daughter < byr_daughter + 40) | is.na(myr1_daughter)) %>% #throws out people getting 
  #marriage after age 40 but not people who never got married
  filter((myr1_sis < byr_sis + 40) | is.na(myr1_sis))
#if their marriage year is less than their birth year + 40, keep

#DID I DO THIS RIGHT??



nrow(regSample2)


#define differenced variables
delta_dage = regSample2$dage_daughter - regSample2$dage_sis
delta_dmarried = regSample2$dmarried_daughter - regSample2$dmarried_sis
delta_ded = regSample2$ded_daughter - regSample2$ded_sis
delta_Occrank = regSample2$Occrank_daughter - regSample2$Occrank_sis
delta_d21 = regSample2$d21_daughter - regSample2$d21_sis
same_regbirth = regSample2$regbirth_daughter == regSample2$regbirth_sis #categorical


#try some other death age cut offs.............

#?lm_robust()
#linear model 2
model2 = lm(delta_dage ~ delta_dmarried + delta_ded + same_regbirth) #Am i picking up an upward trend in longevity that I'm
#not controlling for -> appropriate to use time fixed effects?

#decade of birth, control for

summary(model2)

stargazer(model2, type = "text")


#output interpretation: Reject the null hypothesis at the 5% significance level in favor
#of the alternative hypothesis, and conclude that delta_dmarried has a nonzero effect on the difference in 
#death age between sisters.

#when filtering on d21_daughter and d21_sis == 1
#In other words, for sisters who both live to at least 21, the siser who gets married lives about 13.6 years
#longer on average, holding difference in educational achievement, difference in occupation rank,.


#when filtering on d40_daughter and d40_sis == 1
#In other words, for sisters who both live to at least 40, the siser who gets married lives about 2.73 years
#longer on average, holding difference in educational achievement, difference in occupation rank,.


#when filtering on d40_daughter and d40_sis == 1, but only looking at sisters
#who also married before age 40, the daughter who got married appears to live about 3.2
#years longer on average



#=======================================================================================
#Comparing longevity between people who were married for (basically) their entire life
#versus people who married, but whose spouse died early in the marriage


#========================================================================================

regSample3 = family %>% filter(dmarried_sis == 1 & dmarried_daughter == 1)%>% 
  filter(d40_daughter == 1 & d40_sis == 1)#only want to consider when both sisters are married
#filter on d40 instead of d21 (only include sisters living to at least age 40)


#like in model 2, don't include people getting married after 40
regSample3 = regSample3 %>% 
  filter((myr1_daughter < byr_daughter + 40) | is.na(myr1_daughter)) %>% #throws out people getting 
  #marriage after age 40 but not people who never got married
  filter((myr1_sis < byr_sis + 40) | is.na(myr1_sis))

delta_dage = regSample3$dage_daughter - regSample3$dage_sis
#delta_dmarried = regSample$dmarried_daughter - regSample$dmarried_sis, don't need
delta_ded = regSample3$ded_daughter - regSample3$ded_sis
delta_Occrank = regSample3$Occrank_daughter - regSample3$Occrank_sis
delta_d21 = regSample3$d21_daughter - regSample3$d21_sis
same_regbirth = regSample3$regbirth_daughter == regSample3$regbirth_sis

long_marriage_daughter = regSample3$spouse_dyr_daughter - regSample3$myr1_daughter > 10 #categorical variable- 
#if the marriage lasts at least 10 years before the spouse dies, then  ->1 , else 0

#same thing but for the siser
long_marriage_sis = regSample3$spouse_dyr_sis - regSample3$myr1_sis > 10

#if the difference is greater than 
delta_long_marriage = long_marriage_daughter - long_marriage_sis #
#1 if daughter has a long marriage but sis has a short marriage
#0 if daughter and siser have "equal length" marriage
#-1 if daughter has a SHORT marriage and sis has a long marriage

model3 = lm(delta_dage ~ delta_long_marriage + delta_ded + same_regbirth)

summary(model3)


#filtering on d21 and considering marriages at any point in time
#current output: if the first siser's wife lives longer than 10 years after marriage, whereas 
#the second siser's wife does NOT, then the life expectancy of the first siser is about 
#2 years longer than the second siser's life expectancy, on average, ceteris parabus.


#filtering on d40 and only considering marriages where the daughter and siser are 
#younger than 40
#there appears to be no statistically discernible difference in longevity
#between sisters who both marry before 40, who live to at least 40, but 

#interpret as a zero effect
#=======================================================================================

#Model 3 Robustness Check

#=======================================================================================
#drop people who die in the same year as their wife
#add death year variable to daughter and sis

#in regsample3

#alter cut off for long/short marriage -> maybe 1, 20








