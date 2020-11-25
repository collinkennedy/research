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
library(haven)
library(lfe) #for fixed effects
library(stargazer) #for creating tables
#rm(list=ls())

#get data from FOE database
foe_copy = read_dta("/Users/collinkennedy/Dropbox/Ecn 198 2020 Fall/FOE Database/foe.dta")
View(foe_copy)
attach(foe_copy)

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



  

foe_copy<-foe_copy %>% 
  mutate(count=1) %>% #
  group_by(pid) %>% 
  mutate(duplicates = sum(count))

table(foe_copy$duplicates) %>% print

# drop duplicates
foe_copy<-filter(foe_copy,duplicates==1)

# make sure no missing pids
stopifnot(nrow(filter(foe_copy,is.na(pid)))==0)


#create an inherited wealth variable and attach it do the foe_copy dataset


#Merge Wives
wives = foe_copy %>% 
  filter(dfem==1) %>%
  select(spouse_dage = dage,
         pid_spouse1 = pid)

View(wives)



#create sons and fathers dataframes (tibbles)

sons = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>%#keep all men
  left_join(wives) %>%  #left joining sons and wives
  select(pid_son = pid, #personal id
         dage_son = dage,  #death age of the son
         dmarried_son = dmarried, #whether or not son married
         ded_son=ded,            #whether or not received higher education
         Occrank_son = Occrank, 
         d21_son = d21,         #whether or not lived to 21
         regbirth_son = regbirth, #region of birth 
         spouse_dage_son = spouse_dage,
         pid_fath) #join predicate , father id


fathers = foe_copy %>% 
  mutate(inherited_lnwealth = (lnw/nbirth)) %>% #CHECK THIS
  filter(!is.na(pid_fath)) %>% 
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

View(sons_and_fathers)


#create brothers dataframe, then merge to create the family dataframe
brothers = foe_copy %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>%         #keep all men
  left_join(wives) %>% #left join sons and wives
  select(pid_broth = pid, #personal id
         dage_broth = dage,  #death age of the brother
         dmarried_broth = dmarried, #whether or not bropther married
         ded_broth=ded,            #whether or not received higher education
         Occrank_broth = Occrank, 
         d21_broth = d21,         #whether or not lived to 21
         regbirth_broth = regbirth, #region of birth 
         spouse_dage_broth = spouse_dage,
         pid_fath) #join predicate , father id


#create the family dataframe, will need to incorporate the inherited wealth variable later
family = inner_join(sons_and_fathers,brothers) #noticed there are duplicates?
#i know why, they are getting matched with themselves

family = family %>% 
  filter(pid_son<pid_broth) 

View(select(family,pid_son,spouse_dage_son, pid_broth, spouse_dage_broth))#still duplicates..... assuming its because of multiple brothers
#will that be a problem when it comes to building the model

qf(.95,3,522)





















