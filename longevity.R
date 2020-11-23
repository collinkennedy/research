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
foe<-filter(foe_copy,duplicates==1)

# make sure no missing pids
stopifnot(nrow(filter(foe_copy,is.na(pid)))==0)































