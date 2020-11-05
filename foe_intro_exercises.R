#A. Longevity--------------------------------------------------------------------------
#1) What is the average age at death of women born in England and Wales, 1860 - 1869?

rm(foe)
attach(foe_copy)


women = c(which(foe_copy$dfem == 1))

womenFOE = subset(foe_copy,foe_copy$dfem == 1 & (foe_copy$byr >= 1860 & foe_copy$byr<=1869) & (regbirth < 10 ))#create a subset of the Original DF so 
                                              #it only contains women 
View(womenFOE)

mean(womenFOE$dage, na.rm = TRUE)#calculate average age at death, removing NAs
which.max(womenFOE$byr) #making sure the maximum year of death is 1869 (correct)

#2) What is the average age at death of women born in England and Wales, 1860-1869,
#who married and lived to at least age 21?-------------------------------------------

marriedAnd21= subset(womenFOE, womenFOE$dmarried==1 & d21==1 &  (regbirth < 10))#create a new subset
                                                            #that meets the criteria
View(marriedAnd21)
mean(marriedAnd21$dage,na.rm=TRUE)#calculate the average age at death of women
                                  #who married and lived to at least age 21


#B. Intergenerational Mobility-------------------------------------------------------
#1) What is the correlation in occupational status between grandfathers and grandsons?
library(haven)

library(dplyr)
library(stargazer)

sons <- foe %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>%         #keep all men
  select(Occrank,pid,pid_fath)#select these three columns for the new tibble


View(sons)
# create a table for fathers
fathers<-foe %>% 
  filter(!is.na(pid_fath)) %>% 
  filter(dfem==0) %>% #keep all men
  select(Occrank_fath = Occrank, pid_gfath = pid_fath, pid_fath = pid) #keeping the Occrank of men, and 
#renaming to Occrank_fath.Since these
#are supposed to be fathers, we are calling their pid -> pid_fath. This is how
#we will be able to link the sons to the fathers, because the sons pid_fath will
#potentially match with someone in the father df who has the same pid_fath value


#now create a table for the fathers of fathers (grandfathers)
grandFathers = foe %>% 
  filter(!is.na(pid_fath)) %>% #only want people with fathers 
  filter(dfem==0) %>%  #only males
  #this extracts the fathers from fathers df
  select(Occrank_gfath = Occrank, pid_gfath = pid) #since we want a df of grandfathers
                                                    #we call pid -> pid_gfath and same for 
                                                    #Occrank (call it Occrank_gfath)

#join sons and fathers
joined<-inner_join(sons,fathers)#
View(joined)                         

#join joined df with grandfathers
joinedWithGFathers = inner_join(joined, grandFathers) %>% 
  filter(!is.na(Occrank)&!is.na(Occrank_gfath))
View(joinedWithGFathers)

#check correlation between occupational status between grandfathers and grandsons
cor.test(joinedWithGFathers$Occrank, joinedWithGFathers$Occrank_gfath)


#2)What is the correlation in occupational status between brothers?

#create a dataframe for brothers
brothers = foe %>% 
  filter(dfem==0) %>% 
  select(Occrank_brother = Occrank, pid_brother = pid, pid_fath) #pid_fath is join predicate

#merge sons and Brothers
sonsAndBrothers = inner_join(sons, brothers) %>%  
  filter(pid != pid_brother)  #dont want any overlap (someone being a brother with themself)
  View(sonsAndBrothers)
  
cor.test(sonsAndBrothers$Occrank,sonsAndBrothers$Occrank_brother)

#3)What is the correlation in logarithm of house values between (all) cousins
head(foe$vzpcln2017)#useless

#create a dataframe for cousins (link by grandfather?)\

#update sons data frame to include log average house values 
sons = foe %>% 
  filter(!is.na(pid_fath)) %>%
  filter(dfem==0) %>% 
  filter(!is.na(vzpcln2017)) %>% 
  select(pid, vzpcln2017, pid_fath)

#same thing for fathers
fathers = foe %>% 
  filter(dfem==0) %>% 
  filter(!is.na(pid_fath)) %>% 
  select(pid_grandfather = pid_fath, pid_fath = pid)#I really like to think of these as
                                                    #possible fathers/grandfathers
                                              #the inner_join will establish if they are
                                              #or not

#now merge the sons and fathers dataframes to create a family
sonsAndFathers = inner_join(sons,fathers) #should have sons, fathers, and grandfathers

#create a dataframe of (potential) cousins
cousins = sonsAndFathers %>%  
  select(pid_cousin = pid, logHouseValue = vzpcln2017, pid_cousin_father= pid_fath,
         pid_grandfather)

sonsFathersCousins = inner_join(sonsAndFathers,cousins) %>% #merge based on pid_grandfather
  filter(pid_fath != pid_cousin_father & pid != pid_cousin) %>% #
  filter(pid < pid_cousin)

View(sonsFathersCousins)

cor.test(sonsFathersCousins$logHouseValue,sonsFathersCousins$vzpcln2017)

#C Selective Migration--------------------------------------------------------------------
#What is the average occupational status of FATHERS of who people
#a)Were born in the north of England but die in the south or London

sons <- foe %>% 
  filter(!is.na(pid_fath)) %>% #dont include values that are NA for pid_fath
  filter(dfem==0) %>%         #keep all men
  select(Occrank,pid,pid_fath,regbirth,regdeath)#select these three columns for the new tibble

# create a table for fathers
fathers<-foe %>% 
  filter(!is.na(pid_fath)) %>% 
  filter(dfem==0) %>% #keep all men
  select(Occrank_fath = Occrank, pid_gfath = pid_fath, pid_fath = pid)

#join sons and fathers (now with variables regdeath and regbirth)
joined<-inner_join(sons,fathers)#
View(joined)             

dfA = joined %>% 
  filter(regbirth==1) %>% #must have been born in England
  filter(regdeath==0 | regdeath==4) %>% #died in either south or london %>% 
  filter (!is.na(Occrank_fath)) %>% 
  select(pid,Occrank, pid_fath, Occrank_fath)

View(dfA)

mean(dfA$Occrank_fath)#calculate the average Occrank of the father based on conditions

#b)Were born in the south of England or London but die in the North
dfB = joined %>% 
  filter(regbirth == 0) %>% 
  filter(regdeath == 1) %>% 
  filter (!is.na(Occrank_fath)) %>% 
  select(pid,Occrank, pid_fath, Occrank_fath)

View(dfB)

mean(dfB$Occrank_fath)

#c) Were born in England but die in the USA
dfC = joined %>% 
  filter(regbirth<10 & regbirth != 2) %>% #dont want to include people in Wales
  filter(regdeath==14) %>% 
  filter (!is.na(Occrank_fath)) %>% 
  select(pid,Occrank, pid_fath, Occrank_fath)

mean(dfC$Occrank_fath)
  
  


  

