#How is marriage associated with Longevity?

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
rm(list=setdiff(ls(),c("foe_copy")))



#get data from FOE database
if(!exists("foe_copy")){
  foe_copy = read_dta("/Users/collinkennedy/Dropbox/Ecn 198 2020 Fall/FOE Database/foe2020.dta")
}


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
#stopifnot(nrow(filter(foe_copy,is.na(pid)))==0)






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
         spouse_byr_son = spouse_byr, #birth year of the son's spouse
         byr_son = byr,
         d40_son = d40,
         myr1_son = myr1, #year of first marriage
         myr_2_son = myr_2, #year of second marriage
         myr_3_son = myr_3,#year of third marriage
         pid_fath) #join predicate , father id




#don't care about father's birth year so not going to add it
fathers = foe_copy %>%
  filter(lnw != 0 & nbirth != 0) %>% 
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
         spouse_byr_broth = spouse_byr,
         myr1_broth = myr1, #the year the brother got married
         myr_2_broth = myr_2,#year of second marriage
         myr_3_broth = myr_3, #3rd marriage year
         byr_broth = byr, 
         d40_broth = d40, #dummy for whether or not they lived to age 40
         pid_fath) #join predicate , father id


#create the family dataframe, will need to incorporate the inherited wealth variable later
family = inner_join(sons_and_fathers,brothers) #noticed there are duplicates?
#i know why, they are getting matched with themselves

family = family %>% 
  filter(pid_son<pid_broth) #make sure there is no matching of sons to themselves 


#Visualize considering whole range of death age
plotData = family %>% mutate(dage_son = round(dage_son,0)) %>% 
  group_by(dage_son) %>% 
  summarize(dmarried_son = mean(as.numeric(dmarried_son),na.rm =TRUE))
plotData

ggplot(data = plotData, mapping = aes(x = dage_son,y = dmarried_son))+
  geom_point()


#only considering data beyond death age of 40
plotData40 = plotData %>% 
  filter(dage_son>=40) %>% 
  filter(dmarried_son>0)
ggplot(data = plotData40, mapping = aes(x = dage_son, y = dmarried_son))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  labs(title = "Proportion of Sons that Married,",subtitle = "grouped by Death Age")+
  ylab("Proportion of Sons that Married")+
  xlab("Death Age")


ggsave(filename = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/dmarried_vs_dage.png")

#here I tried to visualize the relationship between dmarried and death age of sons. 
#After I had done the bulk of my analysis, I realized or rather felt that I had irresponsibly and
#blindly applied regression to this problem without even checking if there is even a remotely 
#linear relationship between my primary variables of interest. This put my worries to rest.
#In order to produce this plot, I subsetted my family dataframe by grouping by dage_son (which explains why there isn't
#20,000+ observations), and plotted it against the mean numerical value of dmarried_son. dmarried_son
#is a categorical variable, but by converting it to a numerical value and then taking the mean of it
#for each group, and restricting the data to only include people that lived to at least 40,
#the linear relationship is definitely more apparent.

#this justifies the cut off age of 40, data appears to be roughly linear at that point going forward


#=========================================================================================
#Ylongevity= β0+ β1Xdmarried+ β2Xded + β3XOccrank + 
#β4Xd21 + β5Xregbirt h +β6XinheritedLnwealth

#H0: dmarried = 0
#H1:dmarried != 0 
#=========================================================================================
#build linear model 1
#ggplot(data=family, mapping = aes(x = dmarried_son, y = dage_son))+
  #geom_point()

#restrict data to include people that lived to at least 40
#must be married before turning 40, or not married at all (hence NA)
regSample = family %>% 
  filter(d40_son == 1)%>% 
  filter((myr1_son < byr_son + 40) | is.na(myr1_son))

is.na(regSample$myr1_son)
model1 = felm(dage_son ~ dmarried_son + ded_son + Occrank_son + regbirth_son + inherited_lnwealth| byr_son, data = regSample)#used byr_son as 
                                                                              #time fixed effect 
#allow me to indirectly control for factors that are varying over time (this dataset
#spans a significant period of time)

summary(model1)



stargazer(model1,type = "text", title = "Regression Table: Model 1")
stargazer(type = "latex", model1, title = "Regression Table: Model 1",
          out = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/model1out.png")

?stargazer
#========================================================================================
#H0: ∆Xdmarried= 0 (dmarried_son - dmarried_broth)
#Ha: ∆Xdmarried6 != 0

#∆Ylongevity=β0+β1∆Xdmarried+β2∆Xded+β3∆XOccrank+β4∆Xd21+β5∆Xregbirth
#========================================================================================
#define variables:

#changed the filtering from requiring both son and brother to live 
#to at least 21, to having to live to at least age 40
regSample2 = family %>% filter(d40_son == 1 & d40_broth == 1) #only want to consider brothers that live until at 
                                                             #least 40


#don't include people getting married after 40
regSample2 = regSample2 %>% 
  filter((myr1_son < byr_son + 40) | is.na(myr1_son)) %>% #throws out people getting 
#marriage after age 40 but not people who never got married
  filter((myr1_broth < byr_broth + 40) | is.na(myr1_broth)) %>% 
#if their marriage year is less than their birth year + 40, keep
  filter(is.na(myr_2_son)&is.na(myr_3_son) & is.na(myr_2_broth) & is.na(myr_3_broth))#no 2nd or 3rd marriage


#is.na returns TRUE if the value is NA, otherwise it returns FALSE



#define differenced variables
delta_dage = regSample2$dage_son - regSample2$dage_broth
delta_dmarried = regSample2$dmarried_son - regSample2$dmarried_broth
delta_ded = regSample2$ded_son - regSample2$ded_broth
delta_Occrank = regSample2$Occrank_son - regSample2$Occrank_broth
delta_d21 = regSample2$d21_son - regSample2$d21_broth
same_regbirth = regSample2$regbirth_son == regSample2$regbirth_broth #categorical



#try some other death age cut offs.............

#?lm_robust()
#linear model 2
model2 = felm(delta_dage ~ delta_dmarried + delta_ded + 
              delta_Occrank + same_regbirth) #Am i picking up an upward trend in longevity that I'm
                                                          #not controlling for -> appropriate to use time fixed effects?



summary(model2)

stargazer(model2, type = "text", title = "Regression Table: Model 2")
stargazer(model2, type = "latex", title = "Regression Table: Model 2",
          out = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/model2out.tex")


#when filtering on d40_son and d40_broth == 1, but only looking at brothers
#who also married before age 40, the son who got married appears to live about 2.6
#years longer on average, after controlling for differences in educational achievement,occupational
#rank, and region of birth



#Reverse Causality: Are the people who live a long time more likely to get married? Or does 
#getting married cause the average individual to live longer than the average person who does 
#not get married?
#still uncertain if this is a causal relationship, but the finding is still interesting considering
#sample size and the controls



#=======================================================================================
#Comparing longevity between people who were married for (basically) their entire life
#versus people who married, but whose spouse died early in the marriage


#========================================================================================

regSample3 = family %>% filter(dmarried_broth == 1 & dmarried_son == 1)%>% 
  filter(d40_son == 1 & d40_broth == 1)#only want to consider when both brothers are married
#filter on d40 instead of d21 (only include brothers living to at least age 40)


#like in model 2, don't include people getting married after 40
regSample3 = regSample3 %>% 
  filter((myr1_son < byr_son + 40) | is.na(myr1_son)) %>% #only want to consider
  #people who either a) get married before 40, or b) don't get married at all
  filter((myr1_broth < byr_broth + 40) | is.na(myr1_broth)) %>% 
  mutate(marriage_length_son = spouse_dyr_son - myr1_son) %>% 
  mutate(marriage_length_broth = spouse_dyr_broth - myr1_broth) %>%  
  mutate(delta_marriage_length = marriage_length_son - marriage_length_broth) %>% 
  mutate(delta_dage = dage_son - dage_broth)
  
part1<-filter(regSample3, delta_marriage_length>=0)
part2<-filter(regSample3,delta_marriage_length<0)%>%
  mutate(delta_marriage_length = -1*delta_marriage_length)%>%
  mutate(delta_dage = -1*delta_dage)
regSample3<-bind_rows(part1,part2)

regSample3 = regSample3 %>% 
  mutate(delta_marriage_length0_14 = as.numeric(delta_marriage_length >=0 & delta_marriage_length < 15)) %>% 
  mutate(delta_marriage_length15_29 = as.numeric(delta_marriage_length >=15 & delta_marriage_length <30 )) %>%
  mutate(delta_marriage_length30_plus = as.numeric(delta_marriage_length >=30)) %>%
  filter(!is.na(delta_marriage_length)) %>% 
  filter(is.na(myr_2_son)&is.na(myr_3_son) & is.na(myr_2_broth) & is.na(myr_3_broth))#only married once


#create a difference in age at marriage variable
regSample3 = regSample3 %>% 
  mutate(spouse_age_son = (myr1_son - spouse_byr_son)) %>% 
  mutate(spouse_age_broth = (myr1_broth - spouse_byr_broth))

  
  

  

# delta_dage = regSample3$dage_son - regSample3$dage_broth
# delta_ded = regSample3$ded_son - regSample3$ded_broth
delta_Occrank = regSample3$Occrank_son - regSample3$Occrank_broth
delta_spouse_age = regSample3$spouse_age_son - regSample3$spouse_age_broth
# same_regbirth = regSample3$regbirth_son == regSample3$regbirth_broth
# 
# long_marriage_son = (regSample3$spouse_dyr_son - regSample3$myr1_son) > 10 #categorical variable- 
# #if the marriage lasts at least 10 years before the spouse dies, then  ->1 , else 0
# 
# #same thing but for the brother
# long_marriage_broth = (regSample3$spouse_dyr_broth - regSample3$myr1_broth) > 10





#if the difference is greater than 
#delta_long_marriage = long_marriage_son - long_marriage_broth #
#1 if son has a long marriage but broth has a short marriage
#0 if son and brother have "equal length" marriage
#-1 if son has a SHORT marriage and broth has a long marriage



#model3 = lm(delta_dage ~ delta_long_marriage + delta_ded + 
              #delta_Occrank + same_regbirth)

regSample3$byr_son

#new model 3
#Show this regression first
#Model 3 run a regression on delta_marriage_length
model3 =felm(delta_dage ~ 0 + delta_Occrank+ delta_marriage_length0_14+
                               delta_marriage_length15_29 +
                               delta_marriage_length30_plus, data = regSample3)

#save output
stargazer(model3,type = "text",title = "Model 3: Regression Table" )
stargazer(model3, type = "latex", title = "Model 3: Regression Table", 
          out = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/model3out.tex")



#should i do model 3, then this model3Robust, or treat model3Robust - delta_spouse_age as my model 3,
#then add in delta_spouse_age as the new control/robustness check???


#robustness check: adding in delta_spouse_age, how do results change?
model3Robust = felm(delta_dage ~ 0 + delta_Occrank+ delta_marriage_length0_14
            + delta_marriage_length15_29 
            + delta_marriage_length30_plus + delta_spouse_age, data = regSample3)

summary(model3Robust)

stargazer(model3,model3Robust, type = "text", title = "Robustness Check")


stargazer(model3,model3Robust, type = "latex", title = "Robustness Check", 
          out = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/robustness_table.tex")






model3Robust$coefficients[2:4]



#create visualization
marriage_length_df = data.frame(model3Robust$coefficients[2:4])#all coefficients except Occrank (just dummies)
summaryModel3Robust = summary(model3Robust)$coefficients[,c(2)] #all standard errors except Occrank again


summary(model3Robust)$coefficients

marriage_length_df = cbind(marriage_length_df,summaryModel3Robust[2:4])#don't want occrank


names(marriage_length_df) = c("coefficients", "standard_errors")
marriage_length_df = mutate(marriage_length_df, CI_Upper = 1.96*standard_errors + coefficients)
marriage_length_df = mutate(marriage_length_df, CI_Lower = coefficients - 1.96*standard_errors)
marriage_length_df$death_age_difference = c("0-14","15 - 29","30+")



death_age_plot = ggplot(data = marriage_length_df, mapping = aes(x = death_age_difference, y = coefficients))+
  geom_point()+
  geom_errorbar(mapping = aes(ymin = CI_Lower, ymax = CI_Upper))+
  xlab("Difference in Years of Marriage (Between Brothers)")+
  ylab("Estimated Differences in Longevity")+
  labs(title = "Differences in Longevity of Brothers", subtitle = "by difference in marriage length",
       caption = "95% confidence interval bars shown")+

  geom_hline(yintercept = 0, colour = "red")

death_age_plot

ggsave(filename = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/death_age_plot.png", death_age_plot)


#Interpretation:



summary(model3)
stargazer(model3, type = "text", title = "Regression Table: Model 3")
stargazer(model3, type = "latex", title = "Regression Table: Model 3",
          out = "/Users/collinkennedy/Google Drive/UC Davis/UC Davis/fall_quarter_2020/ECN198-Research/model3out.tex")


#stargazer table with all the results from each model

#stargazer(model1,model2,model3, type = "text",column.labels = c("Model 1:", "Model 2:", "Model 3:"))




#filtering on d21 and considering marriages at any point in time
#current output: if the first brother's wife lives longer than 10 years after marriage, whereas 
#the second brother's wife does NOT, then the life expectancy of the first brother is about 
#2 years longer than the second brother's life expectancy, on average, ceteris parabus.


#filtering on d40 and only considering marriages where the son and brother are 
#younger than 40 when they get married,
#there appears to be no statistically discernible difference in longevity
#between brothers who both marry before 40, who live to at least 40, but whose marriages are













