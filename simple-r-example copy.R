# haven loads stata files
library(haven)

# dplyr is a huge library that makes data cleaning much easier
# it is a subset of the wonderful tidyverse library which I wholeheartedly recommend
library(dplyr)

# stargazer is a library that makes pretty tables
library(stargazer)



foe = foe_copy
View(foe)



# %>%  is from dplyr (and it's subset magrittr) and is called a FORWARDS PIPE
# is is a way to "unpack" nested functions
# x %>% f() is the same as f(x)
# x %>%  g() %>% f() is the same as g(f(x))
# x %>%  f(y) is the same as f(x,y)

# create a tibble (dpylr data frame) that is just Occrank, pid, and pid_fath for 
# just men 


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



View(fathers)

#now create a table for the fathers of fathers (grandfathers)
grandFathers = foe %>% 
  filter(!is.na(pid_fath)) %>% #only want people with fathers 
  filter(dfem==0) %>%  #only males
                      #this extracts the fathers from fathers df
  select(Occrank_gfath = Occrank, pid_gfath = pid) 

#join sons and fathers
joined<-inner_join(sons,fathers)
View(joined)                         

#join joined df with grandfathers
joinedWithGFathers = inner_join(joined, grandFathers)
View(joinedWithGFathers)
          
          
#check correlation between occupational status between grandfathers and grandsons
cor.test(joinedWithGFathers$Occrank, joinedWithGFathers$Occrank_gfath)



  
  
# 
# 
left<-as.data.frame(c(1,2,3))
names(left)<-c("value")
left

# 
right<-as.data.frame(c(2,3,4))
names(right)<-c("value")
right

right$surname<-c("aa","bb","cc")
# 
right

join<-right_join(left,right,by=c("value"))
join




# now join
# inner_join(a,b,by=c("var")) is like merge x:y var using b in Stata 
# (where a is loaded in the memory)
# inner is only matches (in Stata, _merge == 3)
# left join is only matches and unmatched master obs (in Stata, _merge == 1 | 3)
# right join is only matches and unmatched using obs (in Stata, _merge = 2 | 3)


joined<-inner_join(sons,fathers)
View(joined)



# cor.test is a nice way to get standard errors and the correlation
cor.test(joined$Occrank,joined$Occrank_fath)


lm(data = joined, Occrank ~ Occrank_fath) %>% stargazer(type='text')



# mutate at is a dplyr command that applies a function to a list of vars
# ~ indicates an equation in R
# divide both by their standard errors

joined<-joined %>% 
  filter_at(vars(Occrank_fath,Occrank),
            ~!is.na(.)) %>% 
  mutate_at(vars(Occrank_fath, Occrank),
                                       ~(./sd(.)))
# na.rm is necessary to drop missing variables; sd(c(NA,1,2)) = NA
#stopifnot is the R version of assert
stopifnot( abs(sd(joined$Occrank,na.rm=TRUE) - sd(joined$Occrank_fath)) < 0.0000001)

cor.test(joined$Occrank,joined$Occrank_fath) %>% print

reg1<-lm(data=joined,Occrank~Occrank_fath)

#summary(reg1) %>% print
stargazer(reg1,type='text') 