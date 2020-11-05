# introductory-exercises.R
# Matt Curtis mjdcurtis@gmail.com
# 10/15/20
#=============================================================================#
# 0. set up
#-----------------------------------------------------------------------------#
library(haven)
library(dplyr)

foe = foe_copy

# unfortunately there are a small number of duplicate pids
# this can mess up merges
# so I drop them

# tag duplicates
foe<-foe %>% 
  mutate(count=1) %>% 
  group_by(pid) %>% 
  mutate(duplicates = sum(count))

table(foe$duplicates) %>% print

# drop duplicates
foe<-filter(foe,duplicates==1)

# make sure no missing pids
stopifnot(nrow(filter(foe,is.na(pid)))==0)

#=============================================================================#
# 1. longevity
#-----------------------------------------------------------------------------#

#1
filter(foe,dfem==1 & regbirth < 10 & byr>=1860&byr<1870)$dage %>%
  mean(na.rm=TRUE)%>% round(2) %>% print
#55yearsold

#-----------------------------------------------------------------------------#

#2 
filter(foe,dfem==1 & regbirth < 10 & byr>=1860&byr<1870 &dmarried ==1 & dage>=21 )$dage %>%
  mean(na.rm=TRUE)%>% round(2) %>% print

#=============================================================================#
# 2. Intergenerational mobility
#-----------------------------------------------------------------------------#

# 1 grandfather - grandson

sons<-filter(foe,dfem==0) %>% filter(!is.na(pid_fath))

fathers<-filter(foe,dfem==0) %>%  select(pid_fath = pid, pid_gfath = pid_fath) %>%
 filter(!is.na(pid_gfath))

grandfathers<-filter(foe,dfem==0) %>% select(pid_gfath = pid,Occrank_gfath = Occrank)

joined<-inner_join(sons,fathers) %>% inner_join(grandfathers) %>% 
  filter(!is.na(Occrank)&!is.na(Occrank_gfath))
View(joined)

cor(joined$Occrank,joined$Occrank_gfath) %>% round(2) %>%  print()
#.6

#-----------------------------------------------------------------------------#
# 2

sons<-filter(foe,dfem==0)  %>%  filter(!is.na(pid_fath))
bros<-select(sons,pid_fath, pid_bro = pid, Occrank_bro = Occrank)



# if you share a father but have the same pid, it is yourself not a brother
# this shouldn't change the correlation, but technically for brothers we get two
# copies of each pair; this should matter for standard errors
# matt curtis and tom curtis
# tom curtis and matt curtis

joined<-inner_join(sons,bros) %>% 
  filter(pid<pid_bro) %>% 
  filter(!is.na(Occrank)) %>%
  filter(!is.na(Occrank_bro)) 

cor(joined$Occrank,joined$Occrank_bro) %>% round(2) %>%  print()

#-----------------------------------------------------------------------------#

#3

foe<-mutate(foe,logprice = vzpcln2017 )

sons<-filter(foe,dfem==0)  %>% filter(!is.na(pid_fath))

fathers<- filter(foe,dfem==0) %>%   select(pid_fath = pid, pid_gfath = pid_fath) %>% 
  filter(!is.na(pid_gfath))



sons<-inner_join(sons,fathers)

cousins<-sons %>% select(pid_cuz = pid, pid_cuz_fath = pid_fath, pid_gfath, logprice_cuz = logprice)

# if you share a father you are actually brothers
joined<-inner_join(sons,cousins) %>% 
  filter(pid_cuz_fath != pid_fath) %>% 
  filter(pid<pid_cuz) %>% filter(!is.na(logprice)) %>% filter(!is.na(logprice_cuz))

cor(joined$logprice,joined$logprice_cuz) %>% round(2) %>%  print()


#=============================================================================#
# 3. Migration
#-----------------------------------------------------------------------------#
kids<- foe %>%  filter(!is.na(pid_fath))

fathers<- filter(foe,dfem==0) %>%   select(pid_fath = pid, Occrank_fath = Occrank)

join<-inner_join(kids,fathers)

# a.

(join %>% filter(regbirth == 1) %>% 
  filter(regdeath==0 | regdeath == 4))$Occrank_fath %>% 
  mean(na.rm=TRUE) %>% round(2) %>% print()
  



# b.

(join %>% filter(regbirth == 0) %>% 
    filter(regdeath==1))$Occrank_fath %>% 
  mean(na.rm=TRUE) %>% round(2) %>% print()


# c.
(join %>% filter(regbirth <10 & regbirth != 2) %>% 
    filter(regdeath==14))$Occrank_fath %>% 
  mean(na.rm=TRUE) %>% round(2) %>% print()
