library(haven)
library(dplyr)
library(ggplot2)
foe <- read_dta("Dropbox/Ecn 198 2020 Fall/FOE Database/foe.dta")


sample<-filter(foe,byr>=1901&byr<=1917) %>% #start subsetting, keep if birthyear >=1901
                                              #but less than or equal to 1917
  mutate(before = byr<=1907) %>% #mutate adds new (categorical) variables to the dataframe
  mutate(after = byr>=1911) %>% #notice we create three new dummy variables 
  mutate(t = byr-1910) 

lm(data=sample, dage~after*t) %>% summary %>% print

plotdata<-group_by(sample,byr,after,before) %>% 
  summarize(dage=mean(dage,na.rm=TRUE)) %>% 
  group_by() %>% 
  mutate(age12 = byr+12)

plot<-ggplot( data=plotdata,aes(x=age12,y=dage)) + #creates the ggplot object
  geom_point(color="steelblue4",size=2)+ #adds scatterplot basically (points)
  geom_smooth(data=filter(plotdata,before==1),color = "red",method="lm",se=FALSE)+
  geom_smooth(data=filter(plotdata,after==1),color = "red",method="lm",se=FALSE)+
  geom_vline(xintercept=1919,linetype="dashed")+
  geom_vline(xintercept=1923,linetype="dashed")+
  ylim(55,75)+
  xlim(1912,1931)+
  xlab("Year turned 12")+
  ylab("Average age at death")+
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) 
print(plot)

ggsave("~/Desktop/graph2.png",width=6,height=4,units='in')  #saves the plot output

