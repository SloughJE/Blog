setwd("~/Desktop/Blog/LogRank")

library(GGally)
library(survival)
library(ggplot2)

pbc=read.table("https://www.umass.edu/statdata/statdata/data/pbc.dat",na.strings = ".",
col.names=c("id","surv_time","death","penicillamine","age","sex","ascites","hepatomegaly","spiders","edema","bilirubin","cholesterol","albumin","urine_copper","alk_phosph","SGOT","Triglycerides","platelets","prothrombin_time","disease_stage"))

cols = c(3, 4, 6,7,8,9,10,20)
pbc[,cols] = apply(pbc[,cols], 2, function(x) as.factor(x))
# 
# pbc_comp = pbc[complete.cases(pbc), ]
# head(pbc_comp)
# str(pbc)

pbc_comp$SurvObj = with(pbc_comp, Surv(surv_time, death == 1))

pbc_surv = survfit(SurvObj ~ penicillamine, data = pbc_comp)
summary(pbc_surv)

p = ggsurv(pbc_surv,cens.col="black")+theme_bw()+xlab("Time in Days")+ylab("Proportion Surviving")+
  ggtitle("Survival Curves for D-penicillamine and Placebo")
p = p+  guides(linetype = FALSE) 
p = p+ scale_colour_manual(
  name   = 'treatment',
  breaks = c(1,2),values=c("blue","indianred"),
  labels = c('D-penicillamine', 'Placebo'))+theme(legend.key = element_blank())
p

ggsave("surv_plot.png",scale=1.2)


survdiff(Surv(surv_time, death==1) ~ penicillamine, data=pbc_comp)


# prob of living past 300 dayas
summary(pbc_surv,time=2000)$surv
#### another post? 
median(pbc_comp$age)
pbc.byage <- with(pbc_comp, survfit(SurvObj~age>63))

survdiff(pbc_comp$SurvObj~pbc_comp$age>63)
# See in output a p-value of 0.17
plot(pbc.byage, conf.int=TRUE, col=c("orangered", "blue"),lty=c(4,5), legend.text=c("Age <= 63", "Age > 63"))
