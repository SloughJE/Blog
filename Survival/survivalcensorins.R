# censoring

setwd("~/Desktop/Blog/Censoring")
library(GGally)
library(survival)
library(ggplot2)

anderson=read.table("http://web1.sph.emory.edu/dkleinb/allDatasets/surv2datasets/anderson.dat",sep=" ",
                    col.names=c("surv_time","relapse","sex","log_WBC","Rx"))


anderson$SurvObj = with(anderson, Surv(surv_time, relapse == 1))

and_surv = survfit(SurvObj ~ Rx, data = anderson)
summary(and_surv)

p = ggsurv(and_surv,cens.col="black")+theme_bw()+xlab("Time in Weeks")+ylab("Proportion Surviving")+
  ggtitle("Survival Curves for New Treatment and Standard Treatment")
p = p+  guides(linetype = FALSE) 
p = p+ scale_colour_manual(
  name   = 'treatment',
  breaks = c(0,1),values=c("blue","indianred"),
  labels = c('New', 'Standard'))+theme(legend.key = element_blank())
p

ggsave("surv_plot2.png",scale=1.2)

