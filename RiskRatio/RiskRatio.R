setwd("~/Desktop/Blog/Relative Risk")

# Forest Plot
library(ggplot2)
library(ggthemes)

x = c("Type 2 diabetes","Ischemic stroke","CHD total","CVD mortality","CHD mortality","All cause mortality")
y = c(0.95, 1.02, 1.06, 0.97, 1.15, 0.99)
ylo = y-c(0.07, 0.12, 0.11, 0.13, 0.18, 0.08)
yhi=y+c(0.08, 0.13, 0.11, 0.15, 0.21, 0.10)

sat_fat=data.frame(x,y,ylo,yhi)

#Turn your 'treatment' column into a character vector
sat_fat$x <- as.character(sat_fat$x)
#Then turn it back into an ordered factor
sat_fat$x <- factor(sat_fat$x, levels=unique(sat_fat$x))

p=ggplot(sat_fat, aes(x=x, y=y, ymin=ylo, ymax=yhi))+
  geom_pointrange(color="blue",shape=15) + 
  coord_flip()+geom_hline(aes(yintercept = 1),color="blue",linetype=2)+
  ggtitle("Relative Risk Ratios for Saturated Fat Intake")+
  xlab("Outcome")+ylim(0,2)+theme_minimal()+
  ylab("Saturated fats protective                                                  Risk Ratio (95% CI)                                                  Saturated fats harmful")
p

ggsave("RR.png",scale=1.2)

####

library(forestplot)
y = c(0.99,1.15,0.97,1.06,1.02,0.95)
ylo = y-c(0.08,0.18,0.13,0.11,0.12,0.07)
yhi = y+c(0.1,0.21,0.15,0.11,0.13,0.08)


mean=c(NA,NA,y)
lower=c(NA,NA,ylo)
upper=c(NA,NA,yhi)

values=data.frame(mean,lower,upper)


texttable<-cbind(
  c("Outcome","All Cause Mortality","CHD mortality","CVD Mortality","CHD Total","Ischemic Stroke","Type 2 Diabetes"),
  c("Relative Risk", "(95% CI)", "0.99 (0.91 to 1.09)","1.15 (0.97 to 1.36","0.97 (0.84 to 1.12)","1.06 (0.95 to 1.17)","1.02 (0.90 to 1.15)","0.95 (0.88 to 1.03)"),
  c("P","","0.91","0.10","0.69","0.29","0.79","0.20"))
  
fp=forestplot(texttable, graph.pos = 2,
           values,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,6)),
           zero=1,
           xlab = "Risk Ratio\n(95% CI)",
           xticks=c(0,0.5,1,2),
           xlog=FALSE,
           col=fpColors(box="royalblue",line="royalblue"))

c("Relative Risk", "(95% CI)", "0.99 (0.91 to 1.09","1.15 (0.97 to 1.36","0.97 (0.84 to 1.12)","1.06 (0.95 to 1.17)","1.02 (0.90 to 1.15)","0.95 (0.88 to 1.03)"),
c("P","","0.91","0.10","0.69","0.29","0.79","0.20"),
c('P',"", "0.17","<0.001","0.29","0.02","0.002","0.61"),
c('I',"(%)","33","70","19","47","59","0")
