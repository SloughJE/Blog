setwd("~/Desktop/Statistics/Blog/ORvsRR")

ORvRR=function(inc_rate){
  i0 = seq(0,10, by = 0.001)
  I01 = inc_rate*1000
  b = rep(I01,length(i0))
  d = 1000-b
  a = b*i0
  a[a > 1000]= 1000 
  c = 1000-a
  OR = (a*d)/(b*c)
  RR = (a/(a+c))/(b/(b+d))
  
  ORRR=data.frame(OR,RR)
  return(ORRR)
}

c1=ORvRR(0.01)
c2=ORvRR(0.05)
c3=ORvRR(0.1)
c4=ORvRR(0.25)
c5=ORvRR(0.5)

df=data.frame(c1,c2,c3,c4,c5)
head(df)
str(df)
tail(df)
library(ggplot2)
library(ggthemes)

p = ggplot(df) + scale_x_continuous(breaks=0:10,limits=c(0,10),expand = c(0,0))+scale_y_continuous(breaks=0:10,limits=c(0,10),expand = c(0,0))
p = p + geom_line(aes(x=RR,y=OR),color="red4",size=1.5)
p = p + geom_line(aes(x=RR.1,y=OR.1),color="red3",size=1.5)
p = p + geom_line(aes(x=RR.2,y=OR.2),color="red2",size=1.5)
p = p + geom_line(aes(x=RR.3,y=OR.3),color="red1",size=1.5)
p = p + geom_line(aes(x=RR.4,y=OR.4),color="red",size=1.5)
p = p + theme_light() + ggtitle("Odds Ratios vs. Risk Ratios")
p = p + geom_segment(aes(x=0,y=0,xend=10,yend=10),linetype=2)
p = p + ylab("Odds Ratio") + xlab("Risk Ratio")
p = p + coord_equal(ratio=1)
p = p + geom_text(x = 1.23, y = 5.5, label = "IR = 0.5", size=3.5)
p = p + geom_text(x = 2.27, y = 6.5, label = "IR = 0.25", size=3.5)
p = p + geom_text(x = 4.1, y = 7.5, label = "IR = 0.1", size=3.5)
p = p + geom_text(x = 5.65, y = 8.5, label = "IR = 0.05", size=3.5)
p = p + geom_text(x = 8.23, y = 9.5, label = "IR = 0.01", size=3.5)
p = p + geom_text(x = 6.1, y = 5.8, angle = 45, label = "Odds Ratio = Risk Ratio", size=4)
p

ggsave("ORvRR.png",scale=1.2)


calcOR = c(1,3,9)
calcRR = c(1,2,3)
dfcalc=data.frame(calcRR,calcOR)
df25

p25 = ggplot(df) + scale_x_continuous(breaks=0:10,limits=c(0,10),expand = c(0,0))+scale_y_continuous(breaks=0:10,limits=c(0,10),expand = c(0,0))
p25 = p25 + geom_line(aes(x=RR.3,y=OR.3),color="red1",size=1.5)
p25 = p25 + geom_point(data=dfcalc,aes(x=calcRR,y=calcOR),size=4)
p25 = p25 + theme_light() + ggtitle("Odds Ratios vs. Risk Ratios")
p25 = p25 + ylab("Odds Ratio") + xlab("Risk Ratio")
p25 = p25 + coord_equal(ratio=1)
p25 = p25 + geom_text(x = 2.27, y = 6.5, label = "IR = 0.25", size=3.5)
p25

ggsave("ORvRR25.png",scale=1.2)
