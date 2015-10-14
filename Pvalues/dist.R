setwd("~/Desktop/Statistics/Blog/P-values")

cord.x <- c(-3,seq(-3,-1.96,0.01),-1.96) 
cord.y <- c(0,dnorm(seq(-3,-1.96,0.01)),0) 

cord.x2 <- c(3,seq(3,1.96,-0.01),1.96) 
cord.y2 <- c(0,dnorm(seq(-3,-1.96,0.01)),0) 

png("pvalue.png", width=10, height=6, units="in", res=600)
curve(dnorm(x,0,1),xlim=c(-3,3),main = expression(paste("H"[0],": ",mu[0]-mu[1] , " = 0 ")),
      xlab="Z-score",ylab="",yaxt='n',bty="n",lwd=2)

polygon(cord.x,cord.y,col='indianred',border="indianred")
polygon(cord.x2,cord.y2,col='indianred',border="indianred")
segments(x0=-1.96,y0= 0,x1=-1.96, y1=.25,lty=2,col="indianred")
segments(x0=1.96,y0= 0,x1=1.96, y1=.25,lty=2,col="indianred")
# abline(v=1.96,lty=2,col="indianred")
# abline(v=-1.96,lty=2,col="indianred")
text(-2.5,.06,"area = 0.025",cex=.7,col="indianred")
text(2.5,.06,"area = 0.025",cex=.7,col="indianred")
text(0,.06,"area = 0.95",cex=.7)
text(0,.23,"fail to reject")
text(2.5,.23,"reject",col="indianred")
text(-2.5,.23,"reject",col="indianred")

dev.off()
