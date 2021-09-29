##################### 26 #######################
coverage<-function(b,se,true,level=.95,df=Inf){ 
  qtile<-level+(1-level)/2 
  lower.bound<-b-qt(qtile,df=df)*se 
  upper.bound<-b+qt(qtile,df=df)*se 

  true.in.ci<-ifelse(true>=lower.bound&true<=upper.bound,1,0)
  cp<-mean(true.in.ci) 
  mc.lower.bound<-cp-1.96*sqrt((cp*(1-cp))/length(b)) 
  mc.upper.bound<-cp+1.96*sqrt((cp*(1-cp))/length(b))
  return(list(coverage.probability=cp, 
              true.in.ci=true.in.ci,
              ci=cbind(lower.bound,upper.bound),
              mc.eb=c(mc.lower.bound,mc.upper.bound)))
}
###########
reps<-100 
results<-matrix(NA,nrow=reps,ncol=6) 
colnames(results)<-c("Mean_est", "Bias", "SE","Lower.bound", "upper.bound","Coverage")
n<-1000
set.seed(63654)
for(i in 1:reps){ 
  Y<-rnorm(n,0,1)
  mean.est<-mean(Y)
  results[i,1]<-mean.est
  results[i,2]<-mean.est
  se<-1/(sqrt(n))
  results[i,3]<-se
  lower<-mean.est-(qt(.975,n)*se)
  results[i,4]<-lower
  upper<-mean.est+(qt(.975,n)*se)
  results[i,5]<-upper
  true.in.ci<-ifelse(0>=lower&0<=upper,1,0)
  results[i,6]<-true.in.ci
}

results<-as.data.frame(results)
table(results$Coverage)

no_true<-results[which(results$Coverage==0),]
table(no_true)

library(ggplot2)
limits <- aes(ymax = upper.bound, ymin = Lower.bound)
ggplot(results, aes(y=mean.est, x=1:100, colour = Coverage)) + 
  geom_hline(aes(yintercept = 0), color = "blue", size = 3) + 
  geom_pointrange(limits) + 
  xlab("Simulations") +
  ylab("Confidence Intervals")
