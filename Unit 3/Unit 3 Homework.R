setwd("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Unit -3/Unit3/Unit3PreLive")

ACT<-read.csv("MathACT.csv")


#Attaching the data set, creating a function, and creating a summary stats table.  Note: In line 23 below, you can add other statistics like median, IQR,etc.

attach(ACT)
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x))
  names(result)<-c("N","Mean","SD","SE")
  return(result)
}
sumstats<-aggregate(Score~Background*Sex,data=ACT,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
sumstats

library(ggplot2)
ggplot(sumstats,aes(x=Background,y=Mean,group=Sex,colour=Sex))+
  ylab("ACT Score")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.1)


## Modify the previous R script so that the summary table also includeds the min, the max, and IQR.


## Question 1

mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x), max(x), IQR(x))
  names(result)<-c("N","Mean","SD","SE", "Min", "Max", "IQR")
  return(result)
}
sumstats<-aggregate(Score~Background*Sex,data=ACT,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
sumstats

## Question 2

ggplot(sumstats,aes(x=Background,y=Mean,group=Sex,colour=Sex))+
  ylab("ACT Score")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)

################################################################################
#################### Exercise # 3 ##############################################
################################################################################

## Question 1

library(Sleuth3)
head(ex1317)

irid<-ex1317

attach(irid)
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x),min(x), max(x), IQR(x))
  names(result)<-c("N","Mean","SD","SE", "Min", "Max", "IQR")
  return(result)
}
sumstats<-aggregate(Iridium~Strata*DepthCat,data=irid,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
sumstats

library(ggplot2)
ggplot(sumstats,aes(x=DepthCat,y=Mean,group=Strata,colour=Strata))+
  ylab("Iridium")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.1)


## Modify the previous R script to display mean plot with SD instead of SE


ggplot(sumstats,aes(x=DepthCat,y=Mean,group=Strata,colour=Strata))+
  ylab("Iridium")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=.1)

### Question 2 -> 2 way Anova model

library(gridExtra)
model.fit<-aov(Iridium~DepthCat+Strata+Strata:DepthCat,data=irid)
myfits<-data.frame(fitted.values=model.fit$fitted.values,residuals=model.fit$residuals)


#Residual vs Fitted
plot1<-ggplot(myfits,aes(x=fitted.values,y=residuals))+ylab("Residuals")+
  xlab("Predicted")+geom_point()

#QQ plot of residuals  #Note the diagonal abline is only good for qqplots of normal data.
plot2<-ggplot(myfits,aes(sample=residuals))+
  stat_qq()+geom_abline(intercept=mean(myfits$residuals), slope = sd(myfits$residuals))

#Histogram of residuals
plot3<-ggplot(myfits, aes(x = residuals)) + 
  geom_histogram(aes(y = ..density..),binwidth = 45,color = "black", fill = "gray") +
  geom_density(alpha = .5, fill = "red")

grid.arrange(plot1, plot2,plot3, ncol=3)

### Question 3 ###### Type III analysis

library(car)
anova(model.fit,type=3)

### Question 4####### multiple comparision

TukeyHSD(model.fit,"DepthCat:Strata",conf.level=.95)
plot(TukeyHSD(model.fit,"DepthCat:Strata",conf.level=.95))


####
#library(eemeans) #maybe need eemeans package
contrast.factor<-~Strata*DepthCat
mycontrast<-c("Shale:3-Limestone:1","Shale:3-Limestone:5","Shale:3-Limestone:6")
dat<-irid


#Running a loop that determines the appropriate 0's and 1's for each 
#contrast specified above.

library(limma)
final.result<-c()
for( j in 1:length(mycontrast)){
  contrast.factor.names<-gsub(" ", "", unlist(strsplit(as.character(contrast.factor),split = "*", fixed = TRUE))[-1])
  contrast.factor.2 <- vector("list", length(contrast.factor.names))
  for (i in 1:length(contrast.factor.names)) {
    contrast.factor.2[[i]] <- levels(dat[, contrast.factor.names[i]])
  }
  new.factor.levels <- do.call(paste, c(do.call(expand.grid, 
                                                contrast.factor.2), sep = ""))
  temp.cont<-mycontrast[j]
  contrast2 <- list(comparison = as.vector(do.call(makeContrasts, 
                                                   list(contrasts = temp.cont, levels = new.factor.levels))))
  
  contrast.result <- summary(contrast(lsmeans(model.fit, 
                                              contrast.factor), contrast2, by = NULL))
  
  final.result<-rbind(final.result,contrast.result)
}
#Cleaning up and applying bonferroni correction to the number
#of total comparisons investigated.
final.result$contrast<-mycontrast
final.result$bonf<-length(mycontrast)*final.result$p.value
final.result$bonf[final.result$bonf>1]<-1

final.result








