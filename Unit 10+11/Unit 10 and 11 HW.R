library(epitools)
#Another way to format a count matrix
mymat1<-matrix(c(330,658,204,386),2,2,byrow=T)
dimnames(mymat1)<-list("Treatment"=c("Less than 4","4 or More"),"Response"=c("Cancer","Control"))
mymat1
#Odds Ratio Intervals
oddsratio.wald(mymat1, rev = "rows")