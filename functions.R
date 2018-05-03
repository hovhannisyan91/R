

library(tidyr)
library(BSDA)


df<-read.csv("C:\\Users\\Karen\\Desktop\\R_Project\\karen_hovhannisyan_test\\Lab1.csv")
#chi square test simple function
names(df)

head(df)
#issue:sample size issue. option 1: to take multiple random samples from the biggest sample

sample.creation <- function(df){
  df<-spread(df,LBTEST, VALUE)
  vars<-apply()
  sites<-unique(MyData2$SiteID)
  
    for (site in sites){
      s1<-c(MyData2$Value[MyData2$SiteID==site])
      s2<-c(MyData2$Value[MyData2$SiteID!=site])
      #group2<-sample(group2,length(group1))
      return(c(s1,s2)) 
      
      #append(vector,t.test(group1,group2)$p.value)
    }
}


chisq= function(s1,s2){
  s1= sample(z,nx,replace= FALSE)
  #issue:sample size issue. option 1: to take multiple random samples from the biggest sample
  p=chisq.test(s1,s2)$p.value
  return(p)
}

#z test simple function
ztest= function(s1,s2){
  sig1=sd(s1)
  sig2=sd(s2)
  p=z.test(s1,s2, sigma.x = sig1, sigma.y = sig2)$p.value
  return(p)
}
#Wilcoxon test simple function
wilcoxtest= function(s1,s2){
  p=wilcox.test(s1,s2)$p.value
  return(p)
}

# t test simple function
ttest= function(s1,s2){
  if(length(s1)==1){
    p1<-t.test(s2, mu=s1[1])$p.value
    return(p1)
  } else {
    p2<-t.test(s1,s2)$p.value  
    return(p2)
    }
  
  }

#single randomiaztion test
single.rand.test=function(x,y,test){
  nx=length(x)
  z=c(x,y)
  s1= sample(z,nx,replace= FALSE)
  s2=setdiff(z,s1) 
  p=test(s1,s2)
  return(p)
}
?z.test



#randomization test
random.test = function(x,y,test,n.sim){
  p.star=test(x,y)
  z=replicate(n.sim,single.rand.test(x,y,test))
  pvalue=sum(z<=p.star)/n.sim
  #return(pvalue)
}
#execution

exec.test <- function(x,y, test, test.fram, n.sim){
  if(test.fram=='standard'){
    pv=test(x,y)} else {pv=random.test(x,y,test, n.sim)}
    return(pv)
  
  ttest= function(s1,s2){
    p=t.test(s1,s2)$p.value
    return(p)
  }
}

exec.test(1:15,10:30,wilcoxtest,"standard",10)


#############################


  
sampling<- function(df){
    df<-spread(df,LBTEST, VALUE) 
    sites<-unique(df$SiteID)
    for (site in sites){
      s1<-c(df$Value[df$SiteID==site])
      s2<-c(df$Value[df$SiteID!=site])
    # group2<-sample(group2,length(group1))
      append(vector,c(s1,s2))
      return(vector)
  }}
  
sample.creation <- function(df){
  sapply(df[4:.],sampling(df))
}



exec.test <- function(df, test, test.fram, n.sim){
  x=sample.creation(df)[1]
  y=sample.creation(df)[2]
  if(test.fram=='standard'){
    pv=test(x,y)} else {pv=random.test(x,y,test, n.sim)}
  return(pv)
  }

######
siteobsnum <- table(df$SITEID)
exec.test <- function(df, test, test.fram, n.sim){
  x=sample.creation(df)[1]
  y=sample.creation(df)[2]
  
  test.fram<-if(length(x)==1){
    pv=test(x,y)} else {pv=random.test(x,y,test, n.sim)}
  return(pv)
}

