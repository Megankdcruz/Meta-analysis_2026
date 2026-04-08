#Run fixed and random effects meta-analysis 
#Present summay of effect sizes using forest plot 
#Need packages metafor 
install.packages("metafor", dependencies = TRUE,repos = "https://cloud.r-project.org")
#The funnel plot 
#First we need to simulate data 
#Generating data  with identical parameters and analysing with a linear model 
slope<--1
intercept<- 0
predictor<- rnorm(n=100, mean = 10, sd= 10)
response<- intercept+slope*predictor+rnorm(n=100,mean = 0, sd= 40)
plot(predictor,response)
#Run a simple lm with the data 
simplelm<- lm(response~predictor)
summary(simplelm)
#Now using the same simulation in a for loop 
store<- matrix(nrow = 200, ncol = 4)
#need to store our data somewhere 
for(x in 1:200){
  samplesize<- ceiling(exp(rnorm(1,4.5,1.5)))+3
  
  predictor<-rnorm(n=samplesize, mean=10, sd=10)
  response<- intercept+predictor*slope+rnorm(n=samplesize,0,40)
  model<- lm(response~predictor)
  store[x,]<-c(samplesize, summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4])
}
store<-as.data.frame(store)  
names(store)<- c("n","slope","standard.error","p.value")

#Producing and interpreting a funnel plot 

par(mfrow=c(1,2))
plot(store$slope,store$n,xlab = "Slope",ylab = "Sample size")
plot(store$slope,(1/store$standard.error),xlab = "Slope", ylab = "Precision, (1/se)")
#We can also colour the slope estimates that are significant and indicate the slope that we used with a dashed line 

sigsslope<- which(store$p.value<0.05)
par(mfrow=c(1,2))
plot(store$slope,store$n, xlab = "Slope", ylab = "Sample size")
points(store$slope[sigsslope],store$n[sigsslope],pch=16,col="red")
abline(v=slope,lty=2)
plot(store$slope,(1/store$standard.error),xlab = "Slope", ylab = "Precision,(1/se)")
points(store$slope[sigsslope],(1/store$standard.error[sigsslope]),pch=16,col="red")
abline(v=slope, lty=2)

#A basic meta-analysis 
#If we imagine that the 200 different slope estimates come from 200 different studies then the most straightforward way to analyse differences in effect sizes is to run a simple lm 

model2<- lm(slope~1, data=store)
#the ~1 tells the model to fit an intercept only
summary(model2)


#Estimating the mean effect size usiwng metafor 

library(metafor)
?rma
#this is a function to fit meta-analytic equal-, fixed-, and random-effects models and meta-regression models using a linear (mixed-effects) model framework
#two main bits of information to run this as yi = slope and sei = standard error 

meta<- rma(yi=slope, sei=standard.error,data = store)
meta

funnel(meta)
#now making a forest plot
forest(meta,cex.lab=0.8,addfit=TRUE,shade="zebra")

#A meta-analysis with moderators and random terms 
#Simulating a new meta-datasets 
#With these data sets slope estimates (effect sizes) vary as a function another variable 
Latitude<- runif(100,0,90)
#we will randomly sample a latitude from 0,90 degree North 
slope<- 0+Latitude*-0.1+rnorm(100,0,3)
plot(Latitude,slope)
#add random effect of species - so slopes 1-10 will be species 1 11-20 species 2 and so on untill 191-200 for species 20 
store2<- matrix(nrow = 200, ncol=7)
#somewhere to store our data 
species<-rep(1:20,each=10)
specieseffect<-rep(rnorm(20,0,2),each=10)
#we will use this to generate our 20 species random effects
for(x in 1:200){
  Latitude<- runif(1,0,90)
  slope<-0+specieseffect[x]+Latitude*-0.1+rnorm(1,0,3)
  samplesize<- ceiling(exp(rnorm(1,4.5,1.5)))
  if(samplesize>3){
    predictor<-rnorm(n=samplesize,mean = 10,sd=10)
    response<- intercept+predictor*slope+rnorm(n=samplesize,0,40)
    
    model<-lm(response~predictor)
    
    store2[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4],Latitude,species[x],x)
  }}

store2<-as.data.frame(store2)
names(store2)<-c("n","slope","standard.error","p.value","latitude","species","ID")

#now running a meta analysis and funnel plot with the new data set 

plot(store2$slope,(1/store2$standard.error),xlab="Slope",ylab="Precision,(1/se)")

meta2<- rma(yi=slope, sei = standard.error,data=store2)
meta2
