#linear regression used to create numeric and absolute output

library(psych) #package which plots plot between 2 variables & indep & dependet also
A=read.csv(file.choose())
A=na.omit(A) #omit null values
sf= sample(2,nrow(A),replace=TRUE,prob = c(0.8,0.2)) #sample dataset
trd=A[sf==1,] #training data
tsd=A[sf==2,] #testing data
#plot(A$profit)
cor(A$PROFIT,A$MKT,A$ADMIN,A$RND) #find out corelation between inputs-error
cor(A[,c(1,2,3,5)])
pairs.panels(A$PROFIT,A$MKT,A$ADMIN,A$RND)#error
pairs.panels(A) #plot between variables & provides corelation between varibale also
 #create model(RND & Profit has highest/strong corelation =0.97)
model1=lm(PROFIT ~ RND, data = trd) #model to create using training data set trd
predop= predict(model1,tsd)#predict is use to predict output using tsd(testing data set)
cbind(predop,tsd$PROFIT) #bind/add column Predicted output profit

#alternative for pairs.panel (for more no of varibles)- find another model