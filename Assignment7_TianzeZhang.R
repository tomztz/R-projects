head(faithful)       #inspect the start of the dataset faithful

#Check the correlation between the variables eruptions and waiting
 
r<-cor(faithful$eruptions, faithful$waiting)
#Create a plot that shows both the points and a smoothed line of the points 
plot(faithful$eruptions, faithful$waiting)# scatterplot
scatter.smooth(x=faithful$eruptions, y=faithful$waiting, main="Eruptions ~ Waiting")  # smoothed line of the points 

#Create box plots for the variables

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))
# box plot for 'eruptions'
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))
# box plot for 'waiting'

#Create graphs of the densities of the variables
par(mfrow=c(1, 2))# divide graph area in 2 columns
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")
# density plot for 'eruptions'
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency")
# density plot for 'waiting'

#Fit a simple linear model that predicts eruptions from waiting
faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # build linear regression model 
print(faithful.lm)
#Visualise the resulting regression line on a scatterplot of the data

plot(faithful$eruptions, faithful$waiting)
abline(faithful.lm)

#In comments in your code, write an equation that describes the linear model you have fitted
sd1<-sd(faithful$eruptions)#get the standard deviation of faithful$eruptions,i.e.Sy
sd2<-sd(faithful$waiting)#get the standard deviation of faithful$waiting,i.e.Sx

B1<-r*(sd2/sd1)#From B1hat=r*Sy/Sx
print(B1)#get the Carats coef

m1<-mean(faithful$eruptions)#get the mean of faithful$eruptions,i.e.x bar
m2<-mean(faithful$waiting)#get the mean of faithful$waiting,i.e.y bar
B0<-m2-B1*m1#From B0 hat =y bar-B1 hat*x bar
print(B0)#we get the constant coefficient

#after running,B1=10.72964,B0=33.4744
#from the sample regression line formula where y hat is wstimate,
#yhat = B0hat + B1hat(x) 
#yjat=33.4744+10.72964x


#At a significance level of 0.05, does there appear to be a statistically significant relationship 
#between eruptions and waiting? Explain your answer in comments in your code. 

summary(faithful.lm)

#there is a significant relationship between eruptions and waiting.
#let H0= Population slope = 0;there is no relationship between eruptions and waiting.
#H1=Population slope != 0;there is a relationship between eruptions and waiting.
#Based on the P-Value in summary,
# 2.2e-16<0.05 which is significant, and indicates that we have sufficient results to reject the 
#H0(null hypothesis) then we could conclude H1 is true, which shows that there is a relationship

#Why are the p-values for the variable waiting and the overall F test so similar for this model?

#f-test tests null hypothesis is that a model with an intercept only fits the data as well as the model considered.
#it depends on the numbers of variables,
#because we only have one other variable (waiting), the f-test and waiting p-values are testing the
# same thing,therefore the p-value of the f-test and of the waiting variable at relatively similar for this
# specific model.    




