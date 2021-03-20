head(faithful)       #inspect the start of the dataset faithful

#Check the correlation between the variables eruptions and waiting

cor(faithful$eruptions, faithful$waiting)
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


faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # build linear regression model
faithful.res <- resid(faithful.lm)

plot(density(faithful.res), main="Density Plot: residuals", ylab="Frequency")  #inspecting the residuals density


par(mfrow=c(2,2)) #dividing the plot into 4 sections with 2 rows and 2 coloumns
plot(faithful.lm) #plotting the 4 plots

#Analyzing the density plots from the last week and this week, the model is apppropriate since the residual
#density plot forms a normal destribution,  this makes the model to be appropriate for two variables taken,
#and can predict with a relatively high confidence level