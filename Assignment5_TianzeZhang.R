install.packages('Rlab')
library (Rlab)

#two sample hypothesis test function
#which takes in parameters the type of test, how many tails to consider,proportion or mean
#alpha, the population difference,the 1st and 2nd sample size,sample mean and standard deviation
TwoSampTest <-function(type=NULL, tails=NULL, propmean=NULL,alpha, Pop_D, n1,n2, x_bar1,x_bar2, sd1,sd2)
{
  #calculate the test statistic
  if(propmean=="mean"){               #if dealing with mean
    
    pooledsd<-sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))#calculate the pooled sd
  se=pooledsd*sqrt((1/n1)+(1/n2))                            #calculate the standard error when variances are the same
  }
  else if(propmean=="proportion"){                #if dealing with proportion
    pooledprop=(x_bar1*n1+x_bar2*n2)/(n1+n2)      #calculate the pooled proportions from the sample
    se=sqrt(pooledprop*(1-pooledprop)*(1/n1+1/n2))#calculate the standard error of proportion
  } else {stop("please choose mean or proportion")}#else output error
  
  test_stat<-(x_bar1-x_bar2-Pop_D)/se            #calculate the test stat
  if (type=="z") {      #if it is a z test
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) #if two tails, get the p value by multiplying the absolute value of the test stat and use the norm function then multiply it by 2
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)#if left tail, get the p value by using the pnorm to the test_stat
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)#if right tail, get the p value by using the pnorm to the test_stat
    } else {stop("please choose tails as two, left, or right")}#else output an error
  }
  else if (type =="t") {#if t-test
    #define df
    df <- n1+n2-2    #df=n1+n2 -2
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) #if two tails, calculate the p value
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)# if left tails, calculate the p value for it
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)# if right tails, calculate the p value for it
    } else {stop("please choose tails as two, left, or right")}#else output error
  }
  else {stop("please choose z or t")}#output error
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"     #if p_val< alpha sig ="significant"
  }  else {sig <-"not significant"}     # else sig= not "significant"
  
  
  ret <- list(type=paste("Two Sample", type, "test.", tails, "tailed"), n1=n1, n2=n2, Pop_D=Pop_D, diff=x_bar1-x_bar2, se_est=se, test_stat=test_stat, p = p_val, alpha = alpha, significance = sig ) #store outputs in a list ready to output
  #return the list
  return( ret )
}


x0<- rnorm(100,4,5)     #create a vector size 100, mean4, sd5
x1<- rnorm(80,3.5,2)  #create a vector size 80, mean3.5, sd2

sd1<- sd(x0)          #sd1=standard deviation of vector x0
m1<-mean(x0)          #m1=mean of vector x0
n1<-length(x0)           #n1=length of vector x0

sd2<- sd(x1)            #sd2=standard deviation of vector x1
m2<- mean(x1)           #m2=standard deviation of vector x1
n2<-length(x1)          #n2=standard deviation of vector x1

#1)	H0: the difference in means of the populations are equal to 1. 
#HA: the difference in means of the populations are not equal to 1. 
#Use a Z-test with the true population SDs. Use alpha = 0.05

TwoSampTest("z", "two", "mean", 0.05, 1,n1, n2, m1, m2, 5 ,2)

#2)	H0: the means of the populations are equal.
#HA: the means of the populations are not equal. 
#Use a Z-test with the sample SDs. Use alpha = 0.05

TwoSampTest("z", "two", "mean", 0.05, 0,n1, n2, m1, m2, sd1,sd2)

#3)	H0: the means of the populations are equal. 
#HA: the means of the populations are not equal. 
#Use a t-test. Use alpha = 0.05.

TwoSampTest("t", "two", "mean", 0.05, 0,n1, n2, m1, m2, sd1,sd2)
#4)	Repeat the previous task using the t.test function in the package {stats}

t.test(x=x0,y=x1,alternative ="two.sided",mu=0,paired = FALSE, var.equal = TRUE,conf.level =0.95)

#5)	Repeat the previous task using the t.test function in the package {stats}, without the assumption of equal variance.

t.test(x=x0,y=x1,alternative ="two.sided",mu=0,paired = FALSE, var.equal = TRUE,conf.level =0.95)

#6)	H0: the population proportions are equal. HA: the population proportions are not equal 

x2<- rbern(100, 0.3)         #create a vector length 100, probability 0.3 store in x2
x3<- rbern(85, 0.7)          #create a vector length 85, probability 0.7 store in x3

sd2<- sd(x2)                #sd2=standard deviation of vector x2
m2<-mean(x2)               #m2=mean of vector x2
n2<-length(x2)            #n2=length of vector x2

sd3<- sd(x3)             #sd3=standard deviation of vector x3
m3<- mean(x3)            #m3=mean of vector x3
n3<-length(x3)           #n3=length of vector x3

TwoSampTest("z", "two", "proportion", 0.05,0, n2, n3, m2, m3, sd2, sd3)

#7)	write code to calculate the 95% Confidence Interval for your estimate of the difference between the two population proportions.


se = sqrt(((m2*(1-m2)) / n2)+( (m3*(1-m3)) /  n3))#calculate the standard error
right <- (m2 - m3) + se*(m2 - m3)                #calculate the CI of the right side
left <- (m2 - m3) - se*(m2 - m3)                #calculate the CI of the left side
paste("95 percent confidence interval with difference 0 :")
paste(right)
paste(left)