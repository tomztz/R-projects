
install.packages('Rlab')
library (Rlab)
#one sample hypothesis test function
#which takes in parameters the type of test, how many tails to consider
#alpha, the population mean mu,the sample size,sample mean and standard deviation
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))               #calculate the standard error
  test_stat <- (x_bar-mu)/(sd/sqrt(n))#calculate the test stat
  
  if (type=="z") {      #if it is a z test
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) #if two tails, get the p value by multiplying the absolute value of the test stat and use the norm function then multiply it by 2
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)#if left tail, get the p value by using the pnorm to the test_stat
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)#if right tail, get the p value by using the pnorm to the test_stat
    } else {stop("please choose tails as two, left, or right")}#else output an error
  }
  else if (type =="t") {#if t-test
    #define df
    df <- n-1  #df=population -1
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
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )#store outputs in a list ready to output
  #return the list
  return( ret )
}

x0<- rnorm(100,4,5)#create a vector size 100, mean4, sd5
N0 <- length(x0)#N0=length of vector
mean0 <- mean(x0)#mean0=mean of vector
sd0 <- sd(x0)#sd0=standard deviation of vector

 OneSampTest("z","two",0.05,0,N0,mean0,5)#call the function with z score,two tails,95% conf level,miu=0,length N0,mean0, and population sd 5
 OneSampTest("t","right",0.025,4.2,N0,mean0,sd0)#call the function with z score,right tail,95% sig,miu=0,length N0,mean0, and sd0
  
 t.test(x=x0,alternative = "less",mu=4.2,conf.level =0.95 )
 #call t.test function of the vector, less(right),mean 4.3,conf level 0.95
 x1 <- rbern(100, 0.3)#create a vector length 100, probability 0.3 store in x1
 sd1 <-sd(x1)#sd1=standard deviation of x1
 mean2 <- mean(x1)#mean2=mean of x1

 OneSampTest("z", "two", 0.05, 0.28 , 100, mean2, sd1)#call the function with z score,two tails,95% conf level, miu 0.28,length 100,mean2,sd1
 OneSampTest("z", "left", 0.05, 0.35 , 100, mean2, sd1)#call the function with z score,left tail,95% conf level, miu 0.35,length 100,mean2,sd1l
 