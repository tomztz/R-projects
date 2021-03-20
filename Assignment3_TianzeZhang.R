x0<-rnorm(10000,4,5)#create a vector x0 of length 1000, 
#drawn from a normal distribution with mean 4, sd 5
N0 <- length(x0)    #N0= the size of x0
mean0 <- mean(x0)   #mean0=mean of x0
sd0 <- sd(x0)       #sd0=standard deviation of x0

x1<-rexp(10000,1)   # create a exponential of sample size 10000 at a rate 1;
N2 <- length(x1)    #N2=length of x1
mean2 <- mean()     #mean2=mean of x1
sd2 <- sd(x1)       #sd2= standard deviation of x1

x2 <- sample(x0,10) #x2=randomally take 10 samples from x0
x3<- sample(x0,50)  #x3=50 samples randomally taken from x0

n1 <- length(x3)    #n1=length of x3
mean1 <- mean(x3)   #mean1=mean of x3
sd1 <- sd(x3)       #sd1=standard deviation of x3

x4<- sample(x0,500)#x3=500 samples randomally taken from x0

x5<-sample(x1,10)#x5=10 samples randomally taken from x1
x6<-sample(x1,50)#x6= 50 samples randomally taken from x1

n3 <- length(x6)#n3=length of x6
mean3 <- mean(x6)#mean3 = mean of x6
sd3 <- sd(x6)#sd3= standard deviation of x6

x7<-sample(x1,500)#randomally take 500 samples from x1


#graph with 10 samples for a Normal distribution:
qqnorm(x2)    #draw qqplot for x2
qqline(x2)    #add a line which passes through the first and thrid quartiles
hist(x2, freq = FALSE)#draw a histogram for x2,freq=false
xfit <- seq(min(x2), max(x2), length = 40) #generate 40 sequences of points between the 
#maxmium and minimum value in x2
yfit <- dnorm(xfit, mean = mean(x2), sd = sd(x2))#find the distribution of the line of best fit
# with the xfit, mean and standard deveation of the 10 samples
lines(xfit, yfit)#draw the line of best fit
#graph with 50 samples for a Normal distribution:
qqnorm(x3)
qqline(x3)
hist(x3, freq = FALSE)
xfit <- seq(min(x3), max(x3), length = 40) #explaination same as 10 samples for Normal distribution
yfit <- dnorm(xfit, mean = mean(x3), sd = sd(x3))
lines(xfit, yfit)
#graph with 500 samples for a Normal distribution:
qqnorm(x4)
qqline(x4)
hist(x4, freq = FALSE)
xfit <- seq(min(x4), max(x4), length = 40) 
yfit <- dnorm(xfit, mean = mean(x4), sd = sd(x4)) #explaination same as 10 samples for Normal distribution
lines(xfit, yfit)

#graph with 10 samples for a Exponential distribution:
qqnorm(x5)
qqline(x5)
hist(x5, freq = FALSE)
xfit <- seq(min(x5), max(x5), length = 40) 
yfit <- dnorm(xfit, mean = mean(x5), sd = sd(x5))#explaination same as 10 samples for Normal distribution
lines(xfit, yfit)
lines(xfit, yfit)
#graph with 50 samples for a Exponential distribution:
qqnorm(x6)
qqline(x6)
hist(x6, freq = FALSE)
xfit <- seq(min(x6), max(x6), length = 40) 
yfit <- dnorm(xfit, mean = mean(x6), sd = sd(x6))#explaination same as 10 samples for Normal distribution
lines(xfit, yfit)
#graph with 500 samples for a Exponential distribution:
qqnorm(x7)
qqline(x7)
hist(x7, freq = FALSE)
xfit <- seq(min(x7), max(x7), length = 40) 
yfit <- dnorm(xfit, mean = mean(x7), sd = sd(x7))#explaination same as 10 samples for Normal distribution
lines(xfit, yfit)


#Z distribution:

se_kn <- sd0/sqrt(n1)#se_kn =standard deviation of x0/square root the length of x3
se_kn2<- sd2/sqrt(n3)#se_kn2=standard deviation of x2/square root the length of x6
se_unkn <- sd1/sqrt(n1)#se_unkn=standard deviation of x3/square root the length of x3
se_unkn2<- sd3/sqrt(n3)#se_unkn2=standard deviation of x6/square root the length of x6

Z_score <- qnorm(0.975)                     #Calculate Z-score for 95% CI
#z score for normal distribution w/known pop sd
left_Z95_kn <- mean1-Z_score*se_kn          #left_Z95_kn=mean of 50 sample sizes-Z_score*se_kn
right_z95_kn <- mean1+Z_score*se_kn         #right_z95_kn=mean of 50 sample sizes+Z_score*se_kn
left_Z95_unkn <- mean1-Z_score*se_unkn      #left_Z95_unkn=mean of 50 sample sizes-Z_score*se_unkn
right_z95_unkn <- mean1+Z_score*se_unkn     #right_z95_unkn=mean of 50 sample sizes+Z_score*se_unkn
paste("Z-distribution for normal distribution w/known pop sd:", left_Z95_kn, right_z95_kn)#print result
paste("Z-distribution for normal distribution w/unknown pop sd:", left_Z95_unkn, right_z95_unkn)#print result
#z score for exponential distribution w/known pop sd
left_Z95_kn2 <- mean3-Z_score*se_kn2       #left_Z95_kn2=mean of 50 sample sizes-Z_score*se_kn2
right_z95_kn2 <- mean3+Z_score*se_kn2      #right_z95_kn2=mean of 50 sample sizes+Z_score*se_kn2
left_Z95_unkn2 <- mean3-Z_score*se_unkn2   #left_Z95_unkn2=mean of 50 sample sizes-Z_score*se_unkn2  
right_z95_unkn2 <- mean3+Z_score*se_unkn2  #right_z95_unkn2=mean of 50 sample sizes+Z_score*se_unkn2
paste("Z-distribution for exponential distribution w/known pop sd:", left_Z95_kn2, right_z95_kn2)#print result
paste("Z-distribution for exponential distribution w/unknown pop sd:", left_Z95_unkn2, right_z95_unkn2)#print result

#t distribution:
t_score <- qt(0.975, n3-1)#Calculate critical t-value for 95% CI and x1

#t score for exponiential distribution w/known pop sd
left_t95_kn <- mean3-t_score*se_kn2      #left_t95_kn=mean of 50 samples-t_score*se_kn2
right_t95_kn <- mean3+t_score*se_kn2     #right_t95_kn=mean of 50 samples+t_score*se_kn2
left_t95_unkn <- mean3-t_score*se_unkn2 #left_t95_unkn=mean of 50 samples-t_score*se_unkn2
right_t95_unkn <- mean3+t_score*se_unkn2#right_t95_unkn=mean of 50 samples+t_score*se_unkn2
paste("t-distribution for normal distribution w/known pop sd:", left_t95_kn2, right_t95_kn2)#print result
paste("t-distribution for normal distribution w/unknown pop sd:", left_t95_kn2, right_t95_unkn2)#print result


#t score for normal distribution w/known pop sd
left_t95_kn2 <- mean1-t_score*se_kn      #left_t95_kn2=mean of 50 samples-t_score*se_kn
right_t95_kn2 <- mean1+t_score*se_kn     #right_t95_kn2=50 samples+t_score*se_kn
left_t95_unkn2 <- mean1-t_score*se_unkn  #left_t95_unkn2=50 samples-t_score*se_unkn
right_t95_unkn2 <- mean1+t_score*se_unkn #right_t95_unkn2=50 samples+t_score*se_unkn



paste("t-distribution for normal distribution w/known pop sd:", left_t95_kn, right_t95_kn)#print result
paste("t-distribution for normal distribution w/unknown pop sd:", left_t95_kn, right_t95_unkn)#print result




