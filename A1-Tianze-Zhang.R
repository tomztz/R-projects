library(numbers)
two_hundred_iteration<-numeric()
for(i in 1:200){                        #200 iterations
vec <- numeric()
for (i in 1:10000) {                    #10000 samples
  
  r<-runif(2, -.Machine$integer.max, .Machine$integer.max)#random generate 2 numbers from range
                                            #-max integer to max integer
  num1<-round(r[1])               #assign the two numbers to different variables
  num2<-round(r[2])
 
  if(coprime(num1,num2)==TRUE){        #if the numbers are coprime add 1 to vector c
    vec<-c(vec,1)
  }
  else{
    vec<-c(vec,0)                 #else add 0 to vec
  }
  
}
sample_mean <- mean(vec)         #calculate the mean of the 10000 values in vec
two_hundred_iteration<-c(two_hundred_iteration,sample_mean)#generate 200 different means add to vector

}
result<-mean(two_hundred_iteration)#get the mean of the 200 different means
print(result)                     #print answer giving approxiamately 0.65