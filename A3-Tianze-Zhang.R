#Q1
two_hundred_iteration2<-numeric()  #vectors declaration  
two_hundred_iteration3<-numeric()
two_hundred_iteration4<-numeric()
two_hundred_iteration5<-numeric()
for(i in 1:200){
test_sample<-round(runif(10000,2,5)) #produce 10000 samples of k between 2-5
vec2<-numeric()
vec3<-numeric()
vec4<-numeric()
vec5<-numeric()
#print (test_sample)
for(j in 1:10000){                 #iteration through the 10000 samples
  k<-round(test_sample[j])
  
  if(k==2){                        #when k=2 p=100*T/(Tk) therefore p=100*24000/(24000*2)
    vec2<-c(vec2,100*24000/(24000*k))#add to the vector
  }
  else if(k==3){                   #when k=3 p=100*24000/(24000*3)
    vec3<-c(vec3,100*24000/(24000*k))
  }
  else if(k==4){                   #when k=4 p=100*24000/(24000*4)
    vec4<-c(vec4,100*24000/(24000*k))
  }
  else if(k==5){                  #when k=5 p=100*24000/(24000*5)
    vec5<-c(vec5,100*24000/(24000*k))
  }
}
sample_mean2 <- mean(vec2)        #take the average of each vector,add the mean to a new vector
two_hundred_iteration2<-c(two_hundred_iteration2,sample_mean2)
sample_mean3 <- mean(vec3)
two_hundred_iteration3<-c(two_hundred_iteration3,sample_mean3)
sample_mean4 <- mean(vec4)
two_hundred_iteration4<-c(two_hundred_iteration4,sample_mean4)
sample_mean5 <- mean(vec5) 
two_hundred_iteration5<-c(two_hundred_iteration5,sample_mean5)
}
mean_2<-mean(two_hundred_iteration2)  #take the average of each vector with 200 different means 
mean_3<-mean(two_hundred_iteration3)
mean_4<-mean(two_hundred_iteration4)
mean_5<-mean(two_hundred_iteration5)

print(mean_2)
print(mean_3)
print(mean_4)
print(mean_5)
probs<-numeric()                          #plot the graph
probs<-c(probs,mean_2,mean_3,mean_4,mean_5)
k<-numeric()
k<-c(k,2,3,4,5)
plot(percentage~k, xlim=c(2, 5), ylim=c(20, 50))
#Q2
iterations<-numeric()
for(i in 1:200){
percentage_mean<-0
max_percentage<-0
max_var<-0
for(i in 1:300){        #generates sigmas in range 0.01 to 3
  sigma <- 0.01*i
  var<-sigma*sigma      #variance=sigma square
  for(i in 1:10000){    #generate 10000 samples 
    percentage<-pnorm(2, mean = 0, sigma, lower.tail=TRUE)#find the percentage where 1<X<2 with mean
    - pnorm(1, mean =0, sigma, lower.tail=TRUE)#0 and standard deviation sigma
    if(percentage>max_percentage){    #find the max percentage and variance
      max_var<-var
      max_percentage<-percentage
    }
  }
  
}
iterations<-c(iterations,max_var)  #add the max variance for each 200 iterations to a vector

}
max_var_two_hundred_iterations<-mean(iterations)  #get the mean of all the variances
formatC(max_var_two_hundred_iterations,format="f",digits=5)





