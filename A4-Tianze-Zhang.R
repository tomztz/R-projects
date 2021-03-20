#Q1                               #assume lambda is 1 for this question
two_hundred_iteration<-numeric()  
for(i in 1:200){
t<-rexp(1)              #Generate one sample of an exponential distribution with parameter lambda
p<-numeric()
for(i in 1:10000){
  
p<-c(p,ppois(5,t,lower.tail=TRUE))  #generate 10,000 samples from a Poisson distribution with parameter lambda*t and find the percentage of them 
                                    #which are less than or equal k=2 and add to vector p
}
p_mean<-mean(p)                     #find the mean of p

two_hundred_iteration<-c(two_hundred_iteration,p_mean)#for each of the mean for 200 iterations add to a vector
}
two_hundred_iteration_mean<-mean(two_hundred_iteration)#find the mean of the vector
print(two_hundred_iteration_mean)#print result


#Q2
vec1<-0              #vectors declaration
vec2<-0
vec3<-0
vec4<-0
vec5<-0
for(j in 1:200){
a<-1                #a=1
exp1<-0             #variables declaration
exp2<-0
exp3<-0
exp4<-0
exp5<-0
for(z in 1:5){    #iterate from a=1 to a=5
vec<-numeric()
y<-round(runif(10000,0,a))  #generate 10000 samples with min 0 max a
for(i in 1:10000){          #iterate through the 10000 samples
  if(round(y[i])>=a/2){     #if X>=a/2 add a/2 to the vector
    vec<-c(vec,a/2)
    
  }
  else{                     #if X<a/2 add X to the vector
    vec<-c(vec,round(y[i]))
  }
  
}
if(a==1){
  exp1<-mean(vec)         #get the expected value for a=1...5 by getting the mean
}
else if(a==2){
  exp2<-mean(vec)
}
else if(a==3){
  exp3<-mean(vec)
}
else if(a==4){
  exp4<-mean(vec)
}
else if(a==5){
  exp5<-mean(vec)
}
a<-a+1                    #counter++
}
vec1<-c(vec1,exp1)        #add the expected value to different vectors for 200 iterations
vec2<-c(vec2,exp2)
vec3<-c(vec3,exp3)
vec4<-c(vec4,exp4)
vec5<-c(vec5,exp5)

}
exp1_two_hundred_iterations<-mean(vec1)  #find the mean of 200 iterations
exp2_two_hundred_iterations<-mean(vec2)
exp3_two_hundred_iterations<-mean(vec3)
exp4_two_hundred_iterations<-mean(vec4)
exp5_two_hundred_iterations<-mean(vec5)

print(exp1_two_hundred_iterations)
print(exp2_two_hundred_iterations)
print(exp3_two_hundred_iterations)
print(exp4_two_hundred_iterations)
print(exp5_two_hundred_iterations)
expected_x<-numeric()                                        #plot the graph
expected_x<-c(expected_x,exp1_two_hundred_iterations,exp2_two_hundred_iterations,
              exp3_two_hundred_iterations,exp4_two_hundred_iterations,exp5_two_hundred_iterations)
a<-numeric()
a<-c(a,1,2,3,4,5)
plot(expected_x~a, xlim=c(1, 5), ylim=c(0, 2.0))
