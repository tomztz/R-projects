#Q1
two_hundred_iteration<-numeric()    #create empty vector
for(j in 1:200){                    #200 iterations
test_sample<-round(runif(10000,1,6))#roll the dice 10000 times
print (test_sample) #print the result
#more variable decleration
temp<-0                             
i<-1
times_all_sides<-numeric()
is_valid<-TRUE
while(i<=10000){
  vec1<-c(FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)#create boolean vector
  count1<-0
while(!vec1[1]||!vec1[2]||!vec1[3]||!vec1[4]||!vec1[5]||!vec1[6]){
     vec1[test_sample[i]]<-TRUE            #set the vector at index of the number of sample to true,
                                           #when all the booleans are true in the vector then all sides
                                            #had appear
     count1<-count1+1                     #count records how many times the dice rolls
     if(i>10000){                        #if there are no remaining samples that will make all 6 sides to appear
                                        #break out the loop
       is_valid<-FALSE                #set is_valid to false so that it won't record the count   
       break
     }
     i<-i+1                       #i records the current element we are accessing
}
  if(is_valid){
times_all_sides<-c(times_all_sides,count1) #record the count
i<-temp+count1+1                           #start from the next element in the sample
temp<-temp+count1
  }

}
mean_times<-round(mean(times_all_sides))      #finding the mean of 10000 samples then 200 iterations
                                              #similarly to lab1
print(mean_times)
two_hundred_iteration<-c(two_hundred_iteration,mean_times)
}
result<-round(mean(two_hundred_iteration))
print(result)                   #print result gives value from 16-17

#Q2
two_hundred_P0<-numeric() #vector decleration
two_hundred_P1<-numeric()
two_hundred_P2<-numeric()
two_hundred_P3<-numeric()
two_hundred_P4<-numeric()
for(y in 1:200){
  count2<-0
  count3<-0
  count4<-0
  count5<-0
  count6<-0
for(k in 6:10){                 #dealing with all possible ks
OS<-1:k
cards<-sample(OS)
for(z in 1:10000){               #sample of 10000
guess<-sample(OS)  
NMatch <- length(which(OS==guess))
if(NMatch==0&&k==6){
  count2<-count2+1
}
else if(NMatch==0&&k==7){
  count3<-count3+1
}
else if(NMatch==0&&k==8){
  count4<-count4+1
}

else if(NMatch==0&&k==9){
  count5<-count5+1
}
else if(NMatch==0&&k==10){
  count6<-count6+1
}

}
P0<-count2/10000                #calculate probability for each k for 10000 samples
P1<-count3/10000
P2<-count4/10000
P3<-count5/10000
P4<-count6/10000
  
  
}
  two_hundred_P0<-c(two_hundred_P0,P0)     #store 200 iteration means for each k in vectors
  two_hundred_P1<-c(two_hundred_P1,P1)
  two_hundred_P2<-c(two_hundred_P2,P2)
  two_hundred_P3<-c(two_hundred_P3,P3)
  two_hundred_P4<-c(two_hundred_P4,P4)
}
mean_P0<-mean(two_hundred_P0)             #obtain the mean for 200 iterations
mean_P1<-mean(two_hundred_P1)
mean_P2<-mean(two_hundred_P2)
mean_P3<-mean(two_hundred_P3)
mean_P4<-mean(two_hundred_P4)

probs<-numeric()                                        #plot the graph
probs<-c(probs,mean_P0,mean_P1,mean_P2,mean_P3,mean_P4)
k<-numeric()
k<-c(k,6,7,8,9,10)
plot(probs~k, xlim=c(6, 10), ylim=c(0, 1))

print(mean_P0)                    #print result each mean approxiamately to 3.60
print(mean_P1)
print(mean_P2)
print(mean_P3)
print(mean_P4)




