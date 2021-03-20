install.packages("gtools")
library(gtools)
n<-1+9
k<-3    #last digit is 1

A <- permutations(n=10,r=3,v=1:n,repeats.allowed=T)
A
nrow(A)
#Q2
B <- permutations(n=10,r=3,v=1:n)
B
nrow(B)



#Q3
C <- combn(10, 3)
ncol(C)
#Q4
C








