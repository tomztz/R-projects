Lab1_true<-read.csv(file="C:/Users/tomzt/RstudioProject/lab1/Lab1.csv", header=TRUE)
summary(Lab1_true$EARN) 
table(Lab1_true$Job.class) 
table1<-table(Lab1_true$EDUC, Lab1_true$Gender, Lab1_true$Job.class)
prop.table(table1)
hist(Lab1_true$EARN)
boxplot(Lab1_true$EARN~Lab1_true$Job.class) 
Lab1_true$EARNx10000 = Lab1_true$EARN/100000 
plot(Lab1_true$EARNx10000, Lab1_true$AGE)