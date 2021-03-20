read.csv(file="C:/Users/tomzt/RstudioProject/lab6/survey.csv", header=TRUE)
tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

#data:  tbl
#X-squared = 5.4885, df = 6, p-value = 0.4828

#Question1:

#the table function creates a two way table of survey$Smoke being the x axis and the survey$Exer being the
#y axis in tbl. 
survey$Smoke
survey$Exer
#we could see after running the above survey, that there are four different levels for survey$Smoke:
#Heavy Never Occas Regul
#and there are three different levels for survey$Exer:Freq None Some
#therefore our x axis will have the four 4 levels as headings i.e 4 rows
#and for the same reason there will be 3 columbs
#from the formula of degrees of freedom:
#DF = ((#rows-1)*(#columns-1))
#therefore (3-1)*(4-1)=6

#Question2:
#Assumptions:
#   For this test it is assumed all expected values are greater than 1 and no more than 20% are less than 5.
#   As per usual a simple random sample is presumed.
#   Each observation is assumed to be independent of one another.

#Question3:

#the warning message is due to the difference between the proportion of the expected value relative to the sample
#size.

#Question4:
#(H0 null ) there is no association between the frequency of smoking and the frequency of exercises
#(H1 alternative)there is an association between the frequency of smoking and the frequency of exercises

#Question5:
#based on the result "p-value = 0.4828", it is geater than 0.05 the significant level, therefore it
#is not significant and we do not have enough evidence to accept H1, we fail to reject H0.
#therefore, we could conclude there are no relations between the frequency of smoking and the frequency of exercise.