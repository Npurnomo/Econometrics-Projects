#Coded by Nico Purnomo, Natasya Lucky, and Abraham
library(stargazer)
library(ggplot2)
# load the dataset
A1_df <- read.csv("A1_Data.csv")
print(A1_df)

# 2.1 convert assg and exam to percent. obtain descriptive stats and histograms
A1_df$assg <- A1_df$assg/30*100
A1_df$exam <- A1_df$exam/70*100

print(A1_df)
rows(A1_df)

# generate summary statistics .... dont forget to make into table
print(mean(A1_df$assg))
print(median(A1_df$assg))
print(sd(A1_df$assg))
print(min(A1_df$assg))
print(max(A1_df$assg))

print(mean(A1_df$exam))
print(median(A1_df$exam))
print(sd(A1_df$exam))
print(min(A1_df$exam))
print(max(A1_df$exam))

statstable_assg <- rbind(mean(A1_df$assg),
                    median(A1_df$assg),
                    sd(A1_df$assg),
                    min(A1_df$assg),
                    max(A1_df$assg))
rownames(statstable_assg)<-c("Mean", "Median", "SD", "Min", "Max")
colnames(statstable_assg)<-"Assignments Marks (%)"
print(statstable_assg)

statstable_exam <- rbind(mean(A1_df$exam),
                         median(A1_df$exam),
                         sd(A1_df$exam),
                         min(A1_df$exam),
                         max(A1_df$exam))
rownames(statstable_exam)<-c("Mean", "Median", "SD", "Min", "Max")
colnames(statstable_exam)<-"Exam Marks (%)"
print(statstable_exam)

# round the table entries to 3dp
print(round(statstable_assg,3))
print(round(statstable_exam,3))

# export the table as a .csv file
write.csv(statstable_assg, file="statstable_assg.csv")
write.csv(statstable_exam, file="statstable_exam.csv")

# EXAMS are harder to score well, median and mean for both are similar
hist(A1_df$assg, main = "Histogram of Assignment Scores (%)", xlab ="%")
hist(A1_df$exam, main = "Histogram of Exam Scores (%)", xlab = "%")
# 2.2) Marks available: 4
#Run a simple regression of exami on a constant and assgi
#. Outline both the statistical and
#causal interpretations of the parameter estimates.

eqn1 <- lm(exam~assg, data=A1_df)
print(summary(eqn1))

#how to present it in a neat table?
# on average increases and causes

#2.3

new_A1_df <- A1_df [which(A1_df$flag.0 == "0"),]
eqn2 <- lm(exam~assg, data=new_A1_df)
print(summary(eqn2))

#2.4 simultaneous bias? no feedback loop, exam after  

#2.5 assignments are taken individually
#Outline the conditions under which the omission of viewsi would
#generate omitted variable bias in the regression model from question 2.3. What direction do
#you think the bias would take? Explai

#omission of views would create a positive bias as I assume that increase in views would
#increase both assg and exam as students are likely to be more involved in the subject
#while actively thinking and discussing about the problems

#2.6  Individual assignments reflect the individual abilities of all the students. 
# hence there is no correlation between the assignment marks themselves ??

#2.7 
eqn3 <- lm(exam~assg+views, data=new_A1_df)
print(summary(eqn3))

# still need to address other OVBs, e.g. IQ, Marks in other similar subjects

#2.8 Do you think that your regression results from question 2.7 are truly comparable to your
# regression results from question 2.3? Explain your answer.

# 37 observations deleted due to missingness, any effect?

stargazer(eqn1, type = "html", title  = "Regression of Exams on Assignments", out = "reg1.htm")
stargazer(eqn2, type = "html", title  = "Regression of Exams on Assignments when there is not any 0 score on assignment", out = "reg2.htm")
stargazer(eqn3, type = "html", title  = "Regression of Exams on Assignments when there is not any 0 score on assignment controlling ED views", out = "reg3.htm")

