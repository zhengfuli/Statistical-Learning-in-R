# exercise 2.8

# i
college = read.csv("./data/College.csv")
# fix(college)
# rownames(college) = college[,1]

college = college[,-1]
fix(college)

print(summary(college))

# ii
pairs(college[,1:10])

# iii
plot(college$Private, college$Outstate)

# iv
Elite = rep("No", nrow(college))
Elite[college$Top10perc>50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
print(summary(college$Elite))
plot(college$Elite, college$Outstate)

# v
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)

# vi.
library(rgl)
attach(college)
cl=c(ifelse(college$Grad.Rate<30,'red',ifelse(college$Grad.Rate<50,'orange',
ifelse(college$Grad.Rate<70,'yellow',ifelse(college$Grad.Rate<90,'green','blue')))))
plot3d(college$Top10perc, college$S.F.Ratio, college$Grad.Rate, col = cl)
fix(college)