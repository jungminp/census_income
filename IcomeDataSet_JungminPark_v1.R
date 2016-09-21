adult.data = read.table("C:/Program Files/RStudio/resources/adult.data.txt",
                        sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education", "education_num",
                                                     "marital", "occupation", "relationship", "race","sex",
                                                     "capital_gain", "capital_loss", "hr_per_week","country", "income"),
                        fill=FALSE,strip.white=T)

#is.na(adult.data) <- adult.data=='?'
#adult.valid <- na.omit(adult.data)

adult.valid <- subset(adult.data, type_employer != "?" & occupation != "?" & country != "?")


adult.valid[["education_num"]] <- NULL
adult.valid[["fnlwgt"]] <- NULL

adult.valid$income_num <- as.numeric(0)
for (i in 1:nrow(adult.valid)){
  if (adult.valid$income[i]== ">50K")
    adult.valid$income_num[i] <- as.numeric(1)
}

cor(x = adult.valid$age, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$type_employer, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$education, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$marital, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$occupation, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$relationship, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$race, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$sex, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$capital_gain, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$capital_loss, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$hr_per_week, y = adult.valid$income_num, use = "everything",method = c("pearson"))
cor(x = adult.valid$country, y = adult.valid$income_num, use = "everything",method = c("pearson"))


library(rpart)

decision_tree <- rpart(income ~ age + type_employer + education + marital + occupation + relationship +
                         race + sex + capital_gain + capital_loss + hr_per_week + country
                       , method="class", data=adult.valid)

summary(decision_tree)

# Display the performance results
printcp(decision_tree)
# Visualize cross-validation results
plotcp(decision_tree)

plot(decision_tree, uniform=TRUE, main="Classification Tree")
text(decision_tree, use.n=TRUE, all=TRUE, cex=.8)

library(rpart.plot)
# Create plot of tree using the rpart.plot package for a better look
rpart.plot(decision_tree)

#Trying out different plotting options
# Just with the decision criteria at the nodes
rpart.plot(decision_tree, type = 1)
# Include the number of records classified as yes or no
rpart.plot(decision_tree, type = 2, extra=2)
# Probability per class of observations in the node (conditioned on the node, sum across a node is 1).
rpart.plot(decision_tree, type = 2, extra=4)
#The probabilities times the fraction of observations in the node (the probability relative to all observations, sum across all leaves is 1).
rpart.plot(decision_tree, type = 2, extra=9)
