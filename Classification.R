
#===================================================================
library(dplyr)
library(rpart)
library(rpart.plot)
library(caTools)
library(DescTools)
library(broom)
library(pROC)
library(ROSE)

# Import the csv file (customer_churn.csv) and explore it.
#====================== Write R code HERE ==========================

Customer_Churn <- read.csv("customer_churn.csv")

str(Customer_Churn)


#===================================================================




#======= Question 1 (1 Point) =======
# Q1-1. Build a logistic regression model to predict customer churn by using all possible variables, except customerID.
# Q1-2. Calculate the Pseudo R2 for the logistic regression.

#====================== Write R code HERE ==========================
# Q1-1
my_glm <- glm(Churn ~  SeniorCitizen + factor(Partner) +
                 factor(PhoneService) + factor(InternetService) + 
                 tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                 factor(TechSupport), data = Customer_Churn, family = "binomial")

my_glm

# Q1-2. 

PseudoR2(my_glm)
#McFadden 
#0.256012
#===================================================================



#======= Question 2 (1 Point) =======
# Q2-1. Split data into 70% train and 30% test datasets (Hint: caTools package).
# Q2-2. Train the same logistic regression on only "train" data.

#====================== Write R code HERE ==========================
# Q2-1.
set.seed(123) # to keep a consistence
split <- sample.split(Customer_Churn, SplitRatio = 0.7)

train <- subset(Customer_Churn, split == TRUE)
test <-  subset(Customer_Churn, split == FALSE)
# Q2-2.

my_prediction <-  glm(Churn ~  SeniorCitizen + factor(Partner) +
                           factor(PhoneService) + factor(InternetService) + 
                           tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                           factor(TechSupport), data = train, family = "binomial")
my_prediction
#===================================================================



#======= Question 3 (2 Point) =======
# Q3-1. For "test" data, make prediction using the logistic regression trained in Question 2 (Hint: broom package).
# Q3-2. Set the cutoff value and create a confusion matrix (Hint: ifelse function).

#====================== Write R code HERE ==========================
# Q3-1.
my_prediction <-  glm(Churn ~  SeniorCitizen + factor(Partner) +
                           factor(PhoneService) + factor(InternetService) + 
                           tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                           factor(TechSupport), data = train, family = "binomial") %>%
  augment(type.predictor = "response", newdata = test)
  
# Q3-2  
my_prediction <-  glm(Churn ~  SeniorCitizen + factor(Partner) +
                           factor(PhoneService) + factor(InternetService) + 
                           tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                           factor(TechSupport), data = train, family = "binomial") %>%
  augment(type.predictor = "response", newdata = test)  %>%
  mutate(predicted = ifelse(.fitted > 0.5,1,0))

table(test$Churn , my_prediction$predicted)


#===================================================================



#======= Question 4 (1 Point) =======
# Q4. Based on prediction results in Question 3, draw a ROC curve and calculate AUC (Hint: pROC package).

#====================== Write R code HERE ==========================

my_ROC <- roc(test$Churn , my_prediction$.fitted )
plot(my_ROC)
auc(my_ROC)
# Area under the curve: 0.8291




#===================================================================


#======= Question 5 (2 Point) =======
# Q5-1. For "train" data, build a decision tree to predict customer churn by using all possible variables, except customerID (which is same as previous questions) (Hint: rpart package).
# The result of decision tree should be presented as a decision plot (Hint: rpart.plot or rattle package)
# Q5-2. For "test" data, draw a ROC curve and calculate AUC (Hint: pROC package).

#====================== Write R code HERE ==========================
# Q5-1

decision_tree <- rpart(Churn ~  SeniorCitizen + factor(Partner) +
                        factor(PhoneService) + factor(InternetService) + 
                        tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                        factor(TechSupport), data = train, method = "class")




prp(decision_tree)



#===================================================================



#======= Question 6 (2 Point) =======
# Q6-1. Prune your decision tree with cp = 0.01.
# Q6-2. For "test" data, draw a ROC curve of the pruned decision tree and calculate AUC.

#====================== Write R code HERE ==========================
# I will oversample train data to balance data for reasonable prediction
train_ROSE <- ROSE(Churn ~ ., data = train)$data

table(train$Churn)
table(train_ROSE$Churn)

ROSE_decision_tree <- rpart(Churn ~  SeniorCitizen + factor(Partner) +
                         factor(PhoneService) + factor(InternetService) + 
                         tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
                         factor(TechSupport), data = train_ROSE, method = "class", cp = 0.01)

prune_decision_tree <- prune(ROSE_decision_tree, cp = 0.01)

prune_decision_tree


#===================================================================
# Q6-2.
# First we make prediction tree based on prune_decision_tree

predicted_tree <- glm(Churn ~  SeniorCitizen + factor(Partner) +
      factor(PhoneService) + factor(InternetService) + 
      tenure + factor(Dependents) + MonthlyCharges + TotalCharges +
      factor(TechSupport), data = train, family = "binomial") %>%
  augment(type.predictor = "response", newdata = test)  %>%
  mutate(predicted = ifelse(.fitted > 0.5,1,0))



ROC_testData <- roc(test$Churn , predicted_tree$predicted )
plot(ROC_testData)
auc(ROC_testData)

# Area under the curve: 0.6645





#======= Question 7 (1 Point) =======
# Q7. Among predictive models you have developed above, which one is best to predict customer churn?
# Use comments to write your opinion (#).

#====================== Write R code HERE ==========================

# Pseudo R-squared is 0.256012. According to the rule Pseudo R-squared ranges from 0.2 to 0.4 is indication of very good model fit. 
#Conversely, after pruning the classification tree we get AUC result which indeed does not allow us to tell the same thing. 
#Additionally, oversampling plays increasingly important role in balancing data in order to get reasonable prediction.




#===================================================================
