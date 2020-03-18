#Group ID is 17
rm(list = ls())
library(MuMIn)
library(Metrics)

#Importing data
yelp_validate <- read.csv("Yelp_validate.csv", header = T)#%20 of the data
yelp_test <- read.csv("Yelp_test.csv", header = T) #%20 of the data
yelp_train <- read.csv("Yelp_train.csv", header = T) #%60 of the data
data <- read.table("Yelp_test.txt", header = F)

#Model test
globalmodel <- lm(stars ~., data = data)
summary(globalmodel)

# removed_variables <- c("X", "Id", "name", "date", "text", "categories")
# data_mod <- yelp_train[!(names(yelp_train) %in% removed_variables)]
# test_mod <- yelp_test[!(names(yelp_test) %in% removed_variables)]


#RMSE
prediction <- predict(globalmodel, yelp)
summary(prediction)
model_error <- rmse(yelp_train$stars, prediction)

#multi-nomial, 


#test
globalmodel <- lm(X~., data = yelp_test, na.action = "na.fail")


x <- c("stars",	"useful",	"funny",	"cool",	"nchar",	"nword",	"sentiment")
colnames(data) <- x


