rm(list=ls())
library(Metrics)
library(xlsx)
library(MASS)
library(ggplot2)

#Importing data
Yelp_train <- read.csv("Yelp_train.csv", header = T)
Yelp_test <- read.csv("Yelp_test.csv", header = T)
Yelp_validate <- read.csv("Yelp_validate.csv", header = T)
Yelp_tv <- read.csv("Yelp_tv.csv", header = T)
dat_train <- read.csv("dat_train.csv", header = T)
Pixu_submission <- read.csv("Pixu_submission.csv")

#Yelp data with extra 1000 predictors
#mod <- read.csv("Yelp_train_mod.csv", header = T)
mod <- read.csv("dat_train.csv", header = T)
removed_variables <- c("X", "Id", "name", "text", "date", "city", "categories")
mod_refined <- mod[,!(names(mod) %in% removed_variables)]
mod_refined$nchar <- log(mod_refined$nchar)
mod_refined$nword <- log(mod_refined$nword)
model <- lm(starss~., data = mod_refined)
model_summary <- summary(model)


#Yelp data with less variables
reduced_yelp_data <- read.table("Yelp_test.txt")
x <- c("stars",	"useful",	"funny",	"cool",	"nchar",	"nword",	"sentiment")
colnames(reduced_yelp_data) <- x
head_data <- head(reduced_yelp_data, 1000)
head_data <- head_data[-c(1),]


#RMSE
prediction <- predict(model, all_train)
prediction <- abs(prediction)
summary(prediction)
model_error <- rmse(Yelp_train$stars, prediction)

error <- (Yelp_train$stars - floor(prediction))
sum(error)


#Testing
data <- head(Yelp_train, 1000)
removed <- c("X", "Id", "name", "text", "date", "city", "categories")
data <- data[,!(names(data) %in% removed)]
model <- lm(stars~., data = data)
summary(model)


#Rounding, reset count and prediction each time
count <- 0
for (val in prediction) {
  count = count  +1
  if (val < 1) {
    prediction[count] = 1
  } else if (val > 4.5) {
      prediction[count] = 5
  } else if (val < 2.5 & val > 2) {
      prediction[count] = 2
  } else if (val > 3.8 & val < 4) {
      prediction[count] = 4
  }
}


#exporting
write.csv(prediction_frame, "C:/Users/James Thomason/Desktop/prediction.csv")
write.xlsx(variable_names, "C:/Users/James Thomason/Desktop/names.xlsx")


##Exporting the review text
write.xlsx(Yelp_train$text, "C:/Users/James Thomason/Desktop/review_text.xlsx")

#Variable Selection
p_values <- model_summary$coefficients[,4]
p_values <- as.character(p_values)
p_values <- as.double(p_values)
big_p_variables <- which(p_values > .05)
small_p_values <- head(order(p_values))
mod_small <- mod_refined[-big_p_variables]
model_small <- lm(stars~., data = mod_small)

#RMSE for smaller model
prediction <- predict(model_small, mod_refined)
prediction <- abs(prediction)
small_model_error <- rmse(mod_refined$stars, prediction)

#Yelp_tv prediction work
dat_out <- read.csv("dat_out.csv", header = T)
dat_out$nchar <- log(dat_out$nchar)
dat_out$nword <- log(dat_out$nword)
prediction <- predict(model, dat_out)
prediction <- abs(prediction)
prediciton <- as.data.frame(prediction)
prediction_frame <- data.frame(Pixu_submission$Id, prediciton$Expected)


#Graph creation
load("all_train.RData")
model <- lm(Yelp_train$stars~., data = all_train)
summary(model)
prediction <- as.character(prediction)
prediction <- as.double(prediction)

#Residuals
resid_predict <- rstudent(model)
plot(prediction, resid_predict)
plot(model, which = 3)

#Scale-Location


#QQ plot
qqnorm(resid_predict)
abline(0, 1, col = "blue", lwd = 2.5)

#Cook's
cookie <- cooks.distance(model)
plot(cookie)

##Star Graphs

#VS lognword
vs_nword <- data.frame(Yelp_train$stars, all_train$nword)
names(vs_nword) <- c("stars", "log_nword")
ggplot(vs_nword, aes(x= stars, y = log_nword)) +
  geom_bar(stat = "identity")

#VS sentiment
vs_sent <- data.frame(Yelp_train$stars, all_train$cool)
names(vs_sent) <- c("stars", "cool")
ggplot(vs_sent,mapping = aes(x= stars, y = cool)) +
  geom_bar(stat = "identity")


+##testing
names(all_train)[412] <- c("FUNNY")
names(all_train[412])
