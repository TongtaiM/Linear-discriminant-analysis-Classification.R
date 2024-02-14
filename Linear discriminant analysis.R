library(MASS)
library(ggplot2)
library(tidyverse) 
library(caret) 

data(iris)
str(iris)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(iris), replace=TRUE, prob=c(0.7,0.3))
train <- iris[sample, ]
test <- iris[!sample, ]

preproc.parameter <- train %>%  
  preProcess(method = c("center", "scale")) 
train.t <- preproc.parameter %>% predict(train) 
test.t <- preproc.parameter %>% predict(test) 

model <- lda(Species~., data=train.t)
model

lda_plot <- cbind(train, predict(model)$x)
ggplot(lda_plot, aes(LD1, LD2)) + geom_point(aes(color = Species))

predictions <- predict(model,test.t)
names(predictions)
head(predictions$class,5)
head(predictions$posterior,5)
head(predictions$x,5)


cm <- table(predictions$class, Actual = test.t$Species);cm
sum(diag(cm))/sum(cm)
  

