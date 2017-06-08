library('rpart')
library('randomForest')
library('caret')
library('parallel')

set.seed(123)

#tree <- rpart(Crime ~ ., data = data.train, method = 'anova')

trCon <- trainControl(method = 'cv', number = 10)

tree <- train(Crime ~., data = data.train, method = 'rpart', trControl = trCon)

inVal <- sample(1:nrow(data.train), size = ceiling(n*0.1))

forest <- randomForest(Crime ~ ., data = data.train, ntree = 1000)

tree_pred <- predict(tree, data.test)
forest_pred <- predict(forest, data.test)

ModelMetrics::mse(data.test$Crime, tree_pred)

ModelMetrics::mse(data.test$Crime, forest_pred)

ntrees <- seq(from = 50, to = 5000, by = 50)
mse_vec <- rep(0, length(ntrees))

folds <- createFolds(data.train$Crime, k = 5)

# ntree mse plot

test_mse <- function(fold, num_trees, data){
    data.fold <- data[fold,]
    data.outside <- data[-fold,]
    forest_test <- randomForest::randomForest(Crime ~ ., data = data.outside, ntree = num_trees)  
    mse_val <- ModelMetrics::mse(data.fold$Crime, predict(forest_test, data.fold))
} 

# Set up parallel processing clusters
cl <- makePSOCKcluster(4)

for (i in seq_along(ntrees)) {
    
    
    
    mse_test <- parSapply(cl = cl, X = folds, FUN = test_mse, 
                          num_trees = ntrees[i], data = data.train)
    
    mse_vec[i] <- mean(mse_test)
}

stopCluster(cl)

plot(ntrees, mse_vec)