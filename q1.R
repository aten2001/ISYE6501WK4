set.seed(1)

q1_data <- read.table('uscrime.txt', header = TRUE)

n <- nrow(q1_data)

inTrain <- sample(1:n, size = ceiling(n*0.9))

data.train <- q1_data[inTrain,]
data.test <- q1_data[-inTrain,]

pca_comps <- prcomp(formula = ~. - Crime - So,  data = data.train, center = TRUE, scale = TRUE)

data_pcomps <- function(pcomps, data) {
    return(as.data.frame(cbind(predict(pcomps, data), So = as.factor(data$So), Crime = data$Crime)))
}

data.train.pcomps <- data_pcomps(pca_comps, data.train)

model <- lm(Crime ~ PC1 + PC2 + PC3 + PC4 + So, data = data.train.pcomps)

data.test.pcomps <- data_pcomps(pca_comps, data.test)

pred <- predict(model, data.test.pcomps)

mse <- ModelMetrics::mse(data.test$Crime, pred)

data.new <- data.frame(M = 14, So = 0, Ed = 10, Po1 = 12, Po2 = 15.5, LF = 0.64, M.F = 94, Pop = 150, NW = 1.1, U1 = 0.12, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

data.new.pcomps <- data_pcomps(pca_comps, data.new)

newpred <- predict(model, data.new.pcomps)