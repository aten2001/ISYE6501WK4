set.seed(1)

q1_data <- read.table('uscrime.txt', header = TRUE)

q1_data$So <- q1_data$So

n <- nrow(q1_data)

inTrain <- sample(1:n, size = ceiling(n*0.9))

data.train <- q1_data[inTrain,]
data.test <- q1_data[-inTrain,]

pca_comps <- prcomp(formula = ~. - Crime - So,  data = data.train, center = TRUE, scale = TRUE)

predict_pcomps <- function(pcomps, data) {
    if ('Crime' %in% names(data)) {
        df <- data.frame(predict(pcomps, data), So = data$So, Crime = data$Crime)
    } else {
        df <- data.frame(predict(pcomps, data), So = data$So)
    }
    return(df)
}

data.train.pcomps <- predict_pcomps(pca_comps, data.train)

model <- lm(Crime ~ PC1 + PC2 + PC3 + PC4 + factor(So), data = data.train.pcomps)

data.test.pcomps <- predict_pcomps(pca_comps, data.test)

pred <- predict(model, data.test.pcomps)

mse <- ModelMetrics::mse(data.test$Crime, pred)

data.new <- data.frame(M = 14, So = factor(0, levels = c(0, 1)), Ed = 10, Po1 = 12, Po2 = 15.5, LF = 0.64, M.F = 94, Pop = 150, NW = 1.1, U1 = 0.12, U2 = 3.6, Wealth = 3200, Ineq = 20.1, Prob = 0.04, Time = 39.0)

data.new.pcomps <- predict_pcomps(pca_comps, data.new)

newpred <- predict(model, data.new.pcomps)


######
coefs <- pca_comps$rotation[,1:4] %*% t(t(model$coefficients[2:5])) / pca_comps$scale 

varnames <- rownames(coefs)

coefs <- setNames(as.vector(coefs), varnames)


equation <- function(varname, coef, center) {
    
    str = paste(coef, '*(', varname, ' - ', center, ')', sep = '') 
    return(str)
}

So_str <- paste(model$coefficients[6], '*So', sep = '')
equation_str <- paste('Crime = ', ' + ', model$coefficients[1], 
                      paste(c(mapply(FUN = equation, varnames, coefs, pca_comps$center), 
                              So_str), collapse = ' + '), sep = '')

######
my_predict_func <- function(coefs, center, newdata) {
    resp <- rep(0, nrow(newdata))
    
    for (i in 1:nrow(newdata)) {
        datapoint <- newdata[i,]
        
        for (var in varnames) {
            resp[i] <- resp[i] + coefs[var]*(datapoint[[var]] - center[var])
        }
        
        resp[i] <- resp[i] + model$coefficients[6]*datapoint$So + model$coefficients[1]
        
    }
    
    return(resp)
    
}

my_pred <- my_predict_func(coefs, pca_comps$center, data.test)

sum(abs(my_pred - pred) > 1e-5)
