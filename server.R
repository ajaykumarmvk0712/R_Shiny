
library(ggplot2)
library(dplyr)
library(randomForest)
library(glmnet)
library(ada)
library(formattable)

# Helper functions

# user must provide a value to bins here and in the 
# function

bins <-10 ## number of groups to create
x.info <- seq(0, 100, by = bins)  ## X axis values
x <- x.info ## vector of X axis for cumulative gains plot
sim <- 500  ## simulations for crude estimate of AUC
seed_value <- 0.005  ## random seed

## Function will output cumulative lift values
## Arguments are vector of fitted probabilities,
## vector of observed values and number of bins to partition

make.gains <- function(score.values, observed.values,
                       bins = 10, sim = 500,
                       seed_value){
  
  # @Arguments
  # score.values : Vector of fitted probabilities from a model
  # observed.values : Vector of observed binary values for a model
  # can be in three formats
  # 0 or 1 labels
  # "Y" or "N" labels
  # "0" or "1" as labels
  # bins : The number of groups to split score values
  # sim  : Number of simulations to calculate crude estimate of AUC
  # seed_value : Random seed to reproduce AUC Value
  
  # @Returns
  #y.axis.info : The vector of Y axis values for plotting
  # cumulative gains curve
  # Results.table  : A table containing the following columns
  # Decile number 10 being the highest and 1 the lowest
  # Responders indicating number of responders in each group
  # Non-Responders indicating number of non-responders in each group
  # lift.perc indicating percentage of responders in each group
  # cum.lift indicating number of cumulative responders
  # cum.lift.perc indicating percentage cumulative gain
  # auc_value  : Estimated AUC of the model.
  
  # Starting the checks before function evaluation
  
  # check if ggplot2, pander, Hmisc packages need to be installed
  set.seed(seed_value)
  list.of.packages <- c("ggplot2","dplyr")
  new.packages <- list.of.packages[!(list.of.packages %in%  
                                       installed.packages()[,"Package"])]      
  if(length(new.packages)) {
    install.packages(new.packages);
  }
  library(ggplot2)
  library(dplyr)
  #library(pander)
  
  ## Check if observed values is in the required format
  
  ## Stopping criteria 1
  if( any(unique(observed.values) =="Y") |
      any(unique(observed.values) =="N") ) {
    observed.values <- ifelse(observed.values == "Y", 1, 0)
  } else if
  (any(unique(observed.values) == "1") |
   any(unique(observed.values) == "0")) {
    observed.values <- ifelse(observed.values == "1", 1, 0)
  } else if
  (any(unique(observed.values) == 1) |
   any(unique(observed.values) == 0)) {
    observed.values <- observed.values
  } else {stop(print(paste("Observed values not in
                          required binary format. Please      
                           provide observed.values as a factor with
                           levels", "Y or N","or 0 and 1", 
                           "or","character 0 and character 1",
                           sep = " ")))}
  
  ## Check if predicted values are probabilities
  
  ## Stopping criteria 2
  if(any(score.values > 1) |
     any(score.values < 0) )
    stop(print(paste("Warning: Predicted scores must
                     be probabilities between 0 and 1")))
  
  ## Warn user in case predicted values are exactly 0 or 1
  if(any(score.values == 1) |
     any(score.values == 0) ) {
    print(paste("Warning: Predicted scores contain
                values which are exactly 0 or 1")) }
  
  ## Stopping criteria 3
  
  l1 <- length(score.values)
  l2 <- length(observed.values)
  if(l1 != l2) stop(
    print(paste("Observed and Score values differ in length",
                sep = "")))
  
  ## Actual start of function
  # Cut probabilities into equal groups
  # find how many of the successes lie in each group
  
  # use dplyr's ntile, mutate and count functions
  
  tiles <- ntile(score.values, bins)
  groups <- unique(score.values)
  g <- length(groups)
  
  # if(g < bins) 
  #   stop(print(paste("There are", g, "unique score values.",
  #                    "Please check score values else dplyr will",
  #                    "forces creation of", bins, "groups",
  #                    sep = " ")))
  # 
  data <- data.frame(observed.values, score.values, tiles)
  answer <- data %>% group_by(tiles) %>% 
    summarise(total.count = n(), 
              responders = length(which(observed.values==1)),
              non.responders = length(which(observed.values==0)))
  
  r <- nrow(answer)
  # getting the top deciles at the first row
  answer <- answer[r:1 ,]
  
  # add percent lift and percent cumulative lift
  
  answer <- mutate(answer, lift.perc = responders / sum(responders))
  answer <- mutate(answer, cum.lift = cumsum(responders))
  answer <- mutate(answer, cum.lift.perc = cum.lift / 
                     sum(responders))
  answer <- answer[, -1]
  answer <- as.data.frame(answer)
  answer[, 4] <- answer[, 4] 
  answer[, 6] <- answer[, 6] 
  colnames(answer) <- c("Count",
                        "Responders", "Non\n Responders",
                        "Percent\n Responders",
                        "Cumulative\n Responders",
                        "Cumulative\nPercent\nResponders")
  y.info <- c(0, as.numeric(answer[,6]))
  y.info <- y.info * 100
  
  # Area under the curve
  pos <- which(observed.values == 1)
  neg <- which(observed.values == 0)
  
  auc <- sum(replicate(sim, score.values[sample(pos, 1)] >
                         score.values[sample(neg, 1)]))
  auc_est <- round((auc / sim), 4) ## crude AUC estimate
  results <- answer
  results[, 4] <- results[, 4] * 100
  results[, 6] <- results[, 6] * 100
  colnames(results) <- c("Count",
                         "Responders", "Non\n Responders",
                         "Percent\n Responders",
                         "Cumulative\n Responders",
                         "Cumulative\nPercent\nResponders")
  
  return(list(y.axis.info = y.info, 
              results.table = results,
              auc_value = auc_est))
  }

## Graphing the lift curve
graph.gains <- function(x, y){
  pl.object <- qplot(x, y, geom = "line",
                     main = "Cumulative Gains Chart",
                     col = I("red")) + 
    geom_segment(aes(x = 0, 
                     xend = 100, y = 0, yend = 100)) 
  
  pl.object <- pl.object + 
    scale_y_continuous(name = "Lift Percent", 
                       breaks=seq(0, 100, 10)) + 
    scale_x_continuous(name = "Model Deciles", 
                       breaks=seq(0, 100, 10))  
  return(pl.object)
}



####################################################
####################################################
####################################################

## Example

## I have referred the link
## http://www.ats.ucla.edu/stat/r/dae/logit.htm
## It talks about a logistic regression example.

# # split URL string to stick to R coding style
# part1 <- "http://www.ats.ucla.edu/"
# part2 <- "stat/data/binary.csv"

# # use paste function to create url
# url <- paste(part1, part2, sep = "")
# 
# # load data from url
# mydata <- read.csv(url)
# 
# # create rank as a factor variable
# mydata$rank <- factor(mydata$rank)
# 
# # build logistic regression model
# mylogit <- glm(admit ~ gre + gpa + rank,
#                data = mydata, family = "binomial")
# 
# # create data only of predictors
# newdata <- data.frame(mydata$gre, mydata$gpa, mydata$rank)
# 
# # obtain scores from model
# score.values <- predict(mylogit, newx = newdata, type = "response")
# 
# # create vector of observed values
# observed.values <- mydata$admit
# 
# # create data of cumulative gains
# cuml.gains <- make.gains(score.values, observed.values,
#                          bins = 10, sim = 500,
#                          seed_value = 0.005)
# 
# # create graph of cumulative gains
# gains.curve <- graph.gains(x.info, cuml.gains$y.axis.info)
# 

# logistic regression no penalty
logistic.scores <- function(x, y, xtest){
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  result.model <- glm(y ~., data = data.frame(y, x), 
                      family = binomial(link = "logit"))
  score.values <- predict(result.model, newdata = xtest,
                          type = "response")
  score.values <- as.vector(score.values)
  return(score.values)
}

# LASSO Penalty
lasso.scores <- function(x, y, xtest){
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  f <- which(sapply(x, class) == "factor")
  xfactor <- model.matrix(y ~ x[,f])[,-1]
  xmat <- cbind(x[, -f], xfactor)
  xmat <- as.matrix(xmat)
  t <- dim(xtest)[1]
  xtestfactor <- model.matrix(y[1:t] ~xtest[,f])[,-1]
  xtestmat <- cbind(xtest[, -f], xtestfactor)
  xtestmat <- as.matrix(xtestmat)
  library(glmnet)
  result.model <- cv.glmnet(xmat , y, alpha = 1,
                            family = "binomial",
                            type.measure = "auc")
  score.values <- predict(result.model, newx = xtestmat,
                          s = result.model$lambda.1se,
                          type = "response")
  score.values <- as.vector(score.values)
  return(score.values)
}

# Ridge Penalty
ridge.scores <- function(x, y, xtest){
  
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  f <- which(sapply(x, class) == "factor")
  xfactor <- model.matrix(y ~ x[,f])[,-1]
  xmat <- cbind(x[, -f], xfactor)
  xmat <- as.matrix(xmat)
  t <- dim(xtest)[1]
  xtestfactor <- model.matrix(y[1:t] ~xtest[,f])[,-1]
  xtestmat <- cbind(xtest[, -f], xtestfactor)
  xtestmat <- as.matrix(xtestmat)
  library(glmnet)
  result.model <- cv.glmnet(xmat , y, alpha = 0,
                            family = "binomial",
                            type.measure = "auc")
  score.values <- predict(result.model, newx = xtestmat,
                          s = result.model$lambda.1se,
                          type = "response")
  score.values <- as.vector(score.values)
  return(score.values)
}


# Adaboost Exponential Loss
ada.exp.scores <- function(x, y, xtest){
  
  library(gbm)
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  f <- which(sapply(x, class) == "factor")
  xfactor <- model.matrix(y ~ x[,f])[,-1]
  xmat <- cbind(x[, -f], xfactor)
  xmat <- as.matrix(xmat)
  t <- dim(xtest)[1]
  xtestfactor <- model.matrix(y[1:t] ~xtest[,f])[,-1]
  xtestmat <- cbind(xtest[, -f], xtestfactor)
  xtestmat <- as.matrix(xtestmat)
  result.model <- gbm.fit(xmat, y, distribution = "adaboost",
                      n.trees = 5000, 
                      verbose = FALSE)
  score.values <- predict.gbm(result.model, 
                          newdata = xtestmat,
                          n.trees = 5000,
                          type = "response")
  score.values <- as.vector(score.values)
  return(score.values)
}

# Probit regression

probit.scores <- function(x, y, xtest){
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  result.model <- glm(y ~., data = data.frame(y, x), 
                      family = binomial(link = "logit"))
  score.values <- predict(result.model, newdata = xtest,
                          type = "response")
  score.values <- as.vector(score.values)
  return(score.values)
}

# Random Forest

rf.scores <- function(x, y, xtest){
  library(randomForest)

  y <- as.factor(y)
  x <- data.frame(x)
  xtest <- data.frame(xtest)
  f <- which(sapply(x, class) == "factor")
  xfactor <- model.matrix(y ~ x[,f])[,-1]
  xmat <- cbind(x[, -f], xfactor)
  t1 <- dim(xtest)[1]
  xtestfactor <- model.matrix(y[1:t1] ~xtest[,f])[,-1]
  xtestmat <- cbind(xtest[, -f], xtestfactor)
  rf.model <- randomForest(xmat, y, ntree = 700)
  colnames(xtestmat) <- colnames(xmat)
  score.values <- predict(rf.model, 
                          newdata = xtestmat,
                          type = "prob")
  score.values <- as.vector(score.values[,2])
  return(score.values)
}

# switch will assign each label the
# corresponding function
return.scores <- function(x, y, xtest, type){
  switch(type,
         "LASSO" = lasso.scores(x, y, xtest),
         "logis" = logistic.scores(x, y, xtest),
         "Ridge" = ridge.scores(x, y, xtest),
         "Adaexp" = ada.exp.scores(x, y, xtest),
         "Probit" = probit.scores(x, y, xtest),
         "randomForest" = rf.scores(x, y, xtest))
}

# Make confusion Matrix
make.conf.matrix <- function(observed.class, 
                             score.values,
                             cutoff){
  predicted.class <- ifelse(score.values >= cutoff, 1, 0)
  cm <- table(observed.class,predicted.class)
  cm <- as.data.frame.matrix(cm)
  if(ncol(cm) == 2){cm <- cm} else {
    zeroes <- c(0, 0)
    zeroes <- as.integer(zeroes)
    cm <- data.frame(cm, zeroes)
  }
  rownames(cm) <- c("Observed 0s", "Observed 1s")
  colnames(cm) <- c("Predicted 0s", "Predicted 1s")
  cm <- cm
  return(list(confusion_matrix = cm))
}

# source the data
part1 <- "http://www.ats.ucla.edu/"
part2 <- "stat/data/binary.csv"

# use paste function to create url
url <- paste(part1, part2, sep = "")

# load data from url
mydata <- read.csv(url)

# create rank as a factor variable
mydata$rank <- factor(mydata$rank)

# create data only of predictors
newdata <- data.frame(mydata$gre, mydata$gpa, mydata$rank)
colnames(newdata) <- c("gre", "gpa", "rank")

# create vector of observed values
observed.values <- mydata$admit
y <- observed.values

# design matrix
gre <- mydata$gre
gpa <- mydata$gpa
univ.rank <- mydata$rank

# Separate into training set
train_test <- function(dataset, y, 
                       fraction, ...){
  set.seed(54)
  req_data <- data.frame(dataset, y)
  sample_data <- req_data %>% group_by_(...) 
  train_data <- sample_frac(sample_data, fraction)
  train.idx <- as.numeric(rownames(train_data)) # because rownames() returns character
  x <- train_data[, -ncol(train_data)]
  test <- sample_data[-train.idx, ]
  xtest <- test[, -ncol(test)]
  ytest <- test[, ncol(test)]
  ytest <- as.data.frame(ytest)
  ytest <- as.factor(ytest[,1])
  response <- train_data[, ncol(train_data)]
  response <- as.data.frame(response)
  response <- as.numeric(response[,1])
  return(list(x = x,
              xtest = xtest,
              response = response,
              ytest = ytest))
}

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  # source functions
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  train.test <- reactive({
    train_test(newdata, y, 
               input$t, "rank")
  })
  
  changes <- reactive({
    thresh <- input$p
    train  <- input$t
  })
  
  scores <- reactive({
    # create score values
    return.scores(train.test()$x , train.test()$response,
                  train.test()$xtest, input$model_type)
  })
  
  # Make cumulative gains
  cuml_gains <- reactive({
  cuml.gains <- make.gains(scores(), train.test()$ytest,
                            bins = 10, sim = 5000,
                            seed_value = 0.05)
  })
  
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    
    # create graph of cumulative gains
    graph.gains(x.info, cuml_gains()$y.axis.info)
    
  })
  
  # Generate a confusion matrix from data
  output$summary <- renderTable({
    # actual start of function
    # predicted class
    
    conf_matrix <<- reactive({
      make.conf.matrix(as.numeric(train.test()$ytest), scores(), 
                       input$p)$confusion_matrix
    })
    conf_matrix()
  })
  
  # Generate a summary of important metrics
  output$Metrics <- renderTable({
    # predicted class
    auc <- cuml_gains()$auc_value
    tpr <- round(conf_matrix()[2, 2] / sum(conf_matrix()), 4) * 100
    tnr <- round(conf_matrix()[1, 1] / sum(conf_matrix()), 4) * 100
    fpr <- round(conf_matrix()[1, 2] / sum(conf_matrix()[1, ]), 4) * 100
    fnr <- round(conf_matrix()[2, 1] / sum(conf_matrix()[2, ]), 4) * 100
    acc <- round(sum(conf_matrix()[1,1], conf_matrix()[2,2]) / sum(conf_matrix()), 4) * 100

  # Present results
    
    results <- rbind(auc, acc, tpr, fpr, 
                     tnr, fnr)
    results <- data.frame(results)
    colnames(results) <- c("Value")
    rownames(results) <- c("AUC Estimate",
                           "Accuracy",
                           "True Positive Rate",
                           "False Positive Rate",
                           "True Negative Rate",
                           "False Negative Rate")
    results
  })
  # Generate an HTML table view of decile wise responders
  output$table <- renderTable({
    cuml_gains()$results.table
  })
  
})


