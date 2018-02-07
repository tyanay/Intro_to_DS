#ID 1: 201390408
#ID 2: 305634271

################################
# All packages installations:
##############
# Installations
# install.packages('corrplot')
# install.packages('caret')
# install.packages('e1071', dependencies=TRUE)
# install.packages('ggplot2')
# install.packages('class')
# install.packages('caTools')
# install.packages('ROCR')
# install.packages('Rlof')
# install.packages('randomForest')
# install.packages('rpart')
# install.packages('wSVM')
# install.packages("factoextra")
# install.packages("fpc")
# install.packages("cluster")
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# install.packages("FactoMineR")
# install.packages('rpart.plot')

# Importing
library(corrplot)
library(RColorBrewer)
library(rpart)
library(rpart.plot)
library(lattice)
library(ggplot2)
library(caret)
library(class)
library(caTools)
library(gplots)
library(ROCR)
library(Rlof)
library(randomForest)
library(e1071)
library(wSVM)
library(mlbench)
library(fpc)
library(cluster)

# 1. (15) Preprocessing  
##############

# 1.1 (0) Set a working directory, load the data file and name it 'data'.
################################
  setwd("/Users/ofirmagdaci/Downloads/Assignment2")
  data <- read.csv("census income.csv",header = TRUE)
  head(data)


# 1.2 (1) Set your random seed to 1 so that models comparisons will not be affected by randomness.
#######################
  set.seed(1)

# 1.3 (2) Replace all '?' values with NA's, then delete rows from the current dataset, 
# in order to receive a full dataset (no NA's).
#######################
  data[data == '?'] <- NA
  data <- data[rowSums(is.na(data))<1,]
  summary(data)

# 1.4 (3) Create a binary 'label' column, representing the 'income' column. '>50k' will receive a positive label.
# Make sure to remove / replace the original 'income' column.
#######################
  data$income <- factor(data$income, c(levels(data$income)), labels = c(0,1))
  summary(data$income) #using summary we validate that we assigned the labels as requested (number of values per each class)


# 1.5 (2) Remove the columns 'capital.gain' and 'fnlwgt' from the dataset.
#######################
  data <- subset(data, select=-c(capital.gain,fnlwgt))
  summary(data)

# 1.6 (4) Scale and center the numeric features (standardization - mean of 0 and a standard deviation of 1).
# Afterwards, examine the correlation between them. 
# Explain in your words the main idea behind removing highly correlated features 
# (it doesn't necessarily mean you need to get rid of features in our case). 
#######################
  for (feature in colnames(data[sapply(data, is.numeric)])) {
    data[feature] <- scale(data[feature],center=TRUE,scale=TRUE)
  }
  summary(data)   #check that all numeric features were scaled (no one missing)
  
  #correlation
  #ANSWER: removing correlated features is important because they hold the same infromation. 
    #Keeping all correlated features might overemphasize their importance in different methods, like PCA, and misguide the ML models (some of them assume independent features).
    #Therefore correlated features are redundant, and should be removed. 
    #In addition - removing correlated features is also important for curse of dimensionality. We can reduce the number of feature without losing information (or small amount of information).
  cor(data[,colnames(data[sapply(data, is.numeric)])], use="complete.obs", method="pearson")  #all.obs since we don't have any missing values
  library(corrgram)
  corrgram(data[,c(colnames(data[sapply(data, is.numeric)]))], #using blue color map
           order=NULL, lower.panel=panel.shade,
           upper.panel=panel.pts, text.panel=panel.txt,
           main="Correlation matrix")
  #no correlated features (highest number is 0.15, were correlated features have at least 0.3-0.4 (rule of thumb)) -> no need to remove features

# 1.7 (3) Split the data into a test set (30%) and a train set (70%) with respect to the label.
################################
  mask = sample.split(data$income, SplitRatio = 0.7)
  train <- data[mask,]
  test <- data[!mask,]
  paste("Rows in the entire dataset: ",nrow(data),"| In train:", nrow(train), "|  In test:",nrow(test), split="")
  x.train <- subset(train, select=-c(income))
  y.train <- subset(train, select=c(income))
  x.test <- subset(test, select=-c(income))
  y.test <- subset(test, select=c(income))

# 2. (20) Decision Tree and Random Forest
#######################

# 2.1 (6) Grow a decision tree using all the features. Predict the labels for the test set with the tree you've built.
# Generate the confusion matrix for the prediction, and plot it. Explain your findings. 
#######################
  # A) Growing a decision tree using all the features
  fit <- rpart(income ~ age+workclass+education+education.num+marital.status+occupation+relationship+race+sex+capital.loss+hours.per.week+native.country,
               data=train, method="class")
  rpart.plot(fit)
  
  # B) Predict the labels for the test set
  Prediction <- predict(fit, x.test, type = "class")
  #head of prediction to validate results
  head(Prediction)
  
  # Calculate the confusion matrix
  confusion.matrix <-confusionMatrix(Prediction, test$income)
  confusion.matrix$table
  plot(confusion.matrix$table)
  
  #specifity, sensitivity and more stats
  confusion.matrix$byClass
  #ANSWER: By the confusion Matrix we can see that we've succeeded to predict correctly most of the test (82%). 
  #By analyzing each cell in the table, we can see that the accuracy when reference set as class '1' is only 40% (Specificity). Most of the predictions set to '0'. 
  #Most of the test set contains '0' class (76%) - imbalance data. In this cases we may want to use other metrics than accuracy (predicting class 0 all the time will yield accracy = 0.76).
  
  
# 2.2 (6) Train a simple random forest of 101 trees over the training data, using all the features.
# Predict the labels for the test set with the forest you've built.
# Generate the confusion matrix for the prediction. Explain your findings (in comparison to the previous model). 
# A) Create Random forest model using all the features
  d.random.forest <- randomForest(income ~  age+workclass+education+education.num+marital.status+occupation+relationship+race+sex+
                                    capital.loss+hours.per.week+native.country,
                                  data = train, importance = TRUE, ntree = 101)
  d.random.forest
  # B) Predict the labels for the test set
  resForest <- predict(d.random.forest, x.test, type = "class")
  
  # Calculate the confusion matrix
  forest.confusion.matrix <-confusionMatrix(resForest, test$income)
  forest.confusion.matrix$table
  plot(forest.confusion.matrix$table)
  
  #specifity, sensitivity and more stats
  forest.confusion.matrix$byClass
  
  #ANSWER: By the confusion Matrix we can see that we've succeeded to predict correctly less cases than the decision tree model (accuracy 77%<82%). 
  #By analyzing each cell in the table, we can see that the accuracy when reference set as class '1' is only 11%, but accuracy for class '0' was improved from the first model (99% vs. 92%). 
  #'Sensitivity' was improved and 'Specificity' has became much worse than the decision tree.
  #Therefore, we'll try to build another model using random forest with other hyperparameters.
  
# 2.3 (5) Build another model using random forest, but this time use a regularization method - 
# choose a built in argument that can help you control the trees depth, and create grid search to find the best
# value among at least 5 different values (in terms of accuracy). 
# Explain your findings (in comparison to the previous models). 
#######################
  #Grid search by mtry parameter to 
  control <- trainControl(method="repeatedcv", number=5, repeats=2, search="grid")
  tunegrid <- expand.grid(.mtry=c(5,10,20,50,90))
  #!!! PAY ATTENTION !!!#
  ##This command takes 60 minutes to run
  rf_gridsearch <- train(income ~ age+workclass+education+education.num+marital.status+occupation+relationship+race+sex+
                           capital.loss+hours.per.week+native.country, data=train, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
  print(rf_gridsearch)
  plot(rf_gridsearch)
  
  #We can see that the best value is: 10 with accuracy of 84%. Since the accuracy of the previous models 
  ## was lower (82% and 77%), we'll choose the random forest model with 10 trees.
  
  
# 2.4 (3) Plot a feature importance chart (or charts) for the best model (from the previous question).
# Explain your findings.
#######################
  rf_out <- randomForest(income ~  age+workclass+education+education.num+marital.status+occupation+relationship+race+sex+
                           capital.loss+hours.per.week+native.country,
                         data = train, importance = TRUE, ntree = 10)
  importance(rf_out)
  #plot importances
  varImpPlot(rf_out, main="Feature importance plot")
  #ANSWER: The more the accuracy/Gini of the random forest decreases due to the exclusion (or permutation) 
  #of a single variable, the more important that variable is seemed, and therefore variables with a large mean decrease in accuracy are more important for classification of the data. 
  #1. Accuracy analysis-> the most importent features are capital.loss, marital.status and occupation.
   #The native.country feature doesn't affect on the model.
   #The other features have the same importance for the model.
  #2. Gini analysis-> the most importent features are age and marital.status.
   #occupation is still rating high, but the capital.loss doesn't.
  


# 3. (5) Transformation - In order to build some other more "numerical" models, we will convert all factorial features
# (besides the label) to be numeric. This is despite the fact that most of the features has no numerical meaning.
# In addition to the numerical transformation, apply standardization on all the columns.
# Explain the importance of scaling before modeling.
#######################
  #standardization
  x.train <- sapply(x.train,as.numeric)
  x.train <- scale(x.train)
  x.test <- sapply(x.test,as.numeric)
  x.test <- scale(x.test)
  summary(x.train)
  summary(x.test)
  
  train <- data.frame(x.train,y.train)
  test <- data.frame(x.test, y.test)
  data <- data.frame(rbind(train,test))
  summary(data)

  #scaling before modeling is important, especially for distanced based model like KNN, for models which assume specific distribution (like Naive Bayes) and more.
  #For KNN for example, a new record will be classified by its near neighbors. These neighbors are selected by distance proximity. 
  #Using non-standardized feature will cause the model to calculate the distance between records mostly by features with high values (like salary) and less by binary features for example (like gender).
  #For KNN, standardization will cause all features to be "equally important" in term of distance weights. Proximity will be defined in terms of relative distance (distance in std) and not in absolute values.
  #For other models, standardization may be requiered to meet models assumptions (like PCA which assume mean = 0) or Naive Bayes which demand standard normal distribution. 

# 4(10) KNN
#######################

# 4.1 (10) Build a KNN model using the training data, and predict the test labels, using 3 significantly different K's.
# Compare and discuss the performances of the models. 
#######################
  # train 3 different classifiers, k = 1, 5, 11
  knn_1 <- knn(train = x.train, test = x.test, cl = t(y.train),  k = 1, prob = TRUE)
  knn_5 <- knn(train = x.train, test = x.test, cl = t(y.train),  k = 5, prob = TRUE)
  knn_11 <- knn(train = x.train, test = x.test, cl = t(y.train),  k = 11, prob = TRUE)
  
  #display confusion matrices
  confusionMatrix(knn_1, test$income)$table
  confusionMatrix(knn_5, test$income)$table
  confusionMatrix(knn_11, test$income)$table
  
  #plot the confusion matrices
  plot(confusionMatrix(knn_1, test$income)$table, main="Confusion matrix - KNN (k=1)")
  plot(confusionMatrix(knn_5, test$income)$table, main="Confusion matrix - KNN (k=5)")
  plot(confusionMatrix(knn_11, test$income)$table, main="Confusion matrix - KNN (k=11)")
  
  #display accuracy for all 3 models
  paste("Accuracy k=1: ",confusionMatrix(knn_1, test$income)$overall[1],sep="")
  paste("Accuracy k=5: ",confusionMatrix(knn_5, test$income)$overall[1],sep="")
  paste("Accuracy k=11: ",confusionMatrix(knn_11, test$income)$overall[1],sep="")
  #we can see that the accuracy is higher as k is bigger
  
  #All models handled better the '0' label (the common label). We see that as k incrase, it predicts '0' more fequently. However, the TP value also incease with k.
  #The model with k=1 had the highest number of false alarams, which usually considered worse than mistake type 2 (miss). It depends on business needs. False alarams (FP) decrease with k.
  #Number of misses (FN) is declined as k increased, which may be suggest that some class '0' samples are the nearest to some class '1' samples, but when considering more neighbors, some of the neighbors are class '1'.
  #We conclude that k=13 is the best model of the 3, since its confusion matrix's values (And hence all the measures - recall, precision...) dominant over the other 2 models.
  #(also k=5 model is dominant over k=1 model)
  
  models.list = list(knn_1, knn_5, knn_11)
  aucs = list()
  perfs = list()
  legds = list()
  #build auc parts for each model
  for (i in list(1,2,3))
  {
    model <- models.list[[i]]
    # Saving the clasiffing probabilities
    model.probs <- attributes(model)$prob
    # The probabilities are given for a FALSE output. Therefore, we will subtract this probability from the value of 1.
    probs <- 1-model.probs
    #extract tpr and fpr
    model.classes <- as.vector(model)
    pred <- prediction(probs, model.classes)
    perf <- performance(pred,"tpr","fpr")
    perfs[[i]] <- perf 
    auc <- performance(pred,"auc")
    auc <- round(unlist(slot(auc, "y.values")), digits = 2)
    auc <- paste("AUC = ",auc,sep="")
    aucs[[i]] <- auc
  }
  
  #ROC plot for each model
  plot(0.0,0.0, xlim = c(0.0,1.0), ylim = c(0.0,1.0),type = "n" ,main="ROC for all k's")
  plot(perfs[[1]],main=paste("ROC for all k=",1, sep=""))
  legend(0.3, 0.6,aucs[[1]] ,border="white",cex=1,box.col = "white")
  
  plot(perfs[[2]],main=paste("ROC for all k=",5, sep=""))
  legend(0.3, 0.6,aucs[[2]] ,border="white",cex=1,box.col = "white")
  
  plot(perfs[[3]],main=paste("ROC for all k=",11, sep=""))
  legend(0.3, 0.6,aucs[[3]] ,border="white",cex=1,box.col = "white")
  
  #we can see that the AUC is highest when k=11, we again make it dominant over the others.
  #As the accuracy - the AUC is increasing with k, which make sense beaucse they are related. It's a good sanity check.
  #The model with k=1 is very bad, having auc = 0.53, only a bit over the effectivness of a random guess.
  
  y.train <- sapply(y.train, factor)
  #We'll now show a comparison using grid search, displaying all errors for different k's in the same chart
  knn.model <- tune.knn(x.train, sapply(y.train, factor), k = list(1,5,11), tunecontrol = tune.control(sampling = "cross", cross = 5))
  summary(knn.model)
  plot(knn.model, main='Performance (1-Accuracy) of the model as function of k')
  #we can see that the error is lower as k is lower. Of course it won't be true for any k and/or any dataset. Usually k=1 will make a noisy model, since the classification depends only on 1 neighbor
  #As k increased, we expect to get more larger homegenous classification areas (more smooth), without "holes" in them (like in k=1 -> we'll get many small classification areas side by side).
  #The error will may increase if we'll further increase k, due to prefering "macro" classification instead of "micro" classification (and possible proximity of samples of the 2 classes).

# 5 (30) PCA and ROC
#######################

# 5.1 (10) Find the principal components of the train set.
# Display the explained variance of each PC. 
#######################
  library(factoextra)
  #PCA to all the data (without the target of course)
  pca.res <- prcomp(subset(data, select=-c(income)), scale = TRUE, center = TRUE)
  #we'll now display the principle components and their proportion of explained variance (and also the cumulative Proportion )
  summary(pca.res)  
  #we'll plot the result in a line
  plot(pca.res, type = 'l', main = "Variance explained for each of the components")
  title(xlab="Principle components")
  
  #now a relative (variance) plot
  std_dev <- pca.res$sdev
  pr_var <- std_dev^2
  prop_varex <- pr_var/sum(pr_var)
  plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b", main = "Relative variance explained for each components")
  #another version
  library(FactoMineR)
  fviz_eig(pca.res, main="Relative variance explained for each components")
  
  #cumulative scree plot
  plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b", main = "Total fraction of variance explained VS number of components")
  #We don't see a pareto behavior of the variance explained by the PCs. 
  
  #And a nice visualization - take few minutes to run(!)
  biplot(pca.res, main="Biplot of the 2 main PCs") 
  

# 5.2 (10)
# Using the PC's that explain 70% of the variance, create (by projection of the PC's) a new trainset and testset.
# Fit a simple knn model with k=5, and display its confusion matrix.
#######################
  #We didn't see a pareto behavior of the variance explained by the PCs. 
  #Because the threshold above (70%) is quite low (many of the variance will be lost) we expect lower AUC performance in comparison to the knn with k=5 without applying PCA.
  
  #Check how many components are needed for covering 70% of the variance
  cumsum(prop_varex) #cummulative sum of %variance explained
  # -> we need the first 7 components

  data.pca.full <- data.frame(pca.res$x)
  data.pca <- data.pca.full[,1:7] #7 pc
  data.pca <- data.frame(data.pca, subset(data, select=c(income))) #add the target again
  head(data.pca) #validate results
  
  #split to train and test
  mask = sample.split(data.pca$income, SplitRatio = 0.7)
  train.pca <- data.pca[mask,]
  test.pca <- data.pca[!mask,]
  
  #train and test -> split to x (features) and y (target)
  x.train.pca <- subset(train.pca, select=-c(income))
  y.train.pca <- subset(train.pca, select=c(income))
  x.test.pca <- subset(test.pca, select=-c(income))
  y.test.pca <- subset(test.pca, select=c(income))
  knn_5.pca <- knn(train = x.train.pca, test = x.test.pca, cl = t(y.train.pca),  k = 5, prob = TRUE)
  
  #display confusion matrix
  confusionMatrix(knn_5.pca, test.pca$income)$table
  #plot confusion matrix
  plot(confusionMatrix(knn_5.pca, test$income)$table, main="Confusion matrix - KNN (k=11) with PCA")

# 5.3 (10)
# Display the ROC curve of the PCA-KNN model. 
# In general, what can we learn from a ROC chart? 
#######################
  #ROC illustrates the diagnostic ability of a binary classifier system as its discrimination threshold (probability threhold for '1' classification) is varied.
  #The y-axis is true positive rate (TPR), and the x-axis is false positive rate (FPR). The two create a trade-off.
  #The ROC define the AUC - area under the curve, which use data-scientists to compare models (the higher auc the better -> means ROC of one model is above the curve of the other). 
  #The max value of the AUC measure is 1, where the AUC values of random guess is 0.5.
  #The probability threshold to use in the in the final model is a matter of business needs. The ROC presents the model performance for any threshold with no favor to a specific value.
  
  #extract model probabilty
  model.probs <- attributes(knn_5.pca)$prob
  # The probabilities are given for a FALSE output. Therefore, we will subtract this probability from the value of 1.
  probs <- 1-model.probs
  #extract tpr and fpr
  model.classes <- as.vector(knn_5.pca)
  pred <- prediction(probs, model.classes)
  perf <- performance(pred,"tpr","fpr")
  auc <- performance(pred,"auc")
  auc <- round(unlist(slot(auc, "y.values")), digits = 2)
  auc <- paste("AUC (k=5) = ",auc,sep="")
  plot(0.0,0.0, xlim = c(0.0,1.0), ylim = c(0.0,1.0),type = "n" ,main="ROC for PCA-KNN (k=5)")
  plot(perf,main="ROC for PCA-KNN (k=5)")
  legend(0.3, 0.6, auc,border="white",cex=1,box.col = "white")
  
  #The results are bit lower than the KNN with k=5 without PCA (AUC of 0.72). It can be explained due to the use of PCA which explaines only 70% of the variance.
  #Our expectations were fulfilled.


# 6 (20) K-means
#######################

# 6.1 (10) Using the 4 most important features according to question 2.4, run k-means on the complete dataset.
# Generate only 4 clusters. Make sure number of iterations is higher than 10.
#######################
  #we'll select the features: 
    #1. marital.status (1st on gini, 2nd on accuracy)
    #2. education.num (6 on gini, 5 on accuracy)
    #3. hours.per.week (5 on gini, 4 on accuracy)
    #4. occupation (3rd on both)
  cols <- c('occupation','education.num','hours.per.week','marital.status')
  x <- data[cols] #4 most important features according to question 2.4
  kmeans.model <- kmeans(x, centers = 4, iter.max = 20) #??? number of iterations > 10?

# 6.2 (10) plot the clusters and the centers of the clusters.
# What can we learn from the chart about the clusters and the chosen number of clusters?
#######################
  #plot clusters
  plot(x, col = kmeans.model$cluster, main = "K mean clusters, where K=4")
  #points(kmeans.model$centers, col=1:4, pch=10, cex=4)
  #The centers of the clusters. Each cluster is a row in the table, with described a 4 dimentional point in space.
  print(kmeans.model$centers)
  
  #cluster plot projected to 2-d using the 2 features (components) with most variance
  clusplot(x, kmeans.model$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main="Cluster plot (4 centers)")
  # Centroid Plot against 1st 2 discriminant functions (DC)
  plotcluster(x, kmeans.model$cluster, main="Discriminant projection plot - 4 clusters")
  