library(rpart)
library(MASS)
data(Pima.tr)
Diabetes <- MASS::Pima.tr[,8]
X <- MASS::Pima.tr[,-8]
tree <- rpart(Diabetes~.,data=X,control = rpart.control(xval = 10))
plot(tree);text(tree)



n <- nrow(X)
subsample <- sample(1:n, n , replace=TRUE) #bootsrtap
sort(subsample)
tree_boot <- rpart(Diabetes ~ ., data=X, subset=subsample,
                   control=rpart.control(xval=10)) ## 10-fold CV
plot(tree_boot);text(tree_boot)


n <- nrow(Boston)
X <- Boston[,-14]
Y <- Boston[,14]
maxdepth<- 10 # plot the depth d = 3 and d = 5
tree <- rpart(Y ~.,data = X,
              control = rpart.control(maxdepth = maxdepth,minsplit = 2))
plot(tree,margin=.1,uniform=TRUE);text(tree,cex=1.3)



B <- 100
prediction_oob <- rep(0,length(Y)) ## vector with oob predictions
numbertrees_oob <- rep(0,length(Y)) ## how many oob trees
## for each sample ?
for (b in 1:B){ ## loop over bootstrap samples
  subsample <- sample(1:n,n,replace=TRUE) ## "in-bag" samples
  outofbag <- (1:n)[-subsample] ## "out-of-bag" samples
  ## fit tree on "in-bag" samples
  treeboot <- rpart(Y ~ ., data=X, subset=subsample,
                    control=rpart.control(maxdepth=maxdepth,minsplit=2))
  ## predict on oob-samples
  prediction_oob[outofbag] <- prediction_oob[outofbag] +
    predict(treeboot, newdata=X[outofbag,])
  numbertrees_oob[outofbag] <- numbertrees_oob[outofbag] + 1
}
## final oob-prediction is average across all "out-of-bag" trees
prediction_oob <- prediction_oob / numbertrees_oob

plot(prediction_oob, Y, xlab="PREDICTED", ylab="ACTUAL")

df<-as.data.frame(cbind(prediction_oob,Y))
ggplot(data=df,aes(prediction_oob,Y))+
  geom_point(aes(prediction_oob,Y))+
  geom_smooth(method = 'lm',formula = y ~ x, se = F)
