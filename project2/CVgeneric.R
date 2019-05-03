library(caret)
CVgeneric <- function(classf,features, labels, folds, loss)
{ 
  fold_i=createFolds(c(1:nrow(features)), folds, list=TRUE)
  labels=as.factor(labels)
  result=data.frame()
  for(i in 1:folds){
    test_in=fold_i[[i]]
    test_matrix=cbind(features[test_in,], labels[test_in])
    colnames(test_matrix)=c(colnames(features[test_in,]), "ex")
    train_matrix=cbind(features[-test_in, ], labels[-test_in])
    colnames(train_matrix)=c(colnames(features[-test_in, ]), "ex")
    model=classf(train_matrix, features[test_in, ])
    loss_1=loss(labels[test_in], model)
    result=rbind(result, loss_1)
  }
  mean_result=mean(result[,1])
  result=rbind(result, mean_result)
  rownames(result)=c((1:folds), 'mean')
  return(result)
}