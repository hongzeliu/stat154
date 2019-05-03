

library(rattle)
rpart.new=function(training_data, tbp){
  model=rpart::rpart(formula=ex~.,data=training_data,method="class" )
  return(predict(model, tbp, type='class'))
}


multinom.new=function(training_data, tbp){
  model=multinom(data=training_data, formula = ex~., MaxNWts=100000000)
  return(predict(model, tbp))
}
zero_one =function(true_la, pred_la){
  sum(true_la==pred_la)/length(true_la)
}
library(MASS)
lda.new=function(training_data, tbp){
  model=lda(data=training_data,ex~.)
  return(predict(model, tbp)$class)
}

qda.new=function(training_data, tbp){
  model=qda(data=training_data,ex~.)
  return(predict(model, tbp)$class)
}

  