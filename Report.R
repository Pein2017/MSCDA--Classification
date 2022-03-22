### bank loan
data = read.csv(file.choose(),header=TRUE )
#copy = data
install.packages('ROCR')
install.packages('maptree')
install.packages('rpart.plot')
install.packages('klaR')
install.packages('adabag')
install.packages("randomForest") 
install.packages("xgboost")
install.packages('ggplot2')
install.packages('ggcorrplot')
library(ggcorrplot)
library(ggplot2)
library(xgboost)
library(randomForest)
library(adabag)
library(klaR)
library(rpart)
library(MASS)
library(ROCR)
library(maptree) 
library(rpart.plot)

data=copy


address = which( table(data$ZIP.Code)>50 )
all=c()
for (i in 1:length(address) )
{
  name = names(address[i])
  index = which(data$ZIP.Code == name)
  all = c(all,index)
}
data$ZIP.Code[-all] = 0

## set as categorical types
index=c(4,7,9,10,11,12,13)
for (i in index)
{
  data[,i] = as.factor(data[,i])
}


names(data[index])
data = data[-which(data$Experience < 0 ) , ]
Ratio = 12 * data$CCAvg/data$Income
newdata = cbind(data,Ratio)
newdata = as.data.frame(newdata  )
data = newdata
head(data)
get_auc = function(fit,data)
{
  if (class(fit)[1] == "glm")
  {prob = predict(fit , type = 'response' , newdata = data)}
  else if(   length( intersect( class(fit)[1] , c("NaiveBayes","lda","qda") ) ) > 0  )
  {
    prob = predict(fit , type = 'prob' , newdata = data)$posterior[,2]
  }
  else if ( class(fit)[1] =='boosting'   )
  {
    prob = predict(fit , type='prob' , newdata = data)$prob[,2]
  }
  else if ( class(fit)[1] == 'randomForest.formula' )
  {
    prob = predict(fit , type='prob' , newdata = data)[,2]
  }
  else if ( class(fit)[1] == "xgb.Booster")
  {
    matrix  =  model.matrix(Personal.Loan ~ . - 1, data)
    xg_train = xgb.DMatrix(data = matrix, label = as.numeric( data$Personal.Loan)-1  )
    prob = predict(fit , type='prob' , newdata = xg_train )
  }
  else
    {prob = predict(fit , type='prob' , newdata = data)[,2]}
  pred = prediction(prob, data$Personal.Loan)
  auc = performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  return (auc)
}
best_thres_predict = function(fit , data , d , cost)
{
  n= dim(data)[1]
  thres = seq((0+d),(1-d),by= d)
  wrong=c()
  result=c()
  result_fpt=c()
  output = vector(length = n)
  pred_prob = predict(fit,type='response' , newdata = data)
  pred=pred_prob
  nn=length(thres)
  for (k in thres)
  {
    index = which(pred > k)
    pred[index]=1
    pred[-index]=0
    matrix = table(pred,data$Personal.Loan)
    fnt = matrix[1,2]
    wrong_predict = sum(matrix[1,2]+matrix[2,1])
    fpt = matrix[2,1]
    result=c(result,fnt)
    result_fpt=c(result_fpt,fpt)
    wrong = c(wrong,wrong_predict)
    pred=pred_prob
  }
  index = which.min( wrong )
  best_score = wrong[index]
  cat("best threshold before is ", thres[index],"\n")
  
  
  while (  (result_fpt[index-1] - result_fpt[index]  ) < cost*(result[index] - result[index-1]+1  )  )
  {
    index = index-1
    #cat("moving ---")
    if (index<=2)
      break
  }

  best_index = index
  best_para = thres[best_index]
  index = which(pred_prob > best_para)
  output[index]=1
  cat("\n")
  cat("after shifting, best threshold is ", best_para)
  return (list(output,best_para) )
}
CVgroup=function(k,datasize , seed)
{
  set.seed(seed)
  cvlist=list()
  n=rep(1:k,ceiling(datasize/k))[1:datasize]   
  temp=sample(n,datasize)  
  x=1:k
  dataseq=1:datasize
  cvlist=lapply(x,function(x) dataseq[temp==x])  
  return(cvlist)
}

n = dim(data)[1]
#samp = sample(n , size = round(0.75*n) )
final_performance = as.data.frame( NULL )
k = 5
cvlist=CVgroup(k = k,datasize = n ,1)


for (i in 1:k)
{
  performance = as.data.frame(NULL)
  train = data[-cvlist[[i]],]
  test = data[cvlist[[i]],]
  ### logistic 
  fit_logit = glm( Personal.Loan ~. , family = binomial(link='logit') , data = train  )
  step_logit = step(fit_logit)
  cost = 2
  # best_para = best_thres_predict(fit_logit,train,0.005 , 0 )[2]
  # pred_prob = predict(fit_logit , newdata = test ,type = "response")
  # index = which(pred_prob > best_para)
  # output = pred_prob
  # output[index]=1
  # output[-index]=0
  # matrix = table(output,test$Personal.Loan)

  best_para = best_thres_predict(step_logit,train,0.05 , 2 )[2]
  pred_prob = predict(step_logit , newdata = test ,type = "response")
  index = which(pred_prob > best_para)
  output = pred_prob
  output[index]=1
  output[-index]=0
  matrix = table(output,test$Personal.Loan)
  
  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  criteria=c('false negative count' , 'accuracy' , 'auc') 
  auc = get_auc(step_logit,test)
  LR = round( c(fn,acc,auc) , 3)
  performance = as.data.frame(criteria)
  performance = cbind(performance, LR)
  #### Trees
  tree = rpart(Personal.Loan~., method = 'class' , data = train )
  summary(tree)
  printcp(tree)
  rpart.plot(tree,type=4,cex=0.5, sub="tree")
  p_tree=prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  rpart.plot(p_tree,type=4,cex=0.5, sub="tree")

  pred = predict(p_tree , type='class' , newdata=test)
  output = pred
  matrix = table(output,test$Personal.Loan)

  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  auc = get_auc(p_tree,test)
  tree = round( c(fn,acc,auc) , 3)
  performance = cbind(performance,tree)

  ### Naive Bayes
  fit_bayes=NaiveBayes(Personal.Loan~.,train)
  pre_bayes=predict(fit_bayes , test ,type = 'response')$class
  output = pre_bayes
  matrix = table(output,test$Personal.Loan)

  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  auc = get_auc(fit_bayes,test)
  naive_bayes = round( c(fn,acc,auc) , 3)
  performance = cbind(performance,naive_bayes)

  ### boosting
  bost = boosting(Personal.Loan~. , data = train ,  mfinal = 100)
  summary(bost)
  bost$importance
  get_auc(bost,test)
  pre =predict(bost, newdata = test , type='prob')

  output = pre$class
  matrix = table(output,test$Personal.Loan)
  ( round( bost$importance[order(bost$importance, decreasing = T)] , 3) )
  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  auc = get_auc(bost,test)
  boosting = round( c(fn,acc,auc) , 3)
  performance = cbind(performance,boosting)
  
  ### random forest
  rf = randomForest(Personal.Loan~., data=train,ntree=500
                    ,importance=TRUE, type=classification)
  importance(rf )
  #hist(treesize(rf))
  #plot(rf)
  pre =predict(rf, newdata = test , type='response')
  output = pre
  matrix = table(output,test$Personal.Loan)
  
  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  auc = get_auc(rf,test)
  randomforest = round( c(fn,acc,auc) , 3)
  performance = cbind(performance,randomforest)
  
  ###  XGBoost
  matrix  =  model.matrix(Personal.Loan ~ . - 1, train)
  xg_train = xgb.DMatrix(data = matrix, label = as.numeric( train$Personal.Loan)-1  )
  xgb = xgboost(data = xg_train ,max_depth=3, eta=0.5,
                 objective='binary:logistic', nround=25)



  matrix  =  model.matrix(Personal.Loan ~ . - 1, test)
  xg_train = xgb.DMatrix(data = matrix, label = as.numeric( test$Personal.Loan)-1  )
  pre  = predict(xgb, matrix, type = 'prob'  )
  index = which(pre>0.5)
  pre[index] = 1
  pre[-index] = 0
  output = pre
  matrix = table(output,test$Personal.Loan)
  get_auc(xgb,test)
  fn = matrix[1,2]
  acc = (matrix[1,1] + matrix[2,2]) / dim(test)[1]
  auc = get_auc(xgb,test)
  xgboost =round( c(fn,acc,auc) , 3)
  performance = cbind(performance,xgboost)
  if ( i == 1 )
  {
    final_performance = performance
  }else
  {
    for (j in 2:dim(performance)[2] )
    {
      final_performance[,j] = final_performance[,j] + performance[,j]
    }
  }
}

for (i in 2:dim(performance)[2] )
{
  final_performance[,i] = final_performance[,i]/k
}
final_performance[1,-1] = round(final_performance[1,-1])
####  plot
Personal.loan = data$Personal.Loan
p = ggplot(data,aes(Age,Income) ) + geom_point(aes(colour=Personal.loan ) , )  
UG.c=transform(data,Income=Income*1)
p %+%UG.c + labs(title = "  Age VS Annual income" )

p = ggplot(data,aes(CCAvg,Family) ) + geom_point(aes(colour=Personal.loan ))  
UG.c=transform(data,Income=Income*1)
p %+%UG.c + labs(title = "Monthly credit speding VS Family size" )


per = paste(round(100 * summary(data$Personal.Loan) / n,2),"%")
pie(summary(data$Personal.Loan) , label = per , col = rainbow(2) , main ='pie chart for Y')
legend('topright', c('did not accept','accepted') ,fill=rainbow(2) )
### importance plot
imp_bost = ( round( bost$importance[order(bost$importance, decreasing = T)] , 3) )
imp_bost = imp_bost/sum(imp_bost)
barplot(imp_bost[1:5],ylab = 'Importance' , main = 'boosting' , col = 2)

imp_rf =  round(importance(rf)[order( importance(rf)[,4]  , decreasing = T) , ]  ,3  )
imp_rf = imp_rf/sum(imp_rf)
barplot(imp_rf[1:5,4],ylab = 'Importance' , main = 'random forest',col=3)

imp_xgb = xgb.importance( colnames(xg_train) , model = xgb)
xgb.plot.importance(imp_xgb[1:5,],main = 'xgboosting')




### tree plots
par(mfrow=c(1,2),main='tree plot of different CV set')
for (i in c(2,4) )
{
  train = data[-cvlist[[i]],]
  test = data[cvlist[[i]],]
  tree = rpart(Personal.Loan~., method = 'class' , data = train )
  p_tree=prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
  rpart.plot(p_tree,type=4,cex=0.6, main = paste("tree plot of CV set",i))
}






