##数据清洗——
getwd()
setwd('D:/')
library(xlsx)
data1 <- "D:/icu_data_extract.xls"
mydataframe <- read.xlsx(data1,1,encoding = "UTF-8")
head(mydataframe)
library(mice)
pMiss <- function(x){round(sum(is.na(x))/length(x), 3)}
apply(mydataframe, 1, pMiss)
apply(mydataframe, 2, pMiss)
table_miss <- apply(mydataframe, 2, pMiss)
write.csv(table_miss,"icu提取数据缺失值.csv")
md.pattern(mydataframe)
library(VIM)
aggr_plot <- aggr(mydataframe, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mydataframe), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
mydataframe1 <- mydataframe[, -c(2:8,12:47,50:51,56:57,60:64,69:80,83,86:118,120:121)]
summary(mydataframe1)
write.csv(mydataframe1, "mydataframe1数据清洗缺失值小于5%.csv")
mydataframe1 <- read.csv("mydataframe1数据清洗缺失值小于5%清除异常值.csv")
mydataframe1_Bi<- mydataframe1[, c(2:3,13:17,21)]
head(mydataframe1_Bi)
mydataframe1_Con <- mydataframe1[, c(4:12,18:20)]
head(mydataframe1_Con)
tempData_Bi <- mice(mydataframe1_Bi,m=5,maxit=50,meth='logreg',seed=500)
summary(tempData_Bi)
tempData_Con <- mice(mydataframe1_Con,m=5,maxit=50,meth='pmm',seed=500)
densityplot(tempData_Con)
completedData_Bi <- complete(tempData_Bi,action = 5)
summary(completedData_Bi)
completedData_Con <- complete(tempData_Con,action = 5)
summary(completedData_Con)
mydataframe2 <- cbind.data.frame(completedData_Bi,completedData_Con)
summary(mydataframe2)
write.csv(mydataframe2, "数据清洗240808.csv")

#进行原始数据拆分后的运算
mydataframe2 <- read.csv("数据清洗240808.csv")
library(tidyverse)
library(caret)
library(randomForest)
library(Amelia)
library(caretEnsemble)
library(ROCR)
library(pROC)
library(caTools)
library(corrplot)
head(mydataframe2)
str(mydataframe2)
missmap(mydataframe2)
#spearman共线性分析，绘制热力图
spearman_cor <- cor(mydataframe2[,-1], method = "spearman")
corrplot(spearman_cor, method = "color", type = "upper", order = "hclust", is.corr = FALSE, tl.cex = 1.5, cl.ratio = 0.3) # 调整标签字号和刻度线长度
mydataframe3 <- mydataframe2[,c(-1,-8,-19,-20)]
#目标变量因子化
MI <- mydataframe3 %>% mutate(myocardial_injury = as.factor(myocardial_injury))
#训练集测试集7:3
set.seed(42)
shuffle <- sample(nrow(MI))
MI_new <- MI[shuffle,]
split_train <- round(nrow(MI)*0.7)
train <- MI_new[1:split_train,]
test <- MI_new[(split_train+1):nrow(MI_new),]

#——递归特征选择
set.seed(42)
library(mlbench)
library(caret)
view(train)
#定义使用随机森林选择特征。该算法用于探索所有可能的特征子集
control<-rfeControl(functions = rfFuncs,#使用随机森林模型进行自变量的排序
                    method = "cv",#交叉验证
                    number = 5)#5折
results<-rfe(train[,1:16],#特征变量
             train[,17],#因变量
             sizes = c(1:16),#对应于应该保留的特征的数量的数值向量
             rfeControl = control)#控制选项列表，包括拟合预测的函数
results
#变量重要性图
plot(results,type = c("g","o"), grid = F)
# 查看最终最佳特征子集
best_features <- results$optVariables
print(best_features)
#gender变量被除外
train <- train[,-7]
test <- test[,-7]

#——逻辑回归模型构建
logi <- glm(myocardial_injury~., data = train, family = "binomial")
#看看整体表现
summary(logi)

logi <- glm(myocardial_injury~smoking+transfusion_plasma+transfusion_plt+transfusion_rbc+age+rr_max+rr_min+hr_max+sbp_max+t_min, data = train, family = "binomial")
#看看整体表现
summary(logi)

#LR调参
set.seed(42)
library(glmnet)
# 定义搜索空间
param_grid <- expand.grid(alpha = c(0, 0.01, 0.1, 0.5, 1), lambda = seq(0.001, 0.2, by = 0.001))
# 创建重采样描述符
train_control <- trainControl(method = "cv", number = 5, savePredictions = "final")
# 执行调参过程
tuned_model <- train(myocardial_injury ~ smoking + transfusion_plasma + transfusion_plt + transfusion_rbc + age + rr_max + rr_min + hr_max + sbp_max + t_min,
                     data = train,
                     method = "glmnet",
                     metric = "Accuracy",
                     trControl = train_control,
                     tuneGrid = param_grid)
# 查看最优超参数组合及模型性能
print(tuned_model)

best_params <- tuned_model$bestTune
print(best_params)

trellis.par.set(caretTheme())
plot(tuned_model)

# 使用最优超参数重新训练模型
logi_final <- glmnet(x = as.matrix(train[, c("smoking", "transfusion_plasma", "transfusion_plt", "transfusion_rbc", "age", "rr_max", "rr_min", "hr_max", "sbp_max", "t_min")]),
                     y = train$myocardial_injury,
                     family = "binomial",
                     alpha = best_params$alpha,
                     lambda = best_params$lambda)
# 查看最终模型的系数
coef(logi_final)

# 使用测试集进行预测
predictions_lrt <- predict(logi_final, newx = as.matrix(test[, c("smoking", "transfusion_plasma", "transfusion_plt", "transfusion_rbc", "age", "rr_max", "rr_min", "hr_max", "sbp_max", "t_min")]), type = "response")
# 将预测值转换为二进制分类结果（0或1）
predicted_classes <- ifelse(predictions_lrt > 0.5, 1, 0)
# 计算混淆矩阵和性能指标
confusion_matrix_lrt <- table(Predicted = predicted_classes, Actual = test$myocardial_injury)
print(confusion_matrix_lrt)

accuracy <- sum(diag(confusion_matrix_lrt)) / sum(confusion_matrix_lrt)
precision <- confusion_matrix_lrt[2, 2] / sum(confusion_matrix_lrt[, 2])
recall <- confusion_matrix_lrt[2, 2] / sum(confusion_matrix_lrt[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

test$predict_lr <- predict(logi_final, as.matrix(test[,-c(2,3,11,13,14,16)]), type = "response") 
head(test)
lr.roc = roc(test[,16], test$predict_lr, ci = T)
lr.roc

#绘制多条ROC曲线之一
ggroc(list(LR = lr.roc)) + labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve of Different Model') +
  theme(
    text = element_text(size = 15), # 调整字号
    axis.title = element_text(size = 15), # 调整轴标题字号
    axis.text = element_text(size = 12), # 调整轴刻度标签字号
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt") # 调整刻度线粗细
  )
lr.auc = lr.roc$auc

prediction <- predict(logi_final, as.matrix(test[, -c(2,3,11,13,14,16,17)])) # 假设your_model是您已经训练好的模型，train改为了test
# 计算ROC参数
roc_obj <- roc(test$myocardial_injury ~ prediction) #train改为了test
roc_auc <- auc(roc_obj)
#将ROC对象转换为数据库
roc_data <- data.frame(1 - roc_obj$specificities, roc_obj$sensitivities)
# 绘制ROC曲线
#简易代码：plot(roc_obj, main="ROC Curve", xlab="False Positive Rate", ylab="True Positive Rate")
ggplot(roc_data, aes(x = 1 - roc_obj$specificities, y = roc_obj$sensitivities)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUC =", round(roc_auc, 2))), size = 4, color = "black") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curve of Logistic Regression Model") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # 调整图标题字号
    text = element_text(size = 14), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 14), # 调整轴标题的字号
    axis.text = element_text(size = 12), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )


#随机森林——
library(dplyr)
library(data.table)
library(randomForest)
library(caret)
library(pROC)
library(ggplot2)
library(ggpubr)
library(ggprism)
set.seed(42)
#定义训练集特征和目标变量
X_train <- train[, c(-16)]
Y_train <- as.factor(train[, 16])
#创建RF分类模型
model <- randomForest(x = X_train, y = Y_train, ntree = 100)
#输出默认参数下的模型性能
print(model)

set.seed(42)
y_pred <- predict(model, test)
A <- as.matrix(table(y_pred, test$myocardial_injury))
acc <- sum(diag(A))/sum(A);acc  

#调参——特征数
err <- as.numeric()
for(i in 1:(ncol(train)-1)){
  set.seed(42)
  mtry_n <- randomForest(myocardial_injury~.,data=train,mtry=i)
  err <- append(err,mean(mtry_n$err.rate))
}
print(err)
mtry <- which.min(err);mtry

ctrl <- trainControl(method = "cv", number = 5) #使用五折交叉验证，也可使用十折交叉验证
#调整caret没有提供的参数
#如果我们想调整的参数caret没有提供，可以用下面的方式手动调参
#用刚刚调参的最佳mtry值固定mtry
grid <- expand.grid(mtry = c(7))
#定义模型列表，存储每一个模型评估结果
modellist <- list()
#调整的参数是决策树的数量
for(ntree in c(100,150,200,250,300)){ 
  set.seed(42)
  fit <-  train(x = X_train, y = Y_train, method = "rf",
                metric = "Accuracy", tuneGrid = grid,
                trControl = ctrl, ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
#比较结果
results <- resamples(modellist)
#输出最佳模型和参数
summary(results)

set.seed(42)
#使用最佳参数训练最终模型
final_model <- randomForest(x = X_train, y = Y_train, mtry = 7, ntree = 200, method = "rf", trControl = ctrl, tuneGrid = grid)
#输出最终模型
print(final_model)

#在测试集上进行预测
set.seed(42)
x_test <- test[, c(-16,-17)]
y_test <- as.factor(test[, 16])
test_predictions <- predict(final_model, newdata = x_test)
#计算模型指标
confusion_matrix <- confusionMatrix(test_predictions, y_test)
#准确率Accuracy
accuracy <- confusion_matrix$overall["Accuracy"]
#精确率Precision
precision <- confusion_matrix$byClass["Pos Pred Value"]
#召回率Recall
recall <- confusion_matrix$byClass["Sensitivity"]
#F1值F1-score
f1_score <- confusion_matrix$byClass["F1"]
#输出模型指标
print(confusion_matrix)
print(paste("Accuracy",accuracy))
print(paste("Precision",precision))
print(paste("Recall",recall))
print(paste("F1 Score",f1_score))

test$predict_rf <- predict(final_model, x_test, type = "prob") 
head(test)
rf.roc = roc(y_test, test$predict_rf[,2], ci = T)
rf.roc

##绘制多条ROC曲线之一
ggroc(list(LR = lr.roc, RF = rf.roc)) + labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve of Different Model') +
  theme(
    text = element_text(size = 15), # 调整字号
    axis.title = element_text(size = 15), # 调整轴标题字号
    axis.text = element_text(size = 12), # 调整轴刻度标签字号
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt") # 调整刻度线粗细
  )
lr.auc = lr.roc$auc
rf.auc = rf.roc$auc
cat('AUC of LR: ', lr.auc, '\n',
    'AUC of RF: ', rf.auc)

test_predictions_rf <- predict(final_model, newdata = x_test, type = "prob") # 假设your_model是您已经训练好的模型
# 计算ROC参数
roc_obj_rf <- roc(response = y_test, predictor = test_predictions_rf[, 2])
roc_auc_rf <- auc(roc_obj_rf)
#将ROC对象转换为数据库
roc_data_rf <- data.frame(1 - roc_obj_rf$specificities, roc_obj_rf$sensitivities)
# 绘制ROC曲线
ggplot(roc_data_rf, aes(x = 1 - roc_obj_rf$specificities, y = roc_obj_rf$sensitivities)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUC =", round(roc_auc_rf, 2))), size = 4, color = "black") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curve of Random Forest Model") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # 调整图标题字号
    text = element_text(size = 14), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 14), # 调整轴标题的字号
    axis.text = element_text(size = 12), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )


#拉索回归——
library(glmnet)
set.seed(42)
label <- as.factor(Y_train)
predict <- as.matrix(X_train)
# 使用cv.glmnet函数找到最佳λ值
cv_fit <- cv.glmnet(predict, label, family="binomial", alpha=1, nfolds=5)
dev.off()  # 关闭设备
plot(cv_fit)
lasso_fit <- glmnet(X_train, Y_train, family="binomial", alpha=1)
plot(lasso_fit, xvar = "lambda", label = T)
min_lambda <- cv_fit$lambda.min
# 使用minλ值在训练集上重新拟合LASSO模型
lasso_fit_min <- glmnet(X_train, Y_train, family="binomial", alpha=1, lambda=min_lambda)
plot(lasso_fit_min, xvar = "lambda", label = T)
# minλ在测试集上进行预测
predict_test_im <- predict(lasso_fit_min, newx=as.matrix(x_test), s=min_lambda, type="response")
# 将预测结果转换为类别标签
predicted_labels <- ifelse(predict_test_im > 0.5, "1", "0")
# 计算测试集上的准确率或其他评价指标
actual_labels <- as.numeric(y_test) - 1 # 将因子转换为数值
accuracy <- mean(predicted_labels == actual_labels)
# 计算Precision、Recall和F1分数
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(actual_labels))
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1 <- (2 * precision * recall) / (precision + recall)
print(paste("测试集上的准确率:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1))

ose_lambda <- cv_fit$lambda.1se
# 使用1seλ值在训练集上重新拟合LASSO模型
lasso_fit_1se <- glmnet(X_train, Y_train, family="binomial", alpha=1, lambda=ose_lambda)
plot(lasso_fit_1se, xvar = "lambda", label = T)
# 1seλ在测试集上进行预测
predict_test_i1 <- predict(lasso_fit_1se, newx=as.matrix(x_test), s=ose_lambda, type="response")
# 将预测结果转换为类别标签
predicted_labels <- ifelse(predict_test_i1 > 0.5, "1", "0")
# 计算测试集上的准确率或其他评价指标
actual_labels <- as.numeric(y_test) - 1 # 将因子转换为数值
accuracy <- mean(predicted_labels == actual_labels)
# 计算Precision、Recall和F1分数
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(actual_labels))
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1 <- (2 * precision * recall) / (precision + recall)
print(paste("测试集上的准确率:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1))

lasso_model_new <- lasso_fit_1se
test$predict_lasso <- predict(lasso_model_new, as.matrix(x_test), type = "response")
head(test)
lasso.roc = roc(y_test, as.numeric(test$predict_lasso), ci = T)
lasso.roc


##绘制多条ROC曲线之一
ggroc(list(LR = lr.roc, RF = rf.roc, LASSO = lasso.roc)) + labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve of Different Model') +
  theme(
    text = element_text(size = 15), # 调整字号
    axis.title = element_text(size = 15), # 调整轴标题字号
    axis.text = element_text(size = 12), # 调整轴刻度标签字号
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt") # 调整刻度线粗细
  )
lr.auc = lr.roc$auc
rf.auc = rf.roc$auc
lasso.auc = lasso.roc$auc
cat('AUC of LR: ', lr.auc, '\n',
    'AUC of RF: ', rf.auc, '\n',
    'AUC of LASSO: ', lasso.auc)

# Predict from model
preds <- predict(lasso_model_new, newx = as.matrix(x_test), type = 'response')
# 计算ROC参数
roc_obj_lasso <- roc(y_test ~ preds)
roc_auc_lasso <- auc(roc_obj_lasso)
#将ROC对象转换为数据库
roc_data_lasso <- data.frame(1 - roc_obj_lasso$specificities, roc_obj_lasso$sensitivities)
# 绘制ROC曲线
ggplot(roc_data_lasso, aes(x = 1 - roc_obj_lasso$specificities, y = roc_obj_lasso$sensitivities)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUC =", round(roc_auc_lasso, 2))), size = 4, color = "black") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curve of Lasso Regression Model") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # 调整图标题字号
    text = element_text(size = 14), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 14), # 调整轴标题的字号
    axis.text = element_text(size = 12), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )

#支持向量机SVM——
library(dplyr)
library(data.table)
library(e1071)
library(caret)
library(pROC)
library(ggplot2)
library(ggpubr)
library(ggprism)
set.seed(42)
#定义训练集特征和目标变量
X_train <- train[, c(-16)]
Y_train <- as.factor(train[, 16])
#创建并训练SVM模型
svm_model <- svm(x = X_train, y = Y_train)
summary(svm_model)

predict(svm_model)
svm.pre<-ifelse(as.numeric(predict(svm_model))>0,1,0)
n<-ifelse(svm.pre==Y_train ,1,0)
sum(n)

library(kernlab)
set.seed(42)
#参数调整
#创建参数网格
param_grid <- expand.grid(C = 2^(-5:10), sigma = 2^(-10:-1))
svm_grid_search <- function(train, param_grid) {
  results <- data.frame()
  for (i in 1:nrow(param_grid)) {
    svm_model <- ksvm(myocardial_injury ~ ., data = train, kernel = "rbfdot", C = param_grid$C[i], sigma = param_grid$sigma[i])
    predictions <- predict(svm_model, X_train)
    accuracy <- sum(predictions == Y_train) / nrow(train)
    results <- rbind(results, data.frame(C = param_grid$C[i], sigma = param_grid$sigma[i], Accuracy = accuracy))
  }
  return(results)
}
results <- svm_grid_search(train, param_grid)
print(results)

param_grid <- results[which.max(results$Accuracy), ]
print(param_grid)

set.seed(42)
svm_final_model <- ksvm(myocardial_injury ~ ., data = train, kernel = "rbfdot", C = param_grid$C, sigma = param_grid$sigma)
#计算测试集评价模型指标
test_predictions <- predict(svm_final_model, x_test)
confusionMatrix(test_predictions, y_test)

#准确率Accuracy
accuracy <- mean(test_predictions == y_test)
#精确率Precision
precision <- sum(test_predictions == "0" & y_test =="0")/sum(test_predictions == "0")
#召回率Recall
recall <- sum(test_predictions == "0" & y_test == "0")/sum(y_test == "0")
#F1值F1-score
f1_score <- 2 * precision * recall/(precision + recall)
#输出指标结果
print(paste("准确率",accuracy))
print(paste("精确率",precision))
print(paste("召回率",recall))
print(paste("F1值",f1_score))

test$predict_svm <- predict(svm_final_model, x_test, type = "response")
head(test)
svm.roc = roc(y_test, as.numeric(test$predict_svm), ci = T)
svm.roc
test$predict_svm <- predict(svm_final_model, x_test, type = "response")
head(test)
svm.roc = roc(y_test, as.numeric(test$predict_svm), ci = T)
svm.roc

##绘制多条ROC曲线之一
ggroc(list(LR = lr.roc, RF = rf.roc, LASSO = lasso.roc, SVM = svm.roc)) + labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve of Different Model') +
  theme(
    text = element_text(size = 15), # 调整字号
    axis.title = element_text(size = 15), # 调整轴标题字号
    axis.text = element_text(size = 12), # 调整轴刻度标签字号
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt") # 调整刻度线粗细
  )
lr.auc = lr.roc$auc
rf.auc = rf.roc$auc
lasso.auc = lasso.roc$auc
svm.auc = svm.roc$auc
cat('AUC of LR: ', lr.auc, '\n',
    'AUC of RF: ', rf.auc, '\n',
    'AUC of LASSO: ', lasso.auc,'\n',
    'AUC of SVM: ', svm.auc)

predictions_svm <- predict(svm_final_model, x_test) # 假设your_model是您已经训练好的模型
# 计算ROC参数
roc_obj_svm <- roc(y_test ~ as.numeric(predictions_svm))
roc_auc_svm <- auc(roc_obj_svm)
#将ROC对象转换为数据库
roc_data_svm <- data.frame(1 - roc_obj_svm$specificities, roc_obj_svm$sensitivities)
# 绘制ROC曲线
ggplot(roc_data_svm, aes(x = 1 - roc_obj_svm$specificities, y = roc_obj_svm$sensitivities)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUC =", round(roc_auc_svm, 2))), size = 4, color = "black") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curve of Support Vector Machine Model") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # 调整图标题字号
    text = element_text(size = 14), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 14), # 调整轴标题的字号
    axis.text = element_text(size = 12), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )

#XGBoost——
library(xgboost)
library(caTools)
library(caret)
library(e1071)
library(dplyr)
library(pROC)
set.seed(42)
train$smoking<-as.factor(train$smoking)
train$drinking<-as.factor(train$drinking)
train$transfusion_albutein<-as.factor(train$transfusion_albutein)
train$transfusion_plasma<-as.factor(train$transfusion_plasma)
train$transfusion_plt<-as.factor(train$transfusion_plt)
train$transfusion_rbc<-as.factor(train$transfusion_rbc)
train$age<-as.numeric(train$age)
train$rr_max<-as.numeric(train$rr_max)
train$rr_min<-as.numeric(train$rr_min)
train$hr_max<-as.numeric(train$hr_max)
train$hr_min<-as.numeric(train$hr_min)
train$sbp_max<-as.numeric(train$sbp_max)
train$sbp_min<-as.numeric(train$sbp_min)
train$t_max<-as.numeric(train$t_max)
train$t_min<-as.numeric(train$t_min)
train$myocardial_injury<-as.factor(train$myocardial_injury)
str(train)
summary(train)

train$mi_logi <- train$myocardial_injury
levels(train$mi_logi) <- c(FALSE, TRUE)
train$mi_logi <- as.logical(train$mi_logi)
y <- train[,17]
x <- train[,1:15]
x_matrix <- data.matrix(x)
TrainingSamples <- round(length(y))
training <- x_matrix
training_label <- as.numeric(y)
d_training <- xgb.DMatrix(data = training, label = training_label)
grid_label <- factor(training_label, labels = c("no", "yes"))
boost_train_cont = trainControl(method = "cv", number = 5,   verboseIter = TRUE, returnData = FALSE, returnResamp = "all", classProbs = TRUE, summaryFunction = twoClassSummary, allowParallel = TRUE)
boost_grid <- expand.grid(nrounds = c(10, 100, 200, 400, 800, 1000, 2000),eta = c(0.2, 0.1, 0.05, 0.02, 0.01, 0.001),max_depth = c(2, 3, 4, 6, 8, 10), gamma = 0, min_child_weight = 1, subsample = 1, colsample_bytree = 1)
model_train <- train(x=training, y=grid_label, trControl = boost_train_cont, tuneGrid = boost_grid, method = "xgbTree")
ggplot(model_train$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + geom_point() + theme_bw() + scale_size_continuous(guide = "none")
options(max.print = 100000)
model_train$results


params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.05, gamma=0, max_depth=3, min_child_weight=1,    subsample=1, colsample_bytree=1)
Tuned_Model <- xgboost(data = d_training, nrounds = 200, params = params)

pred_tuned <- predict(Tuned_Model, d_training)
error_tuned <- mean(as.numeric(pred_tuned > 0.5) != training_label)
print(paste("Tuned test-error=", error_tuned))
roc_tuned <- roc(training_label, pred_tuned, algorithm = 2)
plot(roc_tuned)
auc(roc_tuned)

importance_matrix <- xgb.importance(names(x_matrix), model = Tuned_Model)
importance_matrix
xgb.plot.importance(importance_matrix)
#xgb.plot.importance(importance_matrix[1:10,])

set.seed(42)
test$smoking<-as.factor(test$smoking)
test$drinking<-as.factor(test$drinking)
test$transfusion_albutein<-as.factor(test$transfusion_albutein)
test$transfusion_plasma<-as.factor(test$transfusion_plasma)
test$transfusion_plt<-as.factor(test$transfusion_plt)
test$transfusion_rbc<-as.factor(test$transfusion_rbc)
test$age<-as.numeric(test$age)
test$rr_max<-as.numeric(test$rr_max)
test$rr_min<-as.numeric(test$rr_min)
test$hr_max<-as.numeric(test$hr_max)
test$hr_min<-as.numeric(test$hr_min)
test$sbp_max<-as.numeric(test$sbp_max)
test$sbp_min<-as.numeric(test$sbp_min)
test$t_max<-as.numeric(test$t_max)
test$t_min<-as.numeric(test$t_min)
test$myocardial_injury<-as.factor(test$myocardial_injury)
str(test)
summary(test)


test$mi_logi <- test$myocardial_injury
levels(test$mi_logi) <- c(FALSE, TRUE)
test$mi_logi <- as.logical(test$mi_logi)
y1 <- test$mi_logi
x1 <- test[,1:15]
x_matrix1 <- data.matrix(x1)
validating <- x_matrix1
validating_label <- as.numeric(y1)
d_validating <- xgb.DMatrix(data = validating, label = validating_label)

validation_pred <- predict(Tuned_Model, d_validating)
roc_validation <- roc(validating_label, validation_pred, algorithm = 2, ci = T)
roc_validation


test_predictions <- predict(Tuned_Model, newdata = d_validating)
test_predictions <- ifelse(test_predictions > 0.5, 1, 0)
#计算准确率、Precision、Recall和F1分数
accuracy <- mean(test_predictions == test$myocardial_injury)
conf_matrix <- confusionMatrix(as.factor(test_predictions), as.factor(test$myocardial_injury))
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1 <- (2 * precision * recall) / (precision + recall)
print(paste("测试集准确率:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1))

test$predict_xgb <- predict(Tuned_Model, d_validating, type = "response")
head(test)
xgb.roc = roc(y_test, test$predict_xgb)
##绘制多条ROC曲线之一
ggroc(list(LR = lr.roc, RF = rf.roc, LASSO = lasso.roc, SVM = svm.roc, XGB = xgb.roc)) + 
  labs(x = 'False Positive Rate', y = 'True Positive Rate', title = 'ROC Curve of Different Model') + 
  theme_bw() + 
  geom_line(size = 1) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 15), # 调整字号
    axis.title = element_text(size = 18), # 调整轴标题字号
    axis.text = element_text(size = 16), # 调整轴刻度标签字号
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt") # 调整刻度线粗细
  )
lr.auc = lr.roc$auc
rf.auc = rf.roc$auc
lasso.auc = lasso.roc$auc
svm.auc = svm.roc$auc
xgb.auc = xgb.roc$auc
cat('AUC of LR: ', lr.auc, '\n',
    'AUC of RF: ', rf.auc, '\n',
    'AUC of LASSO: ', lasso.auc,'\n',
    'AUC of SVM: ', svm.auc,'\n',
    'AUC of XGB: ', xgb.auc)

predictions_xgb <- predict(Tuned_Model, d_validating) # 假设your_model是您已经训练好的模型
# 计算ROC参数
roc_obj_xgb <- roc(test$myocardial_injury ~ predictions_xgb)
roc_auc_xgb <- auc(roc_obj_xgb)
#将ROC对象转换为数据库
roc_data_xgb <- data.frame(1 - roc_obj_xgb$specificities, roc_obj_xgb$sensitivities)
# 绘制ROC曲线
ggplot(roc_data_xgb, aes(x = 1 - roc_obj_xgb$specificities, y = roc_obj_xgb$sensitivities)) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype = "dashed", color = "gray") +
  geom_text(aes(x = 0.8, y = 0.2, label = paste("AUC =", round(roc_auc_xgb, 2))), size = 4, color = "black") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  ggtitle("ROC Curve of XGBoost Model") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), # 调整图标题字号
    text = element_text(size = 14), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 14), # 调整轴标题的字号
    axis.text = element_text(size = 12), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.2, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(0.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )

#DCA曲线绘制
devtools::install_github("ddsjoberg/dcurves")
library(dcurves)
library(rmda)
library(ggplot2)
test_pred <- predict(logi_final, newx = as.matrix(test[, c("smoking", "transfusion_plasma", "transfusion_plt", "transfusion_rbc", "age", "rr_max", "rr_min", "hr_max", "sbp_max", "t_min")]), type = "response")
test$predict_rf <- predict(final_model, x_test, type = "response") #原始代码type = "prob"在此处需修改为"response"，否则数据长度会不同
# 假设你有一个名为test的数据框，其中包含一个名为predict_rf的列
min_value <- min(as.numeric(test$predict_rf))
max_value <- max(as.numeric(test$predict_rf))
# 打印最小值和最大值
print(paste("Minimum value:", min_value))
print(paste("Maximum value:", max_value))
# 假设你有一个名为test的数据框，其中包含一个名为predict_rf的列
test$predict_rf <- (as.numeric(test$predict_rf) - min(as.numeric(test$predict_rf))) / (max(as.numeric(test$predict_rf)) - min(as.numeric(test$predict_rf)))
# 打印缩放后的值
print(test$predict_rf)
test$predict_lasso <- predict(lasso_model_new, as.matrix(x_test), type = "response")
test$predict_svm <- predict(svm_final_model, x_test, type = "response")
# 假设你有一个名为test的数据框，其中包含一个名为predict_rf的列
min_value <- min(as.numeric(test$predict_svm))
max_value <- max(as.numeric(test$predict_svm))
# 打印最小值和最大值
print(paste("Minimum value:", min_value))
print(paste("Maximum value:", max_value))
# 假设你有一个名为test的数据框，其中包含一个名为predict_svm的列
test$predict_svm <- (as.numeric(test$predict_svm) - min(as.numeric(test$predict_svm))) / (max(as.numeric(test$predict_svm)) - min(as.numeric(test$predict_svm)))
# 打印缩放后的值
print(test$predict_svm)
test$predict_xgb <- predict(Tuned_Model, d_validating, type = "response")
test_pred_lr <- as.numeric(test_pred)
test$predict_rf <- as.numeric(test$predict_rf)
test$predict_lasso <- as.numeric(test$predict_lasso)
test$predict_svm <- as.numeric(test$predict_svm)
test$predict_xgb <- as.numeric(test$predict_xgb)
LR <- test_pred_lr
RF <- test$predict_rf
LASSO <- test$predict_lasso
SVM <- test$predict_svm
XGB <- test$predict_xgb
dcurves::dca(myocardial_injury ~ LR + RF + LASSO + SVM + XGB,
             data = test
) %>% 
  plot(smooth = T,
       show_ggplot_code = T # 显示ggplot2代码，方便大家自己调整
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )

##XGB模型可解释性
#install.packages('shapviz')
library(shapviz)
library(caret)
library(pROC)
library(tibble)
library(ROCit)
#计算shap值并绘图
shap_xgboost = shapviz(Tuned_Model, X_pred = as.matrix(training)[, c(1:(ncol(training)))])
#绘制单个样本瀑布图
sv_waterfall(shap_xgboost, row_id = 10) + # row_id指定样本
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#单个样本力图
sv_force(shap_xgboost, row_id = 10) +
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#绘制单个样本瀑布图
sv_waterfall(shap_xgboost, row_id = 1010) + #row_id指定样本
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#单个样本力图
sv_force(shap_xgboost, row_id = 1010) + 
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#绘制变量重要性蜂群图，去掉灰色背景及网格
sv_importance(shap_xgboost, kind = "beeswarm") +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#变量重要性柱状图
sv_importance(shap_xgboost) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
#单个变量依赖图(eg. hr_max)
sv_dependence(shap_xgboost, "hr_max",
              alpha = 0.5,
              size = 1.5,
              color_var = NULL) +
  theme_bw() + 
  theme(
    plot.title = element_text(size = 18, face = "bold"), # 调整图标题字号
    text = element_text(size = 15), # 调整全局文本（包括标签、标题等）的字号
    axis.title = element_text(size = 18), # 调整轴标题的字号
    axis.text = element_text(size = 16), # 调整轴刻度标签的字号
    axis.ticks.length = unit(0.5, "cm"), # 调整刻度线长度
    axis.ticks.width = unit(1.5, "pt"), # 调整刻度线粗细
    panel.grid.major = element_blank(), # 移除主网格线
    panel.grid.minor = element_blank(), # 移除次网格线
    panel.background = element_blank(), # 移除面板背景
    axis.line = element_line(colour = "black") # 设置轴线颜色为黑色
  )
# 绘制多变量相关依赖图
sv_dependence(shap_xgboost, v = c("smoking", "drinking", "hr_max", "rr_max", "age", "hr_min", "t_max", "transfusion_plasma", "sbp_max", "sbp_min", "transfusion_plt", "rr_min", "t_min", "transfusion_rbc", "transfusion_albutein"))

