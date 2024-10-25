install.packages("knitr")
# 設定工作目錄（可選）
setwd("C:/Users/User/Desktop/學/大學/資料探勘")

# 匯入 CSV 檔案
wine<- read.csv("winequality-red.csv")


# 創建 quality01 變數
wine$quality01 <- ifelse(wine$quality >= 6, 1, 0)

# 設定隨機種子
set.seed(1592)

# 定義 Splitdata 函數 
Splitdata <- function(data, p = 0.7) {
  index <- sample(1:nrow(data), size = p * nrow(data))
  train <- data[index, ]
  test <- data[-index, ]
  return(list(train = train, test = test))
}

# 切分資料集
data_split <- Splitdata(wine, 0.7)
train <- data_split$train
test <- data_split$test

# 敘述統計
# 定義所需的變數
variables <- c("quality01", "volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")

# 計算 train 和 test 的描述性統計
train_summary <- data.frame(
  Variable = variables,
  Min = sapply(train[, variables], min),
  `1st Qu.` = sapply(train[, variables], quantile, 0.25),
  Median = sapply(train[, variables], median),
  Mean = sapply(train[, variables], mean),
  `3rd Qu.` = sapply(train[, variables], quantile, 0.75),
  Max = sapply(train[, variables], max),
  Variance = sapply(train[, variables], var),
  SD = sapply(train[, variables], sd),
  Dataset = "Train"
)

# 計算 test 的描述性統計
test_summary <- data.frame(
  Variable = variables,
  Min = sapply(test[, variables], min),
  `1st Qu.` = sapply(test[, variables], quantile, 0.25),
  Median = sapply(test[, variables], median),
  Mean = sapply(test[, variables], mean),
  `3rd Qu.` = sapply(test[, variables], quantile, 0.75),
  Max = sapply(test[, variables], max),
  Variance = sapply(test[, variables], var),
  SD = sapply(test[, variables], sd),
  Dataset = "Test"
)



# 輸出表格
print(train_summary)
print(test_summary)
# 使用 knitr 的 kable 函數來顯示更好看的表格

library(knitr)

# 使用 kable 格式化表格輸出
kable(train_summary, caption = "Summary Statistics for Train Datasets")
kable(test_summary, caption = "Summary Statistics for Test Datasets")

par(mfrow=c(2, 2))

# 繪製 volatile.acidity 的箱型圖
boxplot(train$volatile.acidity, main="Train - Volatile Acidity", ylab="Volatile Acidity")
boxplot(test$volatile.acidity, main="Test - Volatile Acidity", ylab="Volatile Acidity")

# 繪製 total.sulfur.dioxide 的箱型圖
boxplot(train$total.sulfur.dioxide, main="Train - Total Sulfur Dioxide", ylab="Total Sulfur Dioxide")
boxplot(test$total.sulfur.dioxide, main="Test - Total Sulfur Dioxide", ylab="Total Sulfur Dioxide")

# 繪製 sulphates 的箱型圖
boxplot(train$sulphates, main="Train - Sulphates", ylab="Sulphates")
boxplot(test$sulphates, main="Test - Sulphates", ylab="Sulphates")

# 繪製 alcohol 的箱型圖
boxplot(train$alcohol, main="Train - Alcohol", ylab="Alcohol")
boxplot(test$alcohol, main="Test - Alcohol", ylab="Alcohol")

par(mfrow=c(1, 1))
# 建立模型
logit_model <- glm(quality01 ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = train, family = binomial)

# 預測訓練集
train_pred <- predict(logit_model, newdata = train, type = "response")
train_pred_class <- ifelse(train_pred > 0.5, 1, 0)

# 混淆矩陣
train_conf_matrix <- table(train$quality01, train_pred_class)

# 計算預測正確率
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)
train_conf_matrix
train_accuracy

# 預測測試集
test_pred <- predict(logit_model, newdata = test, type = "response")
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

# 混淆矩陣
test_conf_matrix <- table(test$quality01, test_pred_class)

# 計算預測正確率
test_accuracy_logit <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_conf_matrix
test_accuracy_logit

# 載入 LDA 套件
library(MASS)

# 建立 LDA 模型
lda_model <- lda(quality01 ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = train)

# 訓練集預測
train_lda_pred <- predict(lda_model, newdata = train)$class
train_lda_conf_matrix <- table(train$quality01, train_lda_pred)

# 訓練集正確率
train_lda_accuracy <- sum(diag(train_lda_conf_matrix)) / sum(train_lda_conf_matrix)
train_lda_conf_matrix
train_lda_accuracy

# 測試集預測
test_lda_pred <- predict(lda_model, newdata = test)$class
test_lda_conf_matrix <- table(test$quality01, test_lda_pred)

# 測試集正確率
test_lda_accuracy <- sum(diag(test_lda_conf_matrix)) / sum(test_lda_conf_matrix)
test_lda_conf_matrix
test_lda_accuracy

# 建立 QDA 模型
qda_model <- qda(quality01 ~ volatile.acidity + total.sulfur.dioxide + sulphates + alcohol, data = train)

# 訓練集預測
train_qda_pred <- predict(qda_model, newdata = train)$class
train_qda_conf_matrix <- table(train$quality01, train_qda_pred)

# 訓練集正確率
train_qda_accuracy <- sum(diag(train_qda_conf_matrix)) / sum(train_qda_conf_matrix)
train_qda_conf_matrix
train_qda_accuracy

# 測試集預測
test_qda_pred <- predict(qda_model, newdata = test)$class
test_qda_conf_matrix <- table(test$quality01, test_qda_pred)

# 測試集正確率
test_qda_accuracy <- sum(diag(test_qda_conf_matrix)) / sum(test_qda_conf_matrix)
test_qda_conf_matrix
test_qda_accuracy

# 載入 KNN 套件

library(class)

# 設定種子
set.seed(2631)

# K 值搜尋與交叉驗證
k_values <- 1:100
train_labels <- train$quality01
test_labels <- test$quality01

best_k <- 1
best_accuracy <- 0
# 最佳 k 值與混淆矩陣
test_knn_pred <- knn(train = train[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                     test = test[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                     cl = train_labels, k = best_k)


for (k in k_values) {
  train_knn_pred <- knn(train = train[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                        test = test[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                        cl = train_labels, k = k)
  accuracy <- sum(test_knn_pred == test_labels) / length(test_labels)
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
  }
}



knn_conf_matrix <- table(test_labels, test_knn_pred)
knn_accuracy <- sum(diag(knn_conf_matrix)) / sum(knn_conf_matrix)
best_k
knn_conf_matrix
knn_accuracy


test_accuracy_logit
test_lda_accuracy
test_qda_accuracy
knn_accuracy

# 載入 KNN 套件
library(class)

# 設定種子
set.seed(2631)

# K 值搜尋與記錄每個 K 的 Error Rate
k_values <- 1:100
error_rates <- numeric(length(k_values))

train_labels <- train$quality01
test_labels <- test$quality01

for (k in k_values) {
  test_knn_pred <- knn(train = train[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                       test = test[, c("volatile.acidity", "total.sulfur.dioxide", "sulphates", "alcohol")],
                       cl = train_labels, k = k)
  
  # 計算 Error Rate
  error_rates[k] <- mean(test_knn_pred != test_labels)
}

# 繪製 Error Rate 圖
plot(k_values, error_rates, type = "b", col = "blue", 
     xlab = "K 值", ylab = "Error Rate", 
     main = "K 值對應的 Error Rate")

