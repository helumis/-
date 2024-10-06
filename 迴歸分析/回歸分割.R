#################
#################
install.packages("Metrics") 
install.packages("lmtest")
install.packages("car")
# 讀取CSV檔案，並指定header參數為TRUE
data <- read.csv("C:/Users/User/Desktop/學/迴歸分析/112fir第7組迴歸分析報告 - 完整506筆資料.csv", header = TRUE)

# 查看資料結構
str(data)

# 創建索引，將資料分割為90%訓練集和10%測試集
index <- sample(1:2, nrow(data), replace = TRUE, prob = c(0.9, 0.1))

# 分割資料集
train <- data[index == 1, ]
test <- data[index == 2, ]

# 寫入CSV檔案，行名設置為FALSE
write.csv(train, "train.csv", row.names = FALSE)
write.csv(test, "test.csv", row.names = FALSE)
summary(train)
nrow(train)
summary(test)
nrow(test)
#################
#################
# 安裝並載入 readxl 套件，如果尚未安裝，請先運行 install.packages("readxl")
library(readxl)

# 設定檔案路徑
file_path <- "C:/Users/User/Desktop/學/迴歸分析/112fir第7組迴歸分析報告.xlsx"

# 讀取訓練集 (工作表 "訓練集n=456")
train <- read_excel(file_path, sheet = "訓練集n=456")

# 讀取測試集 (工作表 "測試集n=50")
test <- read_excel(file_path, sheet = "測試集n=50")

# 檢視訓練集和測試集的前幾行資料
head(train)
head(test)
nrow(train)
nrow(test)
#################
#################
#使用train資料集#
#################
#################
# 建立線性回歸模型
model_raw <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + BLAC + LSTAT, data = train)

# 顯示模型摘要
summary(model_raw)

####################
##定義要刪除的行號##
####################
rows_to_remove <- c(60, 129, 135, 136, 148, 149, 150, 170, 184, 185, 193, 203, 211, 230, 259, 
                    331, 332, 333, 334, 335, 336, 338, 339, 344, 376, 378, 381, 456)

# 刪除這些行
train_cleaned <- train[-rows_to_remove, ]

# 查看刪除後的資料
head(train_cleaned)
nrow(train_cleaned)
###########################
###########################
# 建立線性回歸模型
model <- lm(MEDV ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + BLAC + LSTAT, data = train_cleaned)

# 顯示模型摘要
summary(model)
########################
###### 常態性檢定#######
########################
# QQ圖 - 檢查殘差的常態性
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Shapiro-Wilk 檢驗 - 檢查殘差的正態性
shapiro.test(residuals(model))
hist(residuals(model), breaks = 30, main = "殘差直方圖", xlab = "殘差", col = "lightblue", border = "black")

###########################
#########齊一性檢定########
###########################
# 殘差 vs 配適值圖 - 檢查同方差性
plot(fitted(model), residuals(model), 
     main = "殘差 vs. 配適值", xlab = "配適值", ylab = "殘差")
abline(h = 0, col = "red")
# Breusch-Pagan 檢驗 - 檢查殘差的同方差性
library(lmtest)
bptest(model)
#White Test#
white_test <- bptest(model, ~ fitted(model) + I(fitted(model)^2))
print(white_test)
###########################
######### 獨立性檢定#######
###########################
# Durbin-Watson 檢驗 - 檢查殘差的獨立性
dwtest(model)
#############################################
#############轉換流程final_model#############
#############################################
# Box-Cox 轉換
bc <- boxcox(model)
lambda <- bc$x[which.max(bc$y)] # 找到最優的 lambda
final_model <- lm(MEDV^lambda ~ CRIM + ZN + CHAS + NOX + RM + DIS + RAD + TAX + PTRATIO + BLAC + LSTAT, data = train_cleaned)

##################################################
##############對final_model殘差###################
##################################################
########################
###### 常態性檢定#######
########################
# QQ圖 - 檢查殘差的常態性
qqnorm(residuals(final_model))
qqline(residuals(final_model), col = "red")

# Shapiro-Wilk 檢驗 - 檢查殘差的正態性
shapiro.test(residuals(final_model))
hist(residuals(final_model), breaks = 30, main = "殘差直方圖", xlab = "殘差", col = "lightblue", border = "black")

###########################
#########齊一性檢定########
###########################
# 殘差 vs 配適值圖 - 檢查同方差性
plot(fitted(final_model), residuals(final_model), 
     main = "殘差 vs. 配適值", xlab = "配適值", ylab = "殘差")
abline(h = 0, col = "red")
# Breusch-Pagan 檢驗 - 檢查殘差的同方差性
library(lmtest)
bptest(final_model)
#White Test#
white_test <- bptest(final_model, ~ fitted(final_model) + I(fitted(final_model)^2))
print(white_test)
###########################
######### 獨立性檢定#######
###########################
# Durbin-Watson 檢驗 - 檢查殘差的獨立性
dwtest(final_model)
#####################################
######模型驗證（測試集驗證）#########
#####################################
#####Box-Cox 轉換前##################
#####################################

summary(model)
# 預測測試集上的值
predicted_test <- predict(model, newdata = test)

# 計算測試集上的均方誤差（MSE）
mse_test <- mean((test$MEDV - predicted_test)^2)
mse_test

# 顯示測試集上的預測結果與實際值的比較
results <- data.frame(Actual = test$MEDV, Predicted = predicted_test)
head(results)
########################
######計算 MAPE#########
########################
# 預測測試集的值
predicted_test <- predict(model, newdata = test)

# 計算MAPE
mape_test <- mean(abs((test$MEDV - predicted_test) / test$MEDV)) * 100
mape_test

#####################################
######模型驗證（測試集驗證）#########
#####################################
summary(final_model)
# 預測測試集上的值
predicted_test <- predict(final_model, newdata = test)

# 計算測試集上的均方誤差（MSE）
mse_test <- mean((test$MEDV - predicted_test)^2)
mse_test

# 顯示測試集上的預測結果與實際值的比較
results <- data.frame(Actual = test$MEDV, Predicted = predicted_test)
head(results)
########################
######計算 MAPE#########
########################
# 預測測試集的值
predicted_test <- predict(final_model, newdata = test)

# 計算MAPE
mape_test <- mean(abs((test$MEDV - predicted_test) / test$MEDV)) * 100
mape_test
#####################################
######模型驗證（測試集驗證）#########
#####################################
#####Box-Cox 轉換前##################
#####################################

summary(model)
# 預測測試集上的值
predicted_test <- predict(model, newdata = test)

# 計算測試集上的均方誤差（MSE）
mse_test <- mean((test$MEDV - predicted_test)^2)
mse_test

# 顯示測試集上的預測結果與實際值的比較
results <- data.frame(Actual = test$MEDV, Predicted = predicted_test)
head(results)
########################
######計算 MAPE#########
########################
# 預測測試集的值
predicted_test <- predict(model, newdata = test)

# 計算MAPE
mape_test <- mean(abs((test$MEDV - predicted_test) / test$MEDV)) * 100
mape_test


################################
################################
library(lmtest)
library(Metrics)

# 計算統計量的函數
calculate_statistics <- function(model, test_data) {
  # 預測值
  predictions <- predict(model, newdata = test_data)
  
  # 實際值
  actuals <- test_data$MEDV  # 假設因變數為 MEDV
  
  # RMSE
  rmse <- rmse(actuals, predictions)
  
  # R²
  r_squared <- summary(model)$r.squared
  
  # 調整 R²
  adjusted_r_squared <- summary(model)$adj.r.squared
  
  # 變異係數
  cv <- sd(predictions) / mean(predictions)
  
  # 計算 MAPE
  mape <- mean(abs((actuals - predictions) / actuals)) * 100
  return(c(RMSE = rmse, R2 = r_squared, Adjusted_R2 = adjusted_r_squared, CV = cv,MAPE=mape))
}

# 計算 model_raw 和 model 的統計量
stats_raw <- calculate_statistics(model_raw, test)
stats <- calculate_statistics(model, test)

# 顯示結果
print("Model Raw Statistics:")
print(stats_raw)

print("Model Statistics:")
print(stats)
