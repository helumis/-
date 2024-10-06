############ts_Final_report#####

file_path <- "C:/Users/User/Desktop/學/時間數列/AirPassengers.csv"

###讀CSV文件並檢視###
data <- read.csv(file_path)
head(data)
str(data)
summary(data)
##拆，以1949-1959預測1960年##
n=length(data$X.Passengers)
data.t <- data[1:(n-12), ]
data.p <- data[(n-12+1):n, ]
head(data.t)
tail(data.t)
data.p

########以data.t分析##############
install.packages("ggplot2")
library(ggplot2)
##########直方(90-110,110-130......)##################
ggplot(data.t, aes(x = X.Passengers)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  labs(title = "Histogram of X.Passengers in data.t",
       x = "Number of Passengers",
       y = "Frequency") +
  theme_minimal()
 
#############箱型#####################
ggplot(data.t, aes(y = X.Passengers)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of X.Passengers in data.t",
       y = "Number of Passengers") +
  theme_minimal()

###########ts圖#############
Time <- 1:nrow(data.t)
ggplot(data.t, aes(x = Time, y = X.Passengers)) +
  geom_line(color = "blue") +
  labs(title = "Time Series of X.Passengers in data.t",
       x = "Time",
       y = "Number of Passengers") +
  theme_minimal()
#######這也可以#########
ts.plot(data.t$X.Passengers)
############未差分APACF####################
par(mfrow=c(2,1))
acf(data.t$X.Passengers,60)
pacf(data.t$X.Passengers,60)
#################差分######################
data.td1=diff(data.t$X.Passengers)
data.tD12=diff(data.t$X.Passengers,12)
data.td1D12=diff(data.td1,12)

################
# 加載需要的庫
library(ggplot2)

# 計算一階差分
data.td1 <- diff(data.t$X.Passengers)

# 計算12期差分
data.tD12 <- diff(data.t$X.Passengers, lag = 12)

# 計算一階差分再做12期差分
data.td1D12 <- diff(data.td1, lag = 12)

# 創建時間序列對象
ts_data.td1 <- ts(data.td1, start = start(data.t$X.Passengers), frequency = frequency(data.t$X.Passengers))
ts_data.tD12 <- ts(data.tD12, start = start(data.t$X.Passengers) + c(0, 12), frequency = frequency(data.t$X.Passengers))
ts_data.td1D12 <- ts(data.td1D12, start = start(data.t$X.Passengers) + c(0, 12 + 1), frequency = frequency(data.t$X.Passengers))

# 繪製圖形
par(mfrow = c(3, 1)) # 設置圖形排列方式

# 繪製一階差分
plot(ts_data.td1, main = "一階差分", ylab = "差分值", xlab = "時間")

# 繪製12期差分
plot(ts_data.tD12, main = "12期差分", ylab = "差分值", xlab = "時間")

# 繪製一階差分再做12期差分
plot(ts_data.td1D12, main = "一階差分再做12期差分", ylab = "差分值", xlab = "時間")

############差分APACF######################
##########td1###############
par(mfrow=c(2,1))
acf(data.td1,60)
pacf(data.td1,60)
##########tD12##############
par(mfrow=c(2,1))
acf(data.tD12,60)
pacf(data.tD12,60)
##########td1D12############
par(mfrow=c(2,1))
acf(data.td1D12,60)
pacf(data.td1D12,60)

#########sarima傻瓜法######################
#p1###########################################################

data.fit <- arima(data$X.Passengers[1:(length(data$X.Passengers)-12)], c(1, 1, 0), seasonal = list(order = c(0, 1, 0), period = 12))
tsdiag(data.fit,60)
data.fit
names(data.fit)
data.fit$coef
data.fit$sigma2
data.fit$aic
tsdiag(data.fit,60)
AIC(data.fit)
BIC(data.fit)
data.pre <- predict(data.fit, n.ahead = 12)
names(data.pre)
U <- data.pre$pred + 1.96 * data.pre$se
L <- data.pre$pred - 1.96 * data.pre$se
plot.ts(data$X.Passengers, xlim = c(120, length(data$X.Passengers)), ylim = c(300, 700),type = "o")
lines(data.pre$pred, col = "red", type = "o")
lines(U, col = "blue", lty = "dashed")
lines(L, col = "blue", lty = "dashed")
abline(v = (length(data$X.Passengers) - 11), lty = "dotted")

################################################
#####################巨集測試###################
################################################

# 安装并加载所需的包（如果尚未安装）
if (!require("latex2exp")) install.packages("latex2exp", dependencies = TRUE)
library(latex2exp)

# 定义拟合模型、生成预测、绘制结果并存储AIC/BIC值的函数
fit_and_plot <- function(p, q, Q, data, model_name, aic_bic_table) {
  # 拟合ARIMA模型
  data.fit <- arima(data$X.Passengers[1:(length(data$X.Passengers) - 12)], 
                    order = c(p, 1, q), 
                    seasonal = list(order = c(0, 1, Q), period = 12))
  
  # 将拟合的模型分配给动态命名的变量
  assign(model_name, data.fit, envir = .GlobalEnv)
  
  # 打印模型详细信息
  cat("\nModel:", model_name, "\n")
  print(data.fit)
  
  # 将AIC和BIC添加到表中
  aic_bic_table <<- rbind(aic_bic_table, data.frame(Model = model_name, AIC = AIC(data.fit), BIC = BIC(data.fit)))
  
  # 提取系数并打印LaTeX方程式
  coef <- data.fit$coef
  ar_terms <- grep("^ar", names(coef), value = TRUE)
  ma_terms <- grep("^ma", names(coef), value = TRUE)
  sar_terms <- grep("^sar", names(coef), value = TRUE)
  sma_terms <- grep("^sma", names(coef), value = TRUE)
  
  if (length(ar_terms) > 0) {
    ar_part <- paste0(" - ", paste0(round(coef[ar_terms], 3), "B^{", seq_along(coef[ar_terms]), "}", collapse = ""))
  } else {
    ar_part <- ""
  }
  
  if (length(ma_terms) > 0) {
    ma_part <- paste0(" + ", paste0(round(coef[ma_terms], 3), "B^{", seq_along(coef[ma_terms]), "}", collapse = ""))
  } else {
    ma_part <- ""
  }
  
  if (length(sar_terms) > 0) {
    sar_part <- paste0(" - ", paste0(round(coef[sar_terms], 3), "B^{12*", seq_along(coef[sar_terms]), "}", collapse = ""))
  } else {
    sar_part <- ""
  }
  
  if (length(sma_terms) > 0) {
    sma_part <- paste0(" + ", paste0(round(coef[sma_terms], 3), "B^{12*", seq_along(coef[sma_terms]), "}", collapse = ""))
  } else {
    sma_part <- ""
  }
  
  latex_equation <- sprintf("(1%s%s)(1 - B)(1 - B^{12})X_t = (1%s%s)\\omega_t", ar_part, sar_part, ma_part, sma_part)
  cat("\nLaTeX Equation for", model_name, ":\n")
  cat(latex_equation, "\n")
  
  # 使用latex2exp显示LaTeX方程式
  par(mfrow = c(1, 1)) # 重置绘图布局
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  text(1, 1, labels = TeX(latex_equation), cex = 1.5)
  
  # 预测接下来的12期
  data.pre <- predict(data.fit, n.ahead = 12)
  U <- data.pre$pred + 1.96 * data.pre$se
  L <- data.pre$pred - 1.96 * data.pre$se
  
  # 绘制原始数据和预测
  par(mfrow = c(1, 1)) # 重置绘图布局
  plot.ts(data$X.Passengers, xlim = c(114, 150), ylim = c(300, max(data$X.Passengers, U)), type = "o", 
          main = paste("ARIMA(", p, ",1,", q, ")(0,1,", Q, ")[12]"))
  lines(data.pre$pred, col = "red", type = "o")
  lines(U, col = "blue", lty = "dashed")
  lines(L, col = "blue", lty = "dashed")
  abline(v = (length(data$X.Passengers) - 11), lty = "dotted")
  
  # 绘制诊断图
  par(mfrow = c(1, 1)) # 重置绘图布局
  tsdiag(data.fit, 60)
}

# 初始化表以存储AIC和BIC值
aic_bic_table <- data.frame(Model = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)

# 遍历p, q和Q的值并拟合模型
for (p in 0:1) {
  for (q in 0:1) {
    for (Q in 0:2) {
      if (p == 0 && q == 0 && Q == 0) next  # 跳过p = q = Q = 0的情况
      
      # 构建模型名称
      model_name <- paste0("data.fit", p, q, Q)
      
      # 拟合并绘制模型
      fit_and_plot(p, q, Q, data, model_name, aic_bic_table)
    }
  }
}

# 打印AIC和BIC表
print(aic_bic_table)

###############################
#######參數模型表##############
###############################
# 安装并加载所需的包（如果尚未安装）
if (!require("latex2exp")) install.packages("latex2exp", dependencies = TRUE)
library(latex2exp)

# 定义拟合模型、生成预测、绘制结果并存储AIC/BIC值的函数
fit_and_plot <- function(p, q, Q, data, model_name, aic_bic_table) {
  # 拟合ARIMA模型
  data.fit <- arima(data$X.Passengers[1:(length(data$X.Passengers) - 12)], 
                    order = c(p, 1, q), 
                    seasonal = list(order = c(0, 1, Q), period = 12))
  
  # 将拟合的模型分配给动态命名的变量
  assign(model_name, data.fit, envir = .GlobalEnv)
  
  # 打印模型详细信息
  cat("\nModel:", model_name, "\n")
  print(data.fit)
  
  # 将AIC和BIC添加到表中
  aic_bic_table <<- rbind(aic_bic_table, data.frame(Model = model_name, AIC = AIC(data.fit), BIC = BIC(data.fit)))
}

# 初始化表以存储AIC和BIC值
aic_bic_table <- data.frame(Model = character(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)

# 遍历p, q和Q的值并拟合模型
for (p in 0:1) {
  for (q in 0:1) {
    for (Q in 0:2) {
      if (p == 0 && q == 0 && Q == 0) next  # 跳过p = q = Q = 0的情况
      
      # 构建模型名称
      model_name <- paste0("data.fit", p, q, Q)
      
      # 拟合并绘制模型
      fit_and_plot(p, q, Q, data, model_name, aic_bic_table)
    }
  }
}

# 打印AIC和BIC表
print(aic_bic_table)

# 提取所有模型的AR、MA、SAR、SMA参数并存储在表格中
model_names <- c("data.fit001", "data.fit002", "data.fit010", "data.fit011", "data.fit012", 
                 "data.fit100", "data.fit101", "data.fit102", "data.fit110", "data.fit111", "data.fit112")

param_table <- data.frame(Model = character(), AR = character(), MA = character(), SAR = character(), SMA = character(), stringsAsFactors = FALSE)

for (model_name in model_names) {
  # 获取模型
  data.fit <- get(model_name)
  
  # 提取系数
  coef <- data.fit$coef
  ar_terms <- grep("^ar", names(coef), value = TRUE)
  ma_terms <- grep("^ma", names(coef), value = TRUE)
  sar_terms <- grep("^sar", names(coef), value = TRUE)
  sma_terms <- grep("^sma", names(coef), value = TRUE)
  
  # 生成AR、MA、SAR、SMA部分
  if (length(ar_terms) > 0) {
    ar_part <- paste0("AR: ", paste(round(coef[ar_terms], 3), collapse = ", "))
  } else {
    ar_part <- "AR: None"
  }
  
  if (length(ma_terms) > 0) {
    ma_part <- paste0("MA: ", paste(round(coef[ma_terms], 3), collapse = ", "))
  } else {
    ma_part <- "MA: None"
  }
  
  if (length(sar_terms) > 0) {
    sar_part <- paste0("SAR: ", paste(round(coef[sar_terms], 3), collapse = ", "))
  } else {
    sar_part <- "SAR: None"
  }
  
  if (length(sma_terms) > 0) {
    sma_part <- paste0("SMA: ", paste(round(coef[sma_terms], 3), collapse = ", "))
  } else {
    sma_part <- "SMA: None"
  }
  
  # 添加到参数表格
  param_table <- rbind(param_table, data.frame(Model = model_name, AR = ar_part, MA = ma_part, SAR = sar_part, SMA = sma_part))
}

# 打印参数表格
print(param_table)
##################################
data.fit001
data.fit002
data.fit010
data.fit011
data.fit012
data.fit100
data.fit101
data.fit102
data.fit110
data.fit111
data.fit112
##################################
model_name <- "data.fit100"
data.fit <- get(model_name)
data.fit
# 提取系数
coef <- data.fit$coef
ar_terms <- grep("^ar", names(coef), value = TRUE)
ma_terms <- grep("^ma", names(coef), value = TRUE)
sar_terms <- grep("^sar", names(coef), value = TRUE)
sma_terms <- grep("^sma", names(coef), value = TRUE)

if (length(ar_terms) > 0) {
  ar_part <- paste0(" - ", paste0(round(coef[ar_terms], 3), "B^{", seq_along(coef[ar_terms]), "}", collapse = ""))
} else {
  ar_part <- ""
}

if (length(ma_terms) > 0) {
  ma_part <- paste0(" + ", paste0(round(coef[ma_terms], 3), "B^{", seq_along(coef[ma_terms]), "}", collapse = ""))
} else {
  ma_part <- ""
}

if (length(sar_terms) > 0) {
  sar_part <- paste0(" - ", paste0(round(coef[sar_terms], 3), "B^{12*", seq_along(coef[sar_terms]), "}", collapse = ""))
} else {
  sar_part <- ""
}

if (length(sma_terms) > 0) {
  sma_part <- paste0(" + ", paste0(round(coef[sma_terms], 3), "B^{12*", seq_along(coef[sma_terms]), "}", collapse = ""))
} else {
  sma_part <- ""
}

latex_equation <- sprintf("(1%s%s)(1 - B)(1 - B^{12})X_t = (1%s%s)\\omega_t", ar_part, sar_part, ma_part, sma_part)
cat("\nLaTeX Equation for", model_name, ":\n")
cat(latex_equation, "\n")
par(mfrow = c(1, 1)) # 重置绘图布局
plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
text(1, 1, labels = TeX(latex_equation), cex = 1.5)