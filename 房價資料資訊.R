
######################
#######packages#######
######################
install.packages("readxl")  # 用於讀取 Excel 文件
install.packages("rvest")
install.packages("httr")

library(readxl)


# 設置檔案路徑
file_A <- "C:/Users/User/Desktop/學/大學/資料探勘/T_113_3_A.csv"
file_B <- "C:/Users/User/Desktop/學/大學/資料探勘/T_113_3_B.csv"
file_C <- "C:/Users/User/Desktop/學/大學/資料探勘/T_113_3_C.csv"

# 匯入資料集
data_A <- read.csv(file_A, header = FALSE, stringsAsFactors = FALSE)
data_B <- read.csv(file_B, header = FALSE, stringsAsFactors = FALSE)
data_C <- read.csv(file_C, header = FALSE, stringsAsFactors = FALSE)



# 處理每個資料集
data_A <- process_data(data_A)
data_B <- process_data(data_B)
data_C <- process_data(data_C)


# 確保每個變數名稱不包含多餘的空白或換行符號
first_col_A <- trimws(colnames(data_A))
first_col_B <- trimws(colnames(data_B))
first_col_C <- trimws(colnames(data_C))

# 輸出變數名稱
cat("113台北Q3A 的變數名稱:\n",paste(first_col_A, collapse = "\n"), "\n\n")
cat("113台北Q3B 的變數名稱:\n",paste(first_col_B, collapse = "\n"), "\n\n")
cat("113台北Q3C 的變數名稱:\n",paste(first_col_C, collapse = "\n"), "\n")


# 加載必要的庫
library(dplyr)

# 找出共同變數
common_vars <- Reduce(intersect, list(colnames(data_A), colnames(data_B), colnames(data_C)))

# 顯示共同變數
cat("共同變數:\n", paste(common_vars, collapse = "\n"), "\n")

# 合併資料集
combined_data <- full_join(data_A, data_B, by = common_vars)  # 先合併 data_A 和 data_B
combined_data <- full_join(combined_data, data_C, by = common_vars)  # 再與 data_C 合併

# 檢查合併結果
head(combined_data)

##########################################
##########正式匯入+合併變數中英文#########
##########################################

# 引入必要套件
library(httr)

# 定義年份和季節
years <- 101:113
seasons <- c("S1", "S2", "S3", "S4")
land_types <- c("A", "B", "C")  # A_lvr_land_A, A_lvr_land_B, A_lvr_land_C

# 初始化清單以存放讀取的資料
data_list <- list()

# 遍歷年份、季節和土地類型，生成 URL
for (year in years) {
  for (season in seasons) {
    for (land_type in land_types) {
      
      # 排除不存在的檔案
      if ((year == 113 && season == "S4") || (year == 101 && season != "S4")) {
        next  # 跳過不存在的檔案
      }
      
      # 建立 URL
      file_name <- paste0(year, season, "_A_lvr_land_", land_type, ".csv")
      file_url <- paste0("https://raw.githubusercontent.com/helumis/-/main/101S4~113S3%E4%B8%8D%E5%8B%95%E7%94%A2%E3%80%81%E9%A0%90%E5%94%AE%E5%B1%8B%E3%80%81%E7%A7%9F%E8%B3%83/", file_name)
      
      # 嘗試讀取 CSV 檔案
      message("正在讀取: ", file_url)
      data <- tryCatch({
        read.csv(file_url, header = FALSE, stringsAsFactors = FALSE)  # 設定 header = FALSE 以保留所有行
      }, error = function(e) {
        message(paste("讀取檔案時出現錯誤:", file_url, "\n錯誤訊息:", e$message))
        return(NULL)  # 返回 NULL 如果出錯
      })
      
      # 將讀取的資料存入清單
      if (!is.null(data)) {
        data_list[[file_name]] <- data  # 成功讀取後加入清單
      }
    }
  }
}

# 檢查是否成功讀取資料
if (length(data_list) == 0) {
  stop("沒有成功讀取任何檔案。")
} else {
  message("所有檔案均成功讀取。")
}

# 定義函數來合併變數名稱
process_data <- function(data) {
  # 檢查資料行數是否足夠
  if (nrow(data) < 2) {
    warning("資料行數不足，無法合併變數名稱。")
    return(data)  # 返回原始資料
  }
  
  # 提取中文與英文變數名稱
  cn_names <- data[1, ]  # 第一行：中文名稱
  en_names <- data[2, ]  # 第二行：英文名稱
  
  # 檢查英文變數名稱是否為空或 NA
  if (all(is.na(en_names) | en_names == "")) {
    # 如果第二行全是空值或 NA，僅保留中文變數名稱
    combined_names <- cn_names
  } else {
    # 合併中文和英文變數名稱
    combined_names <- paste(cn_names, en_names, sep = "_")
  }
  
  # 移除前兩行，剩下的為資料
  data <- data[-c(1, 2), ]
  
  # 設置新的列名稱
  colnames(data) <- combined_names
  
  return(data)
}

# 對 data_list 中的所有資料進行處理
for (file_name in names(data_list)) {
  data_list[[file_name]] <- process_data(data_list[[file_name]])
}

##################################################################
##########+季度合併資料(不動產+預售屋+租賃)#######################
##################################################################
# 引入必要套件
library(dplyr)

# 初始化清單以存放合併後的資料
combined_data_list <- list()

# 將 data_list 轉換為 list，方便進行每三個一組的合併
data_names <- names(data_list)

# 每三個一組合併
for (i in seq(1, length(data_names), by = 3)) {
  # 提取當前三個資料集的名稱
  group_names <- data_names[i:min(i + 2, length(data_names))]
  
  # 檢查是否有三個資料集
  if (length(group_names) < 3) {
    warning("不足三個資料集，無法進行合併:", paste(group_names, collapse = ", "))
    next  # 跳過這個循環
  }
  
  # 提取資料集
  data_group <- lapply(group_names, function(name) data_list[[name]])
  
  # 確保每個資料集都存在
  if (any(sapply(data_group, is.null))) {
    warning("某些資料集不存在:", paste(group_names, collapse = ", "))
    next  # 跳過這個循環
  }
  
  # 找出共同變數
  common_vars <- Reduce(intersect, lapply(data_group, colnames))
  
  # 合併資料集，保留不同變數，遺失值填充 NA
  combined_data <- Reduce(function(x, y) full_join(x, y, by = common_vars), data_group)
  
  # 儲存合併後的資料集
  combined_data_list[[paste0("Group_", ceiling(i / 3))]] <- combined_data
  
  # 顯示合併結果的摘要
  cat("合併結果 - 群組", ceiling(i / 3), ":\n")
  print(summary(combined_data))
  cat("\n\n")
}

# 檢查合併後的資料集
if (length(combined_data_list) == 0) {
  stop("沒有成功合併任何資料集。")
} else {
  message("所有資料集均已成功合併。")
}
# 初始化一個空的清單以存放重新命名的資料集
final_combined_data_list <- list()

# 定義新的名稱列表
new_names <- c("101S4", 
  "102S1", "102S2", "102S3", "102S4", 
  "103S1", "103S2", "103S3", "103S4", 
  "104S1", "104S2", "104S3", "104S4", 
  "105S1", "105S2", "105S3", "105S4", 
  "106S1", "106S2", "106S3", "106S4", 
  "107S1", "107S2", "107S3", "107S4", 
  "108S1", "108S2", "108S3", "108S4", 
  "109S1", "109S2", "109S3", "109S4", 
  "110S1", "110S2", "110S3", "110S4", 
  "111S1", "111S2", "111S3", "111S4", 
  "112S1", "112S2", "112S3", "112S4", 
  "113S1", "113S2", "113S3"
)

# 重新命名資料集
for (i in seq_along(new_names)) {
  if (i <= length(combined_data_list)) {
    final_combined_data_list[[new_names[i]]] <- combined_data_list[[i]]
  }
}

# 確認新的資料集名稱
cat("最終合併後的資料集名稱:\n")
print(names(final_combined_data_list))

#################################################################################
##############################合併成年度資料集###################################
#################################################################################
# 加載必要的庫
library(dplyr)

# 初始化一個空的清單以存放合併後的資料
yearly_combined_data_list <- list()

# 定義年份
years <- c("101", "102", "103", "104", "105", "106", "107", "108", "109", "110", "111", "112", "113")

# 按年份合併資料集
for (year in years) {
  # 根據年份篩選資料集名稱
  group_names <- grep(paste0("^", year), names(final_combined_data_list), value = TRUE)
  
  # 提取對應的資料集
  data_group <- lapply(group_names, function(name) final_combined_data_list[[name]])
  
  # 確保每個資料集都存在
  if (any(sapply(data_group, is.null))) {
    warning("某些資料集不存在:", paste(group_names, collapse = ", "))
    next  # 跳過這個循環
  }
  
  # 找出共同變數
  common_vars <- Reduce(intersect, lapply(data_group, colnames))
  
  # 合併資料集，保留不同變數，遺失值填充 NA
  combined_data <- Reduce(function(x, y) full_join(x, y, by = common_vars), data_group)
  
  # 將合併後的資料集儲存到清單，使用年份命名
  yearly_combined_data_list[[year]] <- combined_data
  
  # 顯示合併結果的摘要
  cat("合併結果 - 年份:", year, ":\n")
  print(summary(combined_data))
  cat("\n\n")
}

# 檢查合併後的資料集
if (length(yearly_combined_data_list) == 0) {
  stop("沒有成功合併任何資料集。")
} else {
  message("所有資料集均已成功按年份合併。")
}

#################################################################################
##############################合併成單一資料集###################################
#################################################################################
# 加載必要的庫
library(dplyr)

# 確保 final_combined_data_list 存在
if (length(final_combined_data_list) == 0) {
  stop("final_combined_data_list 为空，無法合併資料。")
}

# 找出所有資料集的共同變數
common_vars <- Reduce(intersect, lapply(final_combined_data_list, colnames))

# 合併所有資料集，保留不同變數，遺失值填充 NA
merged_data <- Reduce(function(x, y) full_join(x, y, by = common_vars), final_combined_data_list)

# 顯示合併後的資料集摘要
cat("合併後的資料集摘要:\n")
print(summary(merged_data))

# 檢查合併後的資料集
if (nrow(merged_data) == 0) {
  stop("合併後的資料集為空。")
} else {
  message("所有資料集已成功合併。")
}
