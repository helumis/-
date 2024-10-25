install.packages("dplyr")
library(dplyr)

# 設定工作目錄（可選）
setwd("C:/Users/User/Desktop/學/大學/python")

# 匯入 CSV 檔案
movie_titles_and_urls <- read.csv("movie_titles_and_urls.csv")
head(movie_titles_and_urls)

directors_info <- read.csv("directors_info.csv")
head(directors_info)


movies_from_directors <- read.csv("movies_from_directors.csv")
head(movies_from_directors)



# 假設資料集為 movies_from_directors
movies_from_directors <- movies_from_directors %>%
  mutate(box_office = ifelse(category == "Movie" & !is.na(box_office) & box_office != "", 
                             box_office, NA))

