library(readr)
library(dplyr)
library(stringr)

# 获取当前工作目录中所有CSV文件的列表
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# 为每个文件提取uniqueID并读取内容，然后将内容转化为大写
process_file <- function(file_path) {
  # 从文件名中提炼出uniqueID
  uniqueID <- str_extract(basename(file_path), "0_[0-9]+")
  
  # 读取文件
  data <- read_csv(file_path) %>%
    mutate(across(c(doi, journal, title, year), str_to_upper), # 转换为大写
           UniqueID = uniqueID) # 添加UniqueID列
  
  return(data)
}

# 使用lapply处理所有文件并合并结果
merged_data <- bind_rows(lapply(file_list, process_file))

# 将合并后的数据写入新的CSV文件
write_csv(merged_data, "merged_data.csv")

print(head(merged_data))
