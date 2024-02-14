library(readr)
library(stringr)
library(dplyr)

# 载入scientist_pub数据，假设文件位于当前工作目录
scientist_pub <- read_csv("scientist_pub.csv") %>%
  mutate(doi = toupper(doi), title = toupper(title), journal = 
toupper(journal))

# 获取当前工作目录下的Aminer子目录中的所有CSV文件
files <- list.files(path = "Aminer", pattern = "\\.csv$", full.names = 
TRUE, recursive = TRUE)

# 定义一个函数来处理每个文件并计算准确率和召回率
process_file <- function(file_path) {
  aminer_data <- read_csv(file_path) %>%
    mutate(doi = toupper(doi), title = toupper(title), journal = 
toupper(journal))
  
  # 提取 uniqueID
  unique_ID <- str_extract(file_path, "0_[0-9]+")
  
  # 筛选scientist_pub中uniqueID对应的数据
  scientist_pub_filtered <- filter(scientist_pub, uniqueID == unique_ID)
  
  # 筛选匹配的论文数据
  matched_papers <- inner_join(aminer_data, scientist_pub_filtered, by = 
c("doi", "title", "journal"))
  
  # 计算精准度和召回率
  precision <- nrow(matched_papers) / nrow(aminer_data)
  recall <- nrow(matched_papers) / nrow(scientist_pub_filtered)
  
  return(data.frame(file_name = basename(file_path), unique_ID, precision, 
recall))
}

# 应用函数到每个文件
results <- lapply(files, process_file)

# 合并结果
final_results <- bind_rows(results)

# 保存结果到文件，文件保存在当前工作目录
write_csv(final_results, "accuracy_recall_results.csv")

# 计算整体准确率和召回率
overall_precision <- mean(final_results$precision)
overall_recall <- mean(final_results$recall)

# 打印整体准确率和召回率
print(paste("Overall Precision: ", overall_precision))
print(paste("Overall Recall: ", overall_recall))

