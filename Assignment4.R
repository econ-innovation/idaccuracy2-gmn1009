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

process_file <- function(file_path) {
  aminer_data <- read_csv(file_path) %>%
    mutate(doi = toupper(doi), title = toupper(title), journal = 
toupper(journal))
  
  # uniqueID
  unique_ID <- str_extract(file_path, "0_[0-9]+")
  
  
  scientist_pub_filtered <- filter(scientist_pub, uniqueID == unique_ID)
  
  
  matched_papers <- inner_join(aminer_data, scientist_pub_filtered, by = 
c("doi", "title", "journal"))
  
  # 计算
  precision <- nrow(matched_papers) / nrow(aminer_data)
  recall <- nrow(matched_papers) / nrow(scientist_pub_filtered)
  
  return(data.frame(file_name = basename(file_path), unique_ID, precision, 
recall))
}

results <- lapply(files, process_file)

results <- bind_rows(results)

write_csv(results, "results.csv")

precision <- mean(final_results$precision)
recall <- mean(final_results$recall)



