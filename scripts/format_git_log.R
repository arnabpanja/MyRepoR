# flushing out all variables from the environment --- 

rm(list = ls())


# load the libraries --------------
library(stringr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)


options(tibble.width = Inf)




# the git log file which needs to be formatted ---- 
file_path = "C:/Users/lenovo/Desktop/my_repo_r_git_log.csv"

# the field separators ---- 
field_separator <- ";"
max_separator <- 6

# read the file data ----
i_file_data = readLines(con = file_path)


# remove the empty lines ----
i_file_data <- i_file_data[nchar(i_file_data) > 0]


# find the lines with commit hash numbers ---- 
commit_indexes <- which(str_detect(i_file_data, 
                                   pattern = "^commithash="))

# the length after merging the commit hash and file name lines ---- 
reduced_length <- length(i_file_data) - sum(str_detect(i_file_data, 
                                                    pattern = "^commithash="))



reduced_file_data <- vector(mode = "character", 
                    length = reduced_length)


reduced_index <- 1

for(i in seq_along(commit_indexes)){
  
  commit_line <- str_replace(i_file_data[commit_indexes[i]], 
                                 pattern = "commithash=", 
                                 replacement = "")
  
  j <- commit_indexes[i] + 1
  
  repeat{
    
    
    if((j > length(i_file_data)) | str_detect(i_file_data[j], 
                                              pattern = "^commithash=")){
      break
    }
    
    full_line <- str_c(commit_line, 
                       str_replace_all(
                         str_replace_all(i_file_data[j], 
                                         pattern = "\\t", 
                                         replacement = field_separator), 
                         pattern = " ", 
                         replacement = "")) 
    
    sep_to_add <- rep(";", max_separator - (nchar(full_line) - nchar(str_replace_all(full_line, pattern = field_separator, replacement = ""))))
    
    full_line <- str_c(full_line, sep_to_add)
    
    
    reduced_file_data[[reduced_index]] <- full_line
    
    reduced_index <- reduced_index + 1
    j <- j + 1
    
  }
  
  
  
}

my_updated_list <- vector(mode = "list", 
                          length = length(reduced_file_data))

for(i in seq_along(reduced_file_data)){
  
  my_updated_list[[i]] <- str_trim(str_split(reduced_file_data[[i]], 
                                             pattern = field_separator)[[1]], 
                                  side = "both")
  
  
}



my_final_df <- do.call(rbind, my_updated_list) |> 
  as.data.frame() |> 
  setNames(c("commit_hash", 
             "creator", 
             "created_on", 
             "commit_message", 
             "change_type", 
             "file_name", 
             "old_file_name")) |> 
  as_tibble() |> 
  mutate(commit_hash = str_sub(commit_hash, 
                               start = 1, 
                               end = 7), 
         created_on = parse_date_time(created_on, 
                         orders = "%a %b %d %H:%M:%S %Y %z"), 
         month_year = format(created_on, "%b-%y"), 
         sort_field = as.numeric(format(created_on, "%Y%m")))


my_final_df <- my_final_df |> arrange(created_on) |> group_by(month_year, sort_field) |> 
  mutate(n = n()) |> ungroup() |> 
  distinct(month_year, n, sort_field)



p_git_act_plot <-  my_final_df |> 
  ggplot() + 
  geom_line(mapping = aes(x = reorder(month_year, sort_field), 
                         y = n, group = 1), color = "lightgreen", linewidth = 1) + 
  geom_point(mapping = aes(x = reorder(month_year, sort_field), y = n), stat = "identity", size = 2) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) + 
  scale_y_continuous(breaks = seq(0, max(my_final_df$n), 10)) + 
  labs(x = "Month - Year", 
       y = "Commits", 
       title = "Activities in the Git Repository ...")

p_git_act_plot

ggsave(filename = "plots/git_log_plots/p_git_act_plot.pdf", 
       plot = p_git_act_plot)















