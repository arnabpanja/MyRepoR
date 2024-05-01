library(stringr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
options(tibble.width = Inf)

# flushing out all variables from the environment

rm(list = ls())


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
                               end = 7))
my_final_df

View(my_final_df)
