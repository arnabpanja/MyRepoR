library(stringr, warn.conflicts = FALSE)


file_path = "C:/Users/lenovo/Desktop/bb.csv"

field_separator <- ";"
max_separator <- 6

# read the file data 
i_file_data = readLines(con = file_path)


# remove the empty lines 
i_file_data <- i_file_data[nchar(i_file_data) > 0]




commit_indexes <- which(str_detect(i_file_data, 
                                   pattern = "^commithash="))

list_length <- length(i_file_data) - sum(str_detect(i_file_data, 
                                                    pattern = "^commithash="))



file_list <- vector(mode = "list", 
                    length = list_length)


k <- 1

for(i in seq_along(commit_indexes)){
  
  commit_line <- str_replace(i_file_data[commit_indexes[i]], 
                                 pattern = "commithash=", 
                                 replacement = "")
  
  j <- commit_indexes[i] + 1
  
  repeat{
    
    
    if((j > length(i_file_data)) | str_detect(i_file_data[j], pattern = "^commithash=")){
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
    
    
    file_list[[k]] <- full_line
    
    k <- k + 1
    j <- j + 1
    
  }
  
  
  
}

my_updated_list <- vector(mode = "list", 
                          length = length(file_list))

for(i in seq_along(file_list)){
  
  my_updated_list[[i]] <- str_trim(str_split(file_list[[i]], 
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
             "old_file_name"))

View(my_final_df)












