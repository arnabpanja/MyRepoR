library(stringr, warn.conflicts = FALSE)
library(pdftools, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)


pdf_text <- pdftools::pdf_text(pdf = "C:/Users/lenovo/Downloads/test_pdf.pdf")


marks_list <- vector(mode = "list", 
                     length = length(pdf_text))

# Parsing the 1st page ----------

for(page_no in seq_along(pdf_text)){
  
  page_text <- str_sub(pdf_text[[page_no]], 
                           unname(str_locate_all(pdf_text[[page_no]], "Question No")[[1]][1, 1]))
  
  page_text <- str_replace_all(page_text, 
                                   pattern = "Question No", 
                                   replacement = "Question_No")
  
  page_text <- str_replace_all(page_text, 
                                   pattern = "\\s+", 
                                   replacement = ",")
  
  page_text <- str_replace_all(string = page_text, 
                                   pattern = "^\\,|\\,$", "")
  
  page_vec <- str_split(page_text, 
                            pattern = ",", 
                            simplify = FALSE)[[1]]
  
  page_list <- vector(mode = "list", length = 3)
  
  
  for(i in 0:2){
    
    page_list[[i + 1]] = page_vec[(seq_along(page_vec) %% 3) == i]
    
  }
  
  page_df <- as.data.frame(do.call(cbind, page_list)) |> 
    (\(x) x[, c(2:3, 1)])()
  
  
  page_df <- page_df |> slice(4:nrow(page_df)) |> 
    setNames(slice(page_df, 1))
  
  page_df <- as_tibble(page_df) |> mutate(Question_No = as.numeric(Question_No), 
                                          Result = str_replace_all(Result, 
                                                                   pattern = "\\\\", 
                                                                   replacement = ":"), 
                                          Marks = as.double(Marks))
  
  marks_list[[page_no]] <- page_df
  
  
}

marks_tibble <- do.call(rbind, marks_list)

marks_tibble$paper_code <- rep(132941, nrow(marks_tibble))

marks_tibble <- marks_tibble |> select(4, 1:3)

View(marks_tibble)



















