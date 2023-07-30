# A Program to parse pdf files ---------------

library(stringr, warn.conflicts = FALSE)
library(pdftools, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

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
  
  n_cols <- 3
  
  page_list <- vector(mode = "list", length = n_cols)
  
  
  for(i in 0:(n_cols-1)){
    
    page_list[[i + 1]] = page_vec[(seq_along(page_vec) %% n_cols) == i]
    
  }
  
  page_df <- as.data.frame(do.call(cbind, page_list)) |> 
    (\(x) x[, c(2:n_cols, 1)])()
  
  
  page_df <- page_df |> slice(4:nrow(page_df)) |> 
    setNames(slice(page_df, 1))
  
  page_df <- as_tibble(page_df) |> mutate(Question_No = as.numeric(Question_No), 
                                          Result = str_replace_all(Result, 
                                                                   pattern = "\\\\", 
                                                                   replacement = ":"), 
                                          Marks = as.double(Marks))
  
  marks_list[[page_no]] <- page_df
  
  
}

marks_tibble <- do.call(rbind, marks_list) |> 
  as_tibble()

n_paper <- 132941

marks_tibble <- marks_tibble |> 
  mutate(paper_code = rep(n_paper, nrow(marks_tibble)), 
         subject = case_when(
           Question_No <= 45 ~ "Maths", 
           (Question_No >= 103 & Question_No <= 117) ~ "Physics", 
           (Question_No >= 145 & Question_No <= 159) ~ "Chemistry", 
           (Question_No >= 175 & Question_No <= 189) ~ "Biology", 
           TRUE ~ NA
         )) |> 
  filter(!is.na(subject)) |> 
  select(paper_code, subject, 1:n_cols)


p_sat_marks <- marks_tibble |> group_by(paper_code, subject) |> 
  summarise(cnt = n(), 
            marks_obt = sum(Marks), 
            percent_mark = round(marks_obt/cnt, digits = 2) * 100, 
            .groups = "drop") |>
  ggplot() + 
  geom_col(mapping = aes(x = percent_mark, 
                         y = reorder(subject, percent_mark), 
                         fill = subject), show.legend = FALSE) + 
  geom_text(mapping = aes(x = percent_mark, 
                          y = reorder(subject, percent_mark), 
                          label = percent_mark), 
            hjust = "right", 
            fontface = "bold", nudge_x = -0.5, color = "white") + 
  theme_bw() + 
  labs(x = "Percentage", 
       y = "Subject", 
       title = "Ashmita Panja - Class 7 - SAT - Phase 1", 
       caption = "R Programming - ggplot")



p_sat_marks

