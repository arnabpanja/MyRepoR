# A Program to parse pdf files ---------------

library(stringr, warn.conflicts = FALSE)
library(pdftools, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(ggpmisc, warn.conflicts = FALSE)


# Parsing the SAT Exam Paper ----------

n_cols <- 3 # columns in the final data frame 
# marks per each correct answer
n_marks_sat <- 1 
n_marks_nsejs <- 3
n_marks_mat <- 1

pdf_text <- pdftools::pdf_text(pdf = "C:/Users/lenovo/Downloads/test_pdf_sat.pdf")


marks_list <- vector(mode = "list", 
                     length = length(pdf_text))


for(page_no in seq_along(pdf_text)){
  
  page_text <- str_sub(pdf_text[[page_no]], 
                           unname(str_locate_all(pdf_text[[page_no]], "Question No")[[1]][1, 1]))
  
  page_text <- str_replace_all(page_text, 
                                   pattern = "Question No", 
                                   replacement = "Question_No")
  # Negative markings have space -- remove them here 
  page_text <- str_replace_all(page_text, 
                               pattern = "- ", 
                               replacement = "-")
  
  page_text <- str_replace_all(page_text, 
                                   pattern = "\\s+", 
                                   replacement = ",")
  
  page_text <- str_replace_all(string = page_text, 
                                   pattern = "^\\,|\\,$", "")
  
  page_vec <- str_split(page_text, 
                            pattern = ",", 
                            simplify = FALSE)[[1]]
  
  
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

n_paper <- "132941-SAT" # SAT Exam Paper Code 

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

marks_tibble_agg <- marks_tibble |> group_by(paper_code, subject) |> 
  summarise(total_marks = n() * n_marks_sat, 
            marks_obt = sum(Marks), 
            percent_mark = round(marks_obt/total_marks, digits = 2) * 100, 
            .groups = "drop")


p_sat_marks <- ggplot(data = marks_tibble_agg) + 
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
       title = "SAT - Class 7 - Phase 1", 
       caption = "R Programming - ggplot")



p_sat_marks_summary <- p_sat_marks + annotate(geom = "table", 
                       x = 80, 
                       y = 7, 
                       label = list(marks_tibble_agg))

p_sat_marks_summary


# Parsing the NSEJS Exam Paper -------


pdf_text <- pdftools::pdf_text(pdf = "C:/Users/lenovo/Downloads/test_pdf_nsejs.pdf")


marks_list <- vector(mode = "list", 
                     length = length(pdf_text))


for(page_no in seq_along(pdf_text)){
  
  page_text <- str_sub(pdf_text[[page_no]], 
                       unname(str_locate_all(pdf_text[[page_no]], "Question No")[[1]][1, 1]))
  
  page_text <- str_replace_all(page_text, 
                               pattern = "Question No", 
                               replacement = "Question_No")
  # Negative markings have space -- remove them here 
  page_text <- str_replace_all(page_text, 
                               pattern = "- ", 
                               replacement = "-")
  
  page_text <- str_replace_all(page_text, 
                               pattern = "\\s+", 
                               replacement = ",")
  
  page_text <- str_replace_all(string = page_text, 
                               pattern = "^\\,|\\,$", "")
  
  page_vec <- str_split(page_text, 
                        pattern = ",", 
                        simplify = FALSE)[[1]]
  

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


n_paper <- "132942-NSEJS" # NSEJS Paper Code 

marks_tibble <- marks_tibble |> 
  mutate(paper_code = rep(n_paper, nrow(marks_tibble)), 
         subject = case_when(
           Question_No <= 15 ~ "Physics", 
           (Question_No >= 103 & Question_No <= 117) ~ "Chemistry", 
           (Question_No >= 145 & Question_No <= 159) ~ "Maths", 
           (Question_No >= 175 & Question_No <= 189) ~ "Biology", 
           TRUE ~ NA
         )) |> 
  filter(!is.na(subject)) |> 
  select(paper_code, subject, 1:n_cols)

marks_tibble_agg_nsejs <- marks_tibble |> group_by(paper_code, subject) |> 
  summarise(total_marks = n() * n_marks_nsejs, # 3 marks for each question in NSEJS  
            marks_obt = sum(Marks), 
            percent_mark = round(marks_obt/total_marks, digits = 2) * 100, 
            .groups = "drop")


# Parsing the MAT Exam Paper -------


pdf_text <- pdftools::pdf_text(pdf = "C:/Users/lenovo/Downloads/test_pdf_mat.pdf")


marks_list <- vector(mode = "list", 
                     length = length(pdf_text))


for(page_no in seq_along(pdf_text)){
  
  page_text <- str_sub(pdf_text[[page_no]], 
                       unname(str_locate_all(pdf_text[[page_no]], "Question No")[[1]][1, 1]))
  
  page_text <- str_replace_all(page_text, 
                               pattern = "Question No", 
                               replacement = "Question_No")
  # Negative markings have space -- remove them here 
  page_text <- str_replace_all(page_text, 
                               pattern = "- ", 
                               replacement = "-")
  
  page_text <- str_replace_all(page_text, 
                               pattern = "\\s+", 
                               replacement = ",")
  
  page_text <- str_replace_all(string = page_text, 
                               pattern = "^\\,|\\,$", "")
  
  page_vec <- str_split(page_text, 
                        pattern = ",", 
                        simplify = FALSE)[[1]]
  

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


n_paper <- "132940-MAT" # NSEJS Paper Code 

marks_tibble <- marks_tibble |> 
  mutate(paper_code = rep(n_paper, nrow(marks_tibble)), 
         subject = case_when(
           Question_No <= 90 ~ "MAT", 
           TRUE ~ NA
         )) |> 
  filter(!is.na(subject)) |> 
  select(paper_code, subject, 1:n_cols)

marks_tibble_agg_mat <- marks_tibble |> group_by(paper_code, subject) |> 
  summarise(total_marks = n() * n_marks_mat, # 3 marks for each question in NSEJS  
            marks_obt = sum(Marks), 
            percent_mark = round(marks_obt/total_marks, digits = 2) * 100, 
            .groups = "drop")

marks_tibble_agg <- bind_rows(marks_tibble_agg_mat, marks_tibble_agg_nsejs)



p_nsejs_mat_marks <- ggplot(data = marks_tibble_agg) + 
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
       title = "NSEJS & MAT - Class 7 - Phase 1", 
       caption = "R Programming - ggplot")



p_nsejs_mat_marks_summary <- p_nsejs_mat_marks + annotate(geom = "table", 
                                              x = 80, 
                                              y = 9, 
                                              label = list(marks_tibble_agg))

p_nsejs_mat_marks_summary



