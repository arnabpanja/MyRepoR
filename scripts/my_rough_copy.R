# R for Data Science Slack Question 1 -------
# How to replace NA in a matrix/data frame ---- 
# with a set of pre-defined row and column indices -----

# create the data frame ----
df.new <- data.frame(a = sample(1:5, 5), 
                     b = sample(5:9, 5), 
                     c = sample(15:19, 5))

df.new

# set the row and col indices -------
na.rows <- as.matrix(sample(x = 1:5, 
                            size = 3, 
                            replace = FALSE))
na.cols <- as.matrix(sample(x = 1:3, 
                            size = length(na.rows), 
                            replace = FALSE))

# create a matrix out of it ----
na.indices <- cbind(na.rows, na.cols)

na.indices


# use apply function ---- 
invisible(apply(X = na.indices, 
      MARGIN = 1, 
      FUN = function(x) df.new[[x[1], x[2]]] <<- NA))

# modified data frame ----
df.new 



# R for data science slack question 2---- 
# to find the proportion of positive columns in a data frame 


df.new <- data.frame(case = 1:10, 
                     a = c(seq(-5, 3, 1), NA), 
                     b = c(NA, seq(10, -6, -2)), 
                     c = c(1:5, NA, 6:9))

df.new 

cbind(df.new, prop.positive = apply(X = df.new, 
                                    MARGIN = 1, FUN = function(x) round(sum(x[-1] >= 0, 
                                                                            na.rm = TRUE)/length(na.omit(x[-1])), 
                                                      digits = 1)))




# R for data science slack question 3 ---- 
# across function .names argument not giving the column names as expected

df.x <- data.frame(a = sample(1:5, 5), 
                     b = sample(5:9, 5), 
                     c = sample(15:19, 5))

df.x

df.x |> dplyr::transmute(across(where(is.numeric), 
                      .fns = scale,     
                      .names = "normed_{.col}"))



# R for Data Science slack question 4 ------
# A question on look ups using data frames 

# Step 1: the original data frame 
df.x <- data.frame(a = c("F", "10", "A", "13", "21"), 
                   b = c(NA, "h", NA, "r", "d"), 
                   c = c(NA, "c", NA, "d", "h"))


df.x

# Step 3: filter out the look up rows (NA) from the data frame
df.lkp <- df.x[rowSums((is.na(df.x[, -1]))) == ncol(df.x[, -1]), ] |> 
  (\(x){cbind(row.ind = as.numeric(rownames(x)), x)})()

df.lkp



# Step 3: select the rest of the rows as the main rows
df.main <- df.x[-df.lkp$row.ind, ] |> 
  (\(x){cbind(row.ind = as.numeric(rownames(x)), x)})()

df.main




# Step 4: bind the look up row indices for every main row
df.main.with.lkpind <- cbind( 
                            df.main, 
                            lkp.ind = apply(X = as.data.frame(as.numeric(df.main[, 1])), 
                                            MARGIN = 1, 
                                            FUN = function(x) as.numeric(df.lkp[, 1])[max(which((as.numeric(df.lkp[, 1]) < x)))])
                               )
df.main.with.lkpind

# Step 5: join the look up data frame using look up indices and extract 1st col
merge(
     x = df.main.with.lkpind, 
     y = df.lkp, 
     by.x = "lkp.ind", 
     by.y = "row.ind") |> 
  (\(x){cbind(as.data.frame(x$a.y), x[, 3:(3+(ncol(df.x)-1))])})() |> 
  setNames(paste0("V",1:ncol(df.main)))



# R for Data Science slack question 5 ------
# A question on truncating a multiplication result to 3 decimal places

fn.trunc <- function(x, y){
  
  # store the result as a string 
  z <- as.character(format((x*y*1000)/1000, scientific = FALSE))
  
  # extract upto 3 decimal points and convert back to number
  if(any(strsplit(z, "")[[1]] == ".")){
    return(as.numeric(substr(z, 1, which(strsplit(z, "")[[1]] == ".") + 3)))
  } else return(as.numeric(z))
  
}

fn.trunc(1.128, 1.7)
fn.trunc(1.001, 1)
fn.trunc(1.099, 1.01)


# R for Data Science slack question 6 ------
# combining two data frames by rows using bind_rows 


df.x <- data.frame(A = c(3, 2, NA, NA), 
                   B = c(NA, NA, NA, 2), 
                   C = c(NA, NA, 2, 3), 
                   D = 1:4)

df.x

dplyr::bind_rows("df-A/c" = df.x |> dplyr::select(A, C) |> 
                   setNames(c("V1", "V2")), 
                 "df-B/D" = df.x |> dplyr::select(B, D) |> 
                   setNames(c("V1", "V2")), 
                 .id = "group")


# R for Data Science slack question 6 ------
# mean over a varying window 

library(dplyr, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)


df1 <- data.frame(ID = rep(1, 15), 
                  Admit_to_Perform = c(1.07, 1.07, 1.70, 3.73, 3.73, 4.20, 8.87, 11.68, 14.80, 15.67, 19.08, 23.15, 29.68, 36.03, 39.08), 
                  Resp_Rate = c(18, 17, 18, 17, 16, 16, 16, 16, 16, 17, 16, 16, 16, 16, 16))

df2 <- data.frame(ID = rep(2, 15), 
                  Admit_to_Perform = c(1.07, 1.07, 1.70, 3.73, 3.73, 4.20, 8.87, 11.68, 14.80, 15.67, 19.08, 23.15, 29.68, 36.03, 39.08), 
                  Resp_Rate = c(18, 17, 18, 16, 16, 16, 16, 18, 16, 17, 17, 16, 16, 16, 16))

df1 <- rbind(df1, df2)

df1

# create column to store when the hours > 24
# arrange the records by id and rowid
# convert it back to data frame 
df1 <- df1 |> clean_names() |> 
  group_by(id) |> arrange(admit_to_perform) |> 
  mutate(admit_to_perform = round(admit_to_perform, 
                                  digits = 2), 
         rowid = row_number(), 
         more_than_24 = ifelse(admit_to_perform > 24, 1, 0)) |> 
  ungroup() |> 
  arrange(id, rowid) |> as.data.frame()


# store the lower row index for more than 24 hours cases 

df1$ref.indx <- apply(X = df1, MARGIN = 1, FUN = function(x) 
  ifelse(x[5] == 0, 
         1, 
         min(which((x[2] - df1[df1$admit_to_perform < x[2] & df1$id == x[1], ]$admit_to_perform) < 24))))

df1

# use the lower row index to calculate running mean 

df1$run.mean <- apply(X = df1, MARGIN = 1, FUN = function(x) 
  ifelse(x[4] == 1, NA, mean(df1[df1$rowid >= x[6] & df1$rowid <= (x[4] - 1) & df1$id == x[1], ]$resp_rate)))

# the last column gives the output 

df1

# remove the temporary columns 

df1[, -c(4:6)]


# R for Data Science slack question 7 ------

library(tibble)

samp <- data.frame(a = c(1,5,2),
                   b = c(3,1,4),
                   c = c(2,0,3)
                  )

# Add column for max 
samp$max <- apply(X = samp, 
                  MARGIN = 1, 
                  FUN = function(x) max(x))

# Add column for min 
samp$min <- apply(X = samp, 
                  MARGIN = 1, 
                  FUN = function(x) min(x))

samp

# Convert columns to list with max & min alongside 
samp.list <- apply(X = samp[1:3], 
                   MARGIN = 2, 
                   FUN = function(x) 
  list(x, samp[,"max"], samp[, "min"]))



# classify column elements using this function
fn.classify <- function(x){
  list(value = x[[1]], 
       status = ifelse(x[[1]] == x[[2]], "Max", 
                       ifelse(x[[1]] == x[[3]], "Min", "None")))
}

samp.out <- lapply(X = samp.list, 
                   FUN = fn.classify)

# convert the final output with classifications to a tibble
lapply(X = samp.out, 
       FUN = function(x) as_tibble(x))


# R for Data Science Slack Question 8 --------
# generate a pivot table in R 

library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)



data <- structure(list(
  Lp = c(
    "1", "2", "3", "4", "5", "6", "7", "8",
    "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
    "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
    "31", "32"
  ), Group = structure(c(
    1L, 1L, 2L, 2L, 1L, 1L, 2L,
    2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
    2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L
  ), levels = c(
    "Radiowaves",
    "Soniccurrents"
  ), class = "factor"), Gender = structure(c(
    1L,
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L
  ), levels = c(
    "K",
    "M"
  ), class = "factor"), Time = structure(c(
    1L, 1L, 1L, 1L, 2L,
    2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L,
    2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L
  ), levels = c(
    "before_treatment",
    "two_procedures", "three_procedures", "after_treatment"
  ), class = "factor"),
  Value_measured = c(
    3, 5, 3, 7, 1, 5, 3, 5, 3, 1, 5, 7, 0,
    5, 1, 5, 2, 4, 1, 4, 2, 5, 4, 6, 2, 5, 4, 2, 2, 4, 6, 1
  )
), class = c(
  "grouped_df",
  "tbl_df", "tbl", "data.frame"
), row.names = c(NA, -32L), groups = structure(list(
  Gender = structure(c(
    1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
    2L, 2L, 2L, 2L, 2L, 2L, 2L
  ), levels = c("K", "M"), class = "factor"),
  Time = structure(c(
    1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L, 1L, 1L,
    2L, 2L, 3L, 3L, 4L, 4L
  ), levels = c(
    "before_treatment", "two_procedures",
    "three_procedures", "after_treatment"
  ), class = "factor"),
  Group = structure(c(
    1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L,
    1L, 2L, 1L, 2L, 1L, 2L
  ), levels = c("Radiowaves", "Soniccurrents"), class = "factor"), .rows = structure(list(
    1:2, 3:4, 5:6,
    7:8, 9:10, 11:12, 13:14, 15:16, 17:18, 19:20, 21:22,
    23:24, 25:26, 27:28, 29:30, 31:32
  ), ptype = integer(0), class = c(
    "vctrs_list_of",
    "vctrs_vctr", "list"
  ))
), row.names = c(NA, -16L), class = c(
  "tbl_df",
  "tbl", "data.frame"
), .drop = TRUE))


data_clean <- data |> janitor::clean_names() 

# create summary pivot table

data_trf <- data_clean |> 
  mutate(value_meaaured = as.character(value_measured)) |> 
  group_by(time, group, gender, value_measured) |> 
  summarize(cnt = n(), .groups = "drop") |> 
  pivot_wider(names_from = value_measured, 
              values_from = cnt, 
              names_prefix = "val_")



# rearrange columns 

colnames_data_trf <- str_c("val_", suppressWarnings(as.numeric(
  str_replace_all(colnames(data_trf), "val_", ""))) |> (\(x){sort(x[!is.na(x)])})())

# calculate row wise sums

data_trf_final <- data_trf |> select(1:3, all_of(colnames_data_trf)) |> rowwise() |> mutate(Sum = sum(c_across(-c(1:3)), na.rm = TRUE)) |> ungroup()


# create the last row 
tbl_extra <- tibble::tribble(
  ~time, ~group, ~gender,
  "NA", "NA", "Sum"
)


# calculate column wise sums 

data_trf_final <- bind_rows(data_trf_final, 
                            bind_cols(tbl_extra, 
                                      map_df(data_trf_final[, -c(1:3)], 
                                             .f = ~ sum(.x, na.rm = TRUE))))


data_trf_final <- data_trf_final |> 
  mutate(new_time = lag(time), 
         new_group = lag(group)) |> 
  mutate(time = case_when(
  is.na(new_time) ~ time,
  new_time == time ~ NA_character_,
  time =="NA" ~ NA_character_, 
  TRUE ~ time
),
group = case_when(
  is.na(new_group) ~ group,
  new_group == group ~ NA_character_,
  group =="NA" ~ NA_character_, 
  TRUE ~ group
)) |> select(c(1:12))


# replace column names

colnames(data_trf_final) <- str_replace_all(string = colnames(data_trf_final), 
                                            pattern = "val_", 
                                            replacement = "")

# output the table using knitr

opts <- options(knitr.kable.NA = "")


knitr::kable(data_trf_final, "simple")


# R For Data Science Slack Question 9 ------

library(readxl, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(stringr, warn.conflicts = FALSE)

# load data
# create a new column removing time-stamps
practice_data <- read_excel(path = "data/practice_data.xlsx", 
                                    sheet = "Sheet1") |> 
  janitor::clean_names() |> 
  mutate(new_date = as.Date(as.character(date), 
                        format = "%Y-%m-%d"))

# create a look up data frame with 
# distinct dates and their day numbers 
date_master <- practice_data |> 
distinct(new_date) |> arrange(new_date) |> 
  mutate(day_num = str_c("Day ", 
                          row_number()))

# left join and 
# Use "NA" in case of no match in look up
practice_output <- inner_join(x = practice_data, 
                             y = date_master, 
                             by = "new_date") |> 
  select(date, day_num, sub_county, wards)


head(practice_output, 20)


# R4DS Slack Question ---- 30th July 2023

library(dplyr, warn.conflicts = FALSE)


df <-
  data.frame(device_id = c(123,123,1234,12345, 321,321, 'aaa','bbb')
             ,activation_date = c('1/18/2022','1/18/2022','5/21/2023','6/22/2021','8/18/2022','8/18/2022', NA, NA)
             ,import_date = c('5/14/2023','3/1/2023','8/1/2023','8/1/2023','6/10/2023','1/18/2022', '1/1/2022', '1/1/2022')
             ,col4 = c(NA , NA, 1, 2, 'a','b','c','d'))

df


nrow(df)

rand_add <- sample(1:75000, size = 45000, replace = FALSE) |> as.character()


my_list <- vector(mode = "list", length = length(rand_add))

for(i in seq_along(rand_add)){
  
  my_list[[i]] <- df |> mutate(device_id = str_c(device_id, rand_add[[i]]))
  
}

my_new_df <- do.call(rbind, my_list)

nrow(my_new_df)

my_new_df

# Approach 1

print(Sys.time())

my_new_df_2 <-  my_new_df |> 
  group_by(device_id, activation_date) |> 
  mutate(activation_date = case_when(
      import_date < max(import_date) ~ NA, 
      TRUE ~ activation_date
    )) |> 
  ungroup()

head(my_new_df_2)

print(Sys.time())

# Original Approach 

print(Sys.time())

my_new_df_3 <- my_new_df  |>  
  filter(!is.na(activation_date) | is.na(activation_date)) |> 
  group_by(device_id, activation_date)  |>  
  filter(n() > 1 | n() == 1) |> 
  mutate(activation_date = case_when(import_date < max(import_date) ~ NA, 
                                     TRUE ~activation_date)) |> 
  ungroup()

head(my_new_df_3)

print(Sys.time())


all.equal(my_new_df_2, my_new_df_3)



# R4DS Slack Question - help-r-general 

library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
options(tibble.width = Inf)


df <- structure(list(order_id = 1:8, cust_id = c(1L, 1L, 2L, 2L, 3L, 
                                               3L, 4L, 5L), 
                   order_date = structure(c(18276, 18302, 18277, 18317, 18271, 
                                            18312, 18281, 18312), class = "Date"), 
                   amount = c(150L, 150L, 150L, 150L, 150L, 150L, 150L, 150L)), 
              row.names = c(NA,-8L), class = c("tbl_df", "tbl", "data.frame"))

df |>  arrange(cust_id, order_id)


# Approach 1 
# Do a join, compute the date diff 
# and filter the ones where date diff <= 30 
df |> arrange(cust_id, order_id) |> 
  left_join(y = df, 
            by = join_by(cust_id), 
            relationship = "many-to-many") |> 
  mutate(date_diff = time_length(interval(order_date.x, order_date.y), unit = "day")) |> 
  filter((date_diff <= 30) & (order_id.y > order_id.x))

# Approach 2
# Not joining but using lag function 
# to check the previous record
# calculate the date diff 
# and then filter the ones date diff <= 30
df |> arrange(cust_id, order_id) |> 
  mutate(prev_order_date = lag(order_date, 1), 
         prev_cust_id = lag(cust_id, 1)) |>  
  filter(prev_cust_id == cust_id) |>  
  mutate(date_diff = order_date - prev_order_date) |> 
  filter(date_diff <= 30) 


# DSLC Explore Wrangle Question 

library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)


# example data frame A ---------
df_a <- data.frame(id = 1:4, 
                   col_a = sample(1:10, 4, replace = FALSE), 
                   col_b = rep(x = 0, 4), 
                   col_c = sample(50:60, 4, replace = FALSE), 
                   col_d = rep(x = 0, 4))


# example data frame B ---------
df_b <- data.frame(id = 1:4, 
                   col_b = c(99, 199, 299, 399), 
                   col_d = c(499, 599, 699, 799))

# new data frame C with 0 replaced from data frame B ----   
df_c <- df_a |> bind_cols(map_df(.x = tibble(df_a$id) |> setNames("new_col_b"), 
              .f = \(x) df_b[df_b$id == x, "col_b"])) |> 
  bind_cols(map_df(.x = tibble(df_a$id) |> setNames("new_col_d"), 
                   .f = \(x) df_b[df_b$id == x, "col_d"])) |> 
  mutate(col_b = case_when(col_b == 0 ~ new_col_b, 
                              .default = col_b), 
         col_d = case_when(col_d == 0 ~ new_col_d, 
                           .default = col_d)) |> 
  select(-c("new_col_b", "new_col_d"))



df_c








