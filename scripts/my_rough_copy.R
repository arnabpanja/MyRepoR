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

suppressPackageStartupMessages(library(dplyr))


df.x <- data.frame(a = sample(1:5, 5), 
                     b = sample(5:9, 5), 
                     c = sample(15:19, 5))

df.x

df.x |> mutate(across(where(is.numeric), 
                      .fns = scale, 
                      .names = "normed_{.col}"))
