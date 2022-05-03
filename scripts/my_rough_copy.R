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

df.x |> transmute(across(where(is.numeric), 
                      .fns = scale,     
                      .names = "normed_{.col}"))



# R for Data Science slack question 4 ------

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