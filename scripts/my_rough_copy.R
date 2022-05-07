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




