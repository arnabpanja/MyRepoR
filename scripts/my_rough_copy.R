# R for Data Science Slack Question -------
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




