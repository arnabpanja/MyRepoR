suppressPackageStartupMessages(library(data.table))
library(parallel)

#set.seed(124)

fn.dataframe.parallel <- function(x, y){

n.value <- x

df1 <- data.frame(id = rep(1:n.value, 
                           each = 15), 
                  admit = rep(c(1.07, 1.07, 1.70, 3.73, 3.73, 4.20, 8.87, 11.68, 14.80, 15.67, 19.08, 23.15, 29.68, 36.03, 39.08), n.value), 
                  resp_rate = rep(c(18, 17, 18, 17, 16, 16, 16, 16, 16, 17, 16, 16, 16, 16, 16), n.value))


df.temp <- as.data.table(df1)

df.temp[, `:=`(admit = round(admit, 
                             digits = 2))][order(id, admit), rowid := 1:.N, by = id][, more_24 := ifelse(admit > 24, 1, 0)]

df1 <- as.data.frame(df.temp[order(id, rowid)])

rm(list = "df.temp")
# start parallel activity - 1

clust <- makeCluster(detectCores(), 
                     type = "PSOCK") # use FORK in linux/UNIX

clusterExport(clust, "df1", 
              envir = environment())



df1$indx <- parApply(clust, X = df1, MARGIN = 1, FUN = function(x) 
  ifelse(x[5] == 0, 1, 
         which((x[2] - df1[df1$admit < x[2] & df1$id == x[1], ]$admit) < 24)[1]))

stopCluster(clust)


# end parallel activity - 1

# start serial activity

df1_less24 <- df1[df1$more_24 == 0, ]

ls.new <- lapply(aggregate(x = df1_less24[, "resp_rate"], by = list(df1_less24$id), FUN = function(x) cumsum(x)/seq_along(x), simplify = FALSE)[, 2], as.data.frame)

for(i in seq_along(ls.new)){
  ls.new[[i]] <- cbind(rep(i, nrow(ls.new[[i]])), 1:nrow(ls.new[[i]]), ls.new[[i]])
}



df3 <- do.call(rbind, ls.new) |> setNames(c("id", "rowid", "cum_mean")) 

df3 <- merge(df1, df3, by = c("id", "rowid")) |> (\(x){x[order(x$id, x$rowid), ]})()


rm(list = c("df1_less24", "ls.new"))

# end serial activity


# start parallel activity - 2

df1_more24 <- df1[df1$more_24 == 1, ]

clust <- makeCluster(detectCores(), type = "PSOCK") # use FORK in linux/UNIX

clusterExport(clust, c("df1", "df1_more24"), envir = environment())


df1_more24$cum_mean <- parApply(clust, X = df1_more24, MARGIN = 1, FUN = function(x)
  ifelse(x[4] == 1, NA, 
         mean(df1[df1$rowid >= x[6] & df1$rowid <= (x[4]-1) & df1$id == x[1], ]$resp_rate)))

stopCluster(clust)

# end parallel activity - 2

df4 <- df1_more24[, c("id", "rowid", "admit", "resp_rate", "more_24", "indx", "cum_mean")]


df5 <- rbind(df3[, c(1:4,7)], df4[, c(1:4,7)]) |> (\(x){x[order(x$id, x$rowid), ]})()

rm(list = c("df1_more24", "df4"))


rownames(df5) <- 1:nrow(df5)



#return(list(df5[df5$id %in% sample(1:n.value, 3), ]))

return(df5[df5$id %in% sample(1:n.value, 3), ] |> (\(x){cbind("iter" = rep(y, nrow(x)), x)})())


}



ls.times <- list(iter = NULL, nvalue = NULL, time = NULL)

samples <- 100000 #sample(x = 3:50, size = 20, replace = FALSE)

df.final <- data.frame()

for(i in seq_along(samples)){
  
  
  print(paste0("Working on Iteration = ", i, " with nvalue = ", format(samples[i], scientific = FALSE)))
  
  start.time <- Sys.time()
  df.final <- rbind(fn.dataframe.parallel(samples[i], i), df.final) 
  end.time <- Sys.time()
  
  ls.times$iter[i] <- i
  ls.times$nvalue[i] <- samples[i]
  ls.times$time[i] <- round(as.numeric(difftime(end.time, 
                                                start.time, 
                                                units = "secs")), 
                            digits = 2)
  
}


ls.times |> as.data.frame()

as.data.table(df.final) |> (\(x){x[order(iter, id, rowid)]})()






