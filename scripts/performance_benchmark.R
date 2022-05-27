library(data.table)

perf.test <- function(){
  
  dt1 <- data.table(id = rep(1:15, 
                             each = 1000), 
                    admit = rep(c(1.07, 1.07, 1.70, 3.73, 3.73, 4.20, 8.87, 11.68, 14.80, 15.67, 19.08, 23.15, 29.68, 36.03, 39.08), 
                                1000), 
                    resp_rate = rep(c(18, 17, 18, 17, 16, 16, 16, 16, 16, 17, 16, 16, 16, 16, 16), 
                                    1000)) 
  
  
  dt3 <- dt1
  
  dt.new <- cbind(dt3, 
                  dt3[, .(rowid = 1:.N, more_24 = ifelse(admit > 24, 1, 0)), 
                      by = id])
  
  
  
  fn.index <- function(x.id, y.rowid, z.admit){ 
    
    return(which((z.admit - dt.new[id == x.id & rowid < y.rowid, admit]) < 24)[1])
    
    
  }
  
  fn.runmean <- function(x.id, y.rowid, z.ref.indx){
    
    mean(dt.new[id == x.id & rowid >= z.ref.indx & rowid < y.rowid, resp_rate])
    
  }
  
  
  dt.new[order(id, rowid), ref_indx := .(fn.index(id, rowid, admit)), by = c("id", "rowid")]
  
  dt.new[order(id, rowid), run_mean := ifelse(rowid == 1, NaN, .(fn.runmean(id, rowid, ref_indx))), by = c("id", "rowid")]
  
}


ls.times <- list(iter = NULL, time = NULL)


for(i in 1:5){
  
  
  print(paste0("Working on Iteration = ", i))
  
  start.time <- Sys.time()
  perf.test()
  end.time <- Sys.time()
  
  ls.times$iter[i] <- i
  
  ls.times$time[i] <- round(as.numeric(difftime(end.time, 
                                                start.time, 
                                                units = "secs")), 
                            digits = 2)
  
}

print("=== PERFORMANCE BENCHMARK ====")

as.data.table(ls.times)


