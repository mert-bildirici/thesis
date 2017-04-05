x <- read.table(file.choose(), header = T, sep = "\t")

temp <- x

diff <- diff(as.numeric(strptime(x$Zaman.GÃ.Ã..,"%d.%m.%Y %H:%M:%S")))/600

m <- 0

for(i in 1:length(diff)){

  if(diff[i]!=1){
    
    for(k in 1:(diff[i]-1)){
      
      temp <- rbind(temp[1:(i+m+k-1), ], NA)
    
    }
    
    temp <- rbind(temp, x[(i+1):nrow(x), ])
    
    m <- m+diff[i]-1
    
  }
  
}

x <- temp

time <- c(1:nrow(x))

time[1] <- as.numeric(strptime(x$Zaman.GÃ.Ã..[1],"%d.%m.%Y %H:%M:%S"))

for(i in 1:(nrow(x)-1)){
  
  time[i+1] <- time[i]+600
  
}

time <- as.POSIXlt(time, origin = "1970-1-1")

x$Zaman.GÃ.Ã.. <- time

write.table(x, file = "manisa_d.txt", row.names = F, sep = "\t")
