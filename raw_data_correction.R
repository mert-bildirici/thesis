a <- load$fark
a <- a[c(-1)]
a <- round(a)

temp <- load
l <- 0

for(i in 1:length(a)){

  if(a[i]!=1){
    
    for(k in 1:(a[i]-1)){
      
      temp <- rbind(temp[1:(i+l+k-1), ], NA)
    
    }
    
    temp <- rbind(temp, load[i+1:nrow(load), ])
    
  }
  
  l <- l+a[i]-1
  
}

temp <- temp[c(1:52703), ]
temp <- temp[ , c(-4)]

write.csv(temp, file = "load.csv")
