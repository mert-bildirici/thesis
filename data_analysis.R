#finding averages with missing data
generation_average_with_NAs <- sum(all$`Üretim (MW)`, na.rm = T)/(nrow(all)-sum(is.na(all$`Üretim (MW)`)))
consumption_average_with_NAs <- sum(all$`Yük (MW)`, na.rm = T)/(nrow(all)-sum(is.na(all$`Yük (MW)`)))

#comparison
constant_for_power <- generation_average_with_NAs/consumption_average_with_NAs
constant_for_energy <- sum(all$`Üretim (MW)`, na.rm = T)/sum(all$`Yük (MW)`, na.rm = T)

#new load assumption with energy constant
load_assumption <- constant_for_energy * all$`Yük (MW)`
all <- cbind(all, load_assumption)

#power difference after assumption
difference <- all$`Üretim (MW)`-all$load_assumption
all <- cbind(all, difference)

plot(all$difference)
