s1 <- map_data[!duplicated(map_data$Label_N) & map_data$Properties_2017 > map_data$Properties_2006,]
r1 <- sum(s1$Properties_2017-s1$Properties_2006)
s2 <- map_data[!duplicated(map_data$Label_N) & map_data$Properties_2017 <= map_data$Properties_2006,]
r2 <- sum(s2$Properties_2017-s2$Properties_2006)
s3 <- map_data[!duplicated(map_data$Label_N),]
r3 <- sum(s3$Properties_2017) - sum(s3$Properties_2006)
sum(s3$Properties_2017) / sum(s3$Properties_2006)

s1 <- map_data[!duplicated(map_data$Label_N) & map_data$Milk_2017 > map_data$Milk_2006,]
r1 <- sum(s1$Milk_2017-s1$Milk_2006)
s2 <- map_data[!duplicated(map_data$Label_N) & map_data$Milk_2017 <= map_data$Milk_2006,]
r2 <- sum(s2$Milk_2017-s2$Milk_2006)
s3 <- map_data[!duplicated(map_data$Label_N),]
r3 <- sum(s3$Milk_2017) - sum(s3$Milk_2006)
sum(s3$Milk_2017) / sum(s3$Milk_2006)