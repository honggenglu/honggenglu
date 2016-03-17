##Econ 294 Final
##Honggeng Lu
##1505094

install.packages("nycflights13")
library(nycflights13)
library(dplyr)
install.packages("RSQLite")
library(RSQLite)
library(ggplot2)

##Part A
my_db <- src_sqlite("my_db.sqlite", create = T)
d_a <- tbl(my_db, "flights") %>% 
  left_join(weather, by = "origin", copy = TRUE) %>%  
  mutate(canceled = is.na(arr_time)) %>%
  filter(year.x == 2013) %>%
  as.data.frame(n=20000)
lu_a <- d_a %>%
  summarise(cor.temp = cor(dep_delay,temp,use="pairwise.complete.obs"), 
            cor.dewp = cor(dep_delay,dewp,use="pairwise.complete.obs"),
            cor.humid = cor(dep_delay,humid,use="pairwise.complete.obs"), 
            cor.wind_dir = cor(dep_delay,wind_dir,use="pairwise.complete.obs"), 
            cor.wind_speed = cor(dep_delay,wind_speed,use="pairwise.complete.obs"), 
            cor.pressure = cor(dep_delay,pressure,use="pairwise.complete.obs"), 
            cor.visib = cor(dep_delay,visib,use="pairwise.complete.obs"))
lu_a


##Part B
data <- tbl(my_db, "flights") %>%  
  mutate(canceled = is.na(arr_time)) %>%
  filter(year == 2013) %>%
  as.data.frame(n=500000)
data_b1 <- data.frame(month = as.factor(unique(data$month)), value = with(data,tapply(dep_delay,month,mean,na.rm=T)))
ggplot(data_b1, aes(as.factor(month), value)) + geom_bar(aes(fill=month), stat = "identity") + xlab("month") + theme_classic()


data_b2 <- data.frame(day = as.factor(unique(data$day)),
                      value = with(data,tapply(dep_delay,day,mean,na.rm=T)))
ggplot(data_b2, aes(as.factor(day), value)) + geom_bar(aes(fill=day), stat = "identity") + xlab("day") + theme_classic()


data <- transform(data,month = ifelse(month < 10, paste0("0",month),month),
                  day = ifelse(day < 10, paste0("0",day),day))
data <- transform(data,dayofweek = strftime(as.Date(paste0(year,"-",month,"-",day)),
                                            format = "%u"))
data_b3 <- with(data, tapply(dep_delay,dayofweek, mean, na.rm=T))
data_b3 <- data.frame( dayofweek=1:7, value = data_b3)
ggplot(data_b3, aes(as.factor(dayofweek), value)) + geom_bar(aes(fill=dayofweek), stat = "identity") + xlab("dayofweek") + theme_classic()

##Part C
df <- group_by(d_a, dest) %>%  mutate(canceled = is.na(arr_time)) %>% summarise(count = mean(dep_delay), count2 = n(), count3 = sum(canceled)) 
df <- as.data.frame(df)
df$p <- df[ ,4]/df[ ,3]
lu_c1 <- ggplot(df, aes(x= dest,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
lu_c1
lu_c2 <- ggplot(df, aes(x= dest,y=p)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
lu_c2

##Part D
d_d <- d_a %>% left_join(planes, by = "tailnum", copy = TRUE)
d_d <- group_by(d_d, manufacturer) %>% summarise(count = mean(dep_delay))
d_d2 <- as.data.frame(d_d)
lu_d <- ggplot(d_d2, aes(x= manufacturer,y=count)) + geom_bar(stat="identity", fill = "indianred") + theme(axis.text.x=element_text(angle = 90))
lu_d