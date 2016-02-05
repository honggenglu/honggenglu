# 0
print("Honggeng Lu")
print(1505094)
print("hlu22@ucsc.edu")

# 1
library(foreign)
flights <-read.csv("flights.csv", stringsAsFactors = FALSE)
airports <-read.csv("airports.csv", stringsAsFactors = FALSE)
weather <-read.csv("weather.csv", stringsAsFactors = FALSE) 
planes   <-read.csv("planes.csv", stringsAsFactors = FALSE) 

# 2
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)
airports$date<-as.Date(airports$date)
planes$date<-as.Date(planes$date)


# 3
flights.2a<-subset(flights, dest=="SFO"|dest=="OAK")
nrow(flights.2a)

flights.2b<-subset(flights, dep_delay>=60)
nrow(flights.2b)

flights.2c<-subset(flights, arr_delay>=2*dep_delay)
nrow(flights.2c)

# 4
library(dplyr)
select(flights, dep_delay, arr_delay)
select(flights, ends_with("delay"))
select(flights, contains("delay"))

# 5
select(flights, dep_delay) %>% arrange(desc(dep_delay)) %>% head(5)
flights %>% arrange(desc(dep_delay - arr_delay)) %>% head(5)

# 6
flights <- mutate(flights, speed = dist/(time/60))
flights <- mutate(flights, delta = dep_delay - arr_delay)
View(flights)

flights %>% arrange(desc(speed)) %>% head(5)
flights %>% arrange(desc(delta)) %>% head(5)
flights %>% arrange(delta) %>% head(1)

# 7
flights.7a <- flights %>% group_by(carrier) %>% summarise (
    cancelled = sum(cancelled),
    total_flights = n(),
    cancelled_percent = cancelled/total_flights,
    min = min(delta, na.rm = T),
    quantile_1st = quantile(delta, 0.25, na.rm = T),
    mean = mean(delta, na.rm = T),
    median = median(delta, na.rm = T),
    quantile_3rd = quantile(delta, 0.75, na.rm = T),
    quantile_90th = quantile(delta, 0.90, na.rm = T),
    max = max(delta, na.rm = T)
)

print(flights.7a %>% arrange(desc(cancelled_percent)))

cat('First filter data by removing rows having na value in dep_delay,
    then group by date, and calculate average dep_delay and number
    of flights on each date. Finally, filter the summarized table by
    only keeping rows that have more than 10 flights on one day.')
day_delay <- flights %>% dplyr::filter(!is.na(dep_delay)) %>% group_by(date) %>%
  summarise(delay = mean(dep_delay), n = n()) %>% dplyr::filter(n > 10)

# 8
day_delay <- day_delay %>% mutate(diff = delay - lag(delay)) 
day_delay %>% arrange(desc(diff)) %>% head(5)

# 9
dest_delay <- flights %>% group_by(dest) %>%
  summarise (
    avg_arr_delay = mean(arr_delay, na.rm = T),
    number_flights = n()
  )

airports <- airports %>% 
  select(
    dest = iata, 
    name = airport, 
    city,
    state, 
    lat, 
    long
  )

df.9a <- airports %>% left_join(dest_delay, by="dest")
df.9a %>% arrange(desc(avg_arr_delay)) %>% head(5)

df.9b<- airports %>% inner_join(dest_delay, by="dest")
print('Not match. They are different.')

df.9c<- airports %>% right_join(dest_delay, by="dest")
print('There are 116 observations. No NA appear in avg_arr_delay')

df.9d<- airports %>% full_join(dest_delay, by="dest")
print('There are 3378 observations. There are 3262 NA\'s in avg_arr_delay')
print('number of rows of both tables are not the same.')

# 10
hourly_delay <- flights %>% filter(!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())

hourly_delay %>% full_join(weather) %>% group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% arrange(desc(max_delay))


# 11
# a)
library(tidyr)
library(dplyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)

# b)
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df
df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

# c)
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df %>% separate(demo, into = c('sex','age','state') , sep = '_')

#d)
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df
df <- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df


