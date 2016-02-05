#Assignment 3

# 0
HonggengLuAssignment3 <- list(
  firstName = "Honggeng",
  lastName  = "Lu",
  email     = "honggenglu@gmail.com",
  studentID = 1505094
)

# 1
install.packages("dplyr")
library(dplyr)
library(foreign)

df.ex <- read.dta(file="https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta") 

# 2

df.ex.yearmonth <- dplyr::filter(df.ex,year == 2013 & month == 12)
dim(df.ex.yearmonth)
df.ex.yearsummer <- dplyr::filter(df.ex,year == 2013 & (month == 7 |month == 8 |month== 9))
dim(df.ex.yearsummer)

# 3
df.ex.3a <- dplyr::arrange(df.ex,year,month)

# 4
df.ex.4a <- dplyr::select(df.ex,year,age)
df.ex.4b <- dplyr::select(df.ex,year,month,starts_with("i"))
print(unique(df.ex$state))

# 5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

df.ex.5a <- dplyr::mutate(df.ex, rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw)) %>%
  select(rw.stndz, rw_nrmlz)

df.ex.5b <- df.ex %>% group_by(year,month) %>% 
  mutate(rw.stndz = stndz(rw), rw_nrmlz = nrmlz(rw),count = n()) %>%
  select(rw.stndz, rw_nrmlz,count)

# 6
df.ex.6<-
  df.ex %>% 
  group_by(year,month,state)%>%
  summarise(
    rw_min=min(rw,na.rm = T),
    rw_1stQnt=quantile(rw,na.rm = T,0.25),
    rw_mean.art =mean(rw,na.rm = T),
    rw_3rdQnt=quantile(rw,na.rm = T,0.75),
    rw_max=max(rw,na.rm = T),
    rw_median=median(rw,na.rm=T),
    count=n()
  )%>%
  select(state,starts_with("rw_"),count)

print(df.ex.6 %>% ungroup() %>% arrange(desc(rw_mean.art)) %>%
        select(year,month,state) %>% head(1))



# 7
df.ex$state.char <-as.character(df.ex$state)
df.ex.7a <- df.ex %>% arrange(year,month,desc(state.char))