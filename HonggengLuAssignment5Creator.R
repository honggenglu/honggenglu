#Econ 294 Assignment 5
print("Honggeng Lu")
print(1505094)
print("hlu22@ucsc.edu")

# 1
# a
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
graph1a <- ggplot(diamonds, aes(x=x*y*z, y=price, color=clarity))
graph1a+geom_point(aes(color=clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()

# b
graph1b <- ggplot(diamonds,aes(carat, fill=clarity,..density..))
graph1b+geom_histogram()+facet_grid(cut~.)

# c
graph1c <- ggplot(diamonds,aes(x=cut,price))
graph1c+geom_jitter(alpha=0.1)+geom_violin()

# 3
# a
library(foreign)
require(dplyr)
org <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

org3a <- org %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    rw_quantile1st = quantile(rw, .1, na.rm = T),
    rw_quantile9st = quantile(rw, .9, na.rm = T),
    rw_quantile1 = quantile(rw, .25, na.rm = T),
    rw_quantile3 = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

org3a <- org3a %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))

graph3a <- ggplot(org3a, aes(x=date, y=Median.RW))
graph3a + geom_ribbon(aes(ymin=rw_quantile1, ymax=rw_quantile3),alpha=0.6) + geom_ribbon(aes(ymin=rw_quantile1st, ymax=rw_quantile9st),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))

#b#
org3b <- org %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

org3b <- org3b %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))

graph3b <- ggplot(org3b, aes(x=date, y=Median.RW,group=educ))
graph3b + geom_line(aes(color=educ))