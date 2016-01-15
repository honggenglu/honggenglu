library(foreign)

# 0)
print("First name: Honggeng, Last Name: Lu, student ID: 1505094")

# 1)
df.dta <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")
df.csv <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")
df.td <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",sep = "\t")
df.rdata <- load( url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))
print(".RData contains a data structure called NHIS_2007_RData")

# 2)
print("NHIS_2007_CSV.csv: 139KB")
# 139KB
print("NHIS_2007_dta.dta: 189KB")
# 189KB
print("NHIS_2007_TSV.txt: 139KB")
# 139KB
print("NHIS_2007_RData.RData: 46KB")
# 46KB
# .RData is the smallest one, .dta file is the largest one.
# Besides the .dta file, .csv and .txt account for their variability.

# 3)
print(typeof(NHIS_2007_RData))
# the type of this data structure is "list"
print(class(NHIS_2007_RData))
# the class of this data structure is "data.frame"
print(length(NHIS_2007_RData))
# the length of this dataset is 9
print(dim(NHIS_2007_RData))
# there are 9 variables and 4785 observations
print(nrow(NHIS_2007_RData))
# there are 4785 rows
print(ncol(NHIS_2007_RData))
# there are 9 columns
print(summary(NHIS_2007_RData))
#HHX             FMX             FPX             SEX             BMI            SLEEP       
#Min.   :   16   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :12.91   Min.   : 3.000  
#1st Qu.:13404   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:23.63   1st Qu.: 6.000  
#Median :27527   Median :1.000   Median :1.000   Median :2.000   Median :26.97   Median : 7.000  
#Mean   :27009   Mean   :1.019   Mean   :1.359   Mean   :1.549   Mean   :31.73   Mean   : 9.507  
#3rd Qu.:40192   3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:31.51   3rd Qu.: 8.000  
#Max.   :53955   Max.   :6.000   Max.   :8.000   Max.   :2.000   Max.   :99.99   Max.   :99.000  
#educ           height          weight     
#Min.   : 0.00   Min.   :59.00   Min.   :100.0  
#1st Qu.:12.00   1st Qu.:64.00   1st Qu.:149.0  
#Median :13.00   Median :67.00   Median :175.0  
#Mean   :14.25   Mean   :69.58   Mean   :266.2  
#3rd Qu.:16.00   3rd Qu.:71.00   3rd Qu.:215.0  
#Max.   :99.00   Max.   :99.00   Max.   :999.0  

# 4)
df <- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
print(str(df))
# there are 1119754 observations with 30 variables
print(summary(df$rw))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.8    10.7    15.9    19.8    24.4   354.8  521279

# 5)
v <- c(1,2,3,4,5,6,7,4,NULL,NA)
print(length(v))
# The length is 9. Because NULL won't be counted.
print(mean(v,na.rm = T))
# the mean is 4

# 6)
x <- matrix(1:9, nrow = 3)
print(t(x))
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
#[3,]    7    8    9
print(eigen(x))
#$values
#[1]  1.611684e+01 -1.116844e+00 -5.700691e-16
#$vectors
#           [,1]       [,2]       [,3]
#[1,] -0.4645473 -0.8829060  0.4082483
#[2,] -0.5707955 -0.2395204 -0.8164966
#[3,] -0.6770438  0.4038651  0.4082483
y <- matrix( c(1,3,2,2,2,3,3,1,0), ncol = 3)
print(solve(y))
#        [,1]    [,2]  [,3]
#[1,] -0.1875  0.5625 -0.25
#[2,]  0.1250 -0.3750  0.50
#[3,]  0.3125  0.0625 -0.25
print( y %*% solve(y))
#              [,1]         [,2]          [,3]
#[1,]  1.000000e+00 5.551115e-17 -2.220446e-16
#[2,]  0.000000e+00 1.000000e+00  5.551115e-17
#[3,] -5.551115e-17 0.000000e+00  1.000000e+00
print("It's called Identity Matrix")

# 7)
diamonds <- data.frame(
  carat = c(5,2,0.5,1.5,5,NA,3),
  cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair"),
  clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA),
  price = c(850, 450, 450, NA , 750, 980, 420)
)

print(mean(diamonds$price, na.rm = T))
# the mean price is 650
print(mean(diamonds$price[diamonds$cut == "fair"], na.rm = T))
# the mean price of cut "fair" is 673.3333
print(mean(diamonds$price[diamonds$cut != "fair"], na.rm = T))
# the mean price of cut "good", "very good", and "Ideal" is 626.6667
print(median(diamonds$price[diamonds$carat > 2 & (diamonds$cut == "Ideal" | diamonds$cut == "very good" )], na.rm = T))
# NA