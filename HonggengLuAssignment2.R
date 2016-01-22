#Assignment 2

library(foreign)

# 0
HonggengLuAssignment2 <- list(
  firstName = "Honggeng",
  lastName  = "Lu",
  email     = "hlu22@ucsc.edu",
  studentID = 1505094
)

# 1
install.packages("repmis")
library(repmis)
diamondsURL <- source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
HonggengLuAssignment2$s1a <- nrow(diamondsURL);HonggengLuAssignment2$s1a
#There're 7 obervations and 4 variables
HonggengLuAssignment2$s1b <- ncol(diamondsURL);HonggengLuAssignment2$s1b
#There're 4 columns
HonggengLuAssignment2$s1c <- names(diamondsURL);HonggengLuAssignment2$s1c
#The header names are "carat" "cut" "clarity" "price"
HonggengLuAssignment2$s1d <- summary(diamondsURL$price);HonggengLuAssignment2$s1d
#The results are below:
#Min. 1st    Qu.  Median    Mean  3rd Qu.    Max.    NA's 
#    420     450     600     650     825     980       1 

# 2
NHIS_2007_TSV <- read.csv("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt",sep = "\t")
HonggengLuAssignment2$s2a <- dim(NHIS_2007_TSV);HonggengLuAssignment2$s2a
#There're 4785 observations
HonggengLuAssignment2$s2b <- ncol(NHIS_2007_TSV);HonggengLuAssignment2$s2b
#There're 9 columns
HonggengLuAssignment2$s2c <- names(NHIS_2007_TSV);HonggengLuAssignment2$s2c
#The header names are "HHX" "FMX" "FPX" "SEX" "BMI" "SLEEP" "educ" "height" "weight"
HonggengLuAssignment2$s2d <- mean(NHIS_2007_TSV$weight, na.rm=T);HonggengLuAssignment2$s2d
#The mean weight of the weight column is 266.2357
HonggengLuAssignment2$s2e <- median(NHIS_2007_TSV$weight, na.rm=T);HonggengLuAssignment2$s2e
#The median weight of the weight column is 175
hist(NHIS_2007_TSV$weight)
table(NHIS_2007_TSV$weight)
NHIS_2007_TSV$weight1 <- ifelse(NHIS_2007_TSV$weight<900,
                                 yes = NHIS_2007_TSV$weight,
                                 no = 0);NHIS_2007_TSV$weight1
HonggengLuAssignment2$s2f <- mean(NHIS_2007_TSV$weight1, na.rm=T);HonggengLuAssignment2$s2f
#The new mean weight of this adjusted weight column is 154.5749
HonggengLuAssignment2$s2g <- median(NHIS_2007_TSV$weight1, na.rm=T);HonggengLuAssignment2$s2g
#The new mean median weight of this adjusted weight column is 164
men <- subset(NHIS_2007_TSV,(SEX==1))
women <- subset(NHIS_2007_TSV,(SEX==2))
#summary for women
HonggengLuAssignment2$s2h <- summary(women[["weight1"]])
HonggengLuAssignment2$s2h
#Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#0.0   125.0   145.0    138.3   172.0    274.0 
#summary for men
HonggengLuAssignment2$s2i <- summary(men[["weight1"]])
HonggengLuAssignment2$s2i
#Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#0.0   160.0   183.0    174.3   210.0    298.0 

# 3
vec <- c(letters,LETTERS)
HonggengLuAssignment2$s3a <- vec[1:26*2]
HonggengLuAssignment2$s3b <- paste(vec[c(34,15,14)],collapse = "...")
arr <- array(c(letters,LETTERS), dim=c(3,3,3))
View(arr)
arr1 <- arr[,,1]
arr2 <- arr[,,2]
arr3 <- arr[,,3]
HonggengLuAssignment2$s3c <- arr2[,1]
HonggengLuAssignment2$s3d <- c(arr1[2,2],arr2[2,2],arr3[2,2])
HonggengLuAssignment2$s3e <- paste(arr[2,3,1],arr[3,2,2],arr[2,2,2],sep = "")