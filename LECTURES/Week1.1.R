
getOption("defaultPackages")

search()
  

library(help="stats")

search()

data(package="stats")  # Find data included in package "stats"


# Or could use the RStudio menu to install packages
install.packages("actuar",dependencies=TRUE)

library(actuar) # Brings the package into the workspace

 search()



 

help.start()         # Launches a web browser with search capability

help(function_name)  # Get help on "function_name"

?function_name        # Equivalent to the above

args(function_name)    # See what arguments the function accepts

example(function_name) # See an example of the function

example(mean)


help.search("time series") 

??"time series"    # Equivalent to the above

url <- "http://pittardsp.github.io/YOUTUBE.DIR/table_7_3.csv"

engine <- read.table(url, sep = ",", header=TRUE)
engine <- engine[,-1]

head(engine) # 3 engine pollutants

summary(engine)

boxplot(engine,col="red",main="Engine Pollutants")

par(mfrow=c(1,3))

boxplot(engine$co,main="Carbon Monoxide")

hist(engine$co)

qqnorm(engine$co,main="Carbon Monoxide")

qqline(engine$co)



shapiro.test(engine$co)

log.engine <- log(engine$co)

shapiro.test(log.engine)

Shapiro-Wilk normality test

par(mfrow=c(2,2))

log.engine <- log(engine$co)

boxplot(log.engine,main="Carbon Monoxide")

hist(log.engine,main="Carbon Monoxide")

qqnorm(log.engine,main="QQ Plot for the Log of the Carbon Monoxide")

qqline(log.engine)


my.mean <- mean(log.engine)
my.sd < sd(log.engine)
n <- length(log.engine)

# Get standard error
se <- my.sd/sqrt(n)

error <- se*qt(0.975,df=n-1)

left <- my.mean - error

right <- my.mean + error

c(left,right)


c(exp(left),exp(right))

lNull <- log(5.4) - error

rNull <- log(5.4) + error

c(lNull,rNull)

my.mean

p.val <- 2*(1-pt((my.mean-log(5.4))/se,df=n-1))

p.val

t.test(log.engine,mu = log(5.4),alternative = "two.sided")

# First R Session

?mean             # Get help on the mean function

example(kmeans)   # Run an example of kmeans (if it exists)

pi                # Some popular quantities are built-?‐in to R


sqrt(2) # Basic arithmetic


print(pi) # Print the comments of the pi variable


X <- 3; Y <- 4 # Semicolon lets you enter 2 commands on the same line

Z <- sqrt(X^2 + Y^2) # Variables contain information

ls()
[1] "X" "Y" "Z"

log(10)             ceiling(6.8)             2+3


log10(100)          round(6.889,2)           3/2

sin(pi/2)           3/0                      2^3 

cos(pi/2)           0/0                      (56-14)/6 - 4*7*10/(5^2-5)

1.3e6               is.finite(3)             abs(2-4)

9 %% 2              x <- c(1:8,NA)           

floor(5.7)          mean(x)

\end{frame}




# a^2 + b^2 = c^2                 # Pythagorean Theorem

a <- 2; b <- 4

c <- sqrt(a^2 + b^2)              # To solve the PT for c

a <- 2; b <- 4; c <- 1

(-b + sqrt(b^2-4*a*c)) / (2*a)    # First case quadratic formula solution

(-b - sqrt(b^2 - 4*a*c)) / (2*a)  # Second case quadratic formula solution
r <- 4; h <- 6; b <- 3

circumference <- 2*pi*r             # circumference of a circle

area <- (b*h)/2 # Area of a triangle  


my.quad <- function(a,b,c) {
  r1 <- (-b + sqrt(b^2 - 4*a*c)) / (2*a)
  r2 <- (-b - sqrt(b^2 - 4*a*c)) / (2*a)
  my.roots = c(r1,r2)
  return(my.roots)
}

# Solve for ax^2 + bx + c where a = 1, b=6, and c=8

my.quad(1,6,8)




myfiles <- list.files()

str(myfiles)

myfiles[1:5]

for (ii in 1:length(myfiles)) {
  file <- myfiles[ii]
  # Do something 
} 

ls()


my.lm <- lm(mpg ~ wt,mtcars)

ls(my.lm)

save(my.lm,file="/Users/myhome/mylmresults")


mylmstuff <- load("/Users/myhome/mylmresults")


A <- 2.5    # The "<-" is the preferred method of assignment

A = 2.5     # This is equivalent to the above although using the "=" is 
# discouraged except in setting function arguments.

A

mynewvar <- X + 3

MYNEWVAR <- X + 3    # Two different variables

head(mtcars)
mpg cyl disp hp drat wt qsec vs am gear carb
Mazda RX4       21.0 6 160 110 3.90 2.620 16.46 0 1 4 4
Mazda RX4 Wag   21.0 6 160 110 3.90 2.875 17.02 0 1 4 4
Datsun 710      22.8 4 108  93 3.85 2.320 18.61 1 1 4 1
Hornet 4 Drive  21.4 6 258 110 3.08 3.215 19.44 1 0 3 1

write.table(mtcars,file="mtcars.csv", row.names=TRUE,  # Row names get saved
col.names=TRUE,               # Header gets saved
sep=",")                      # Field seperator is ,


mycars <- read.table("mtcars.csv",header=TRUE,sep=",")

url <- "http://pittardsp.github.io/YOUTUBE.DIR/hsb2.csv"

my.input <- read.table(url,header=T,sep=",")

head(my.input)


library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

my_html <- read_html(url)

my_tables <- html_nodes(my_html,"table")[[1]]
populous_table <- html_table(my_tables)

populous_table <- populous_table[,-4:-6]
populous_table$Population <- as.numeric(gsub(",","",
                                             populous_table$Population))/100000

names(populous_table) = c("Rank","Country","Population")

# Let's plot the first 10 rows

library(lattice)
xyplot(Population ~ as.factor(Country), populous_table[1:10,],
       scales = list(x = c(rot=60)),type="h",main="Most Densely Populated Countries")

library(readxl)
url <- "http://pittardsp.github.io/DATA.DIR/nursing_home.xlsx"
download.file(url,"nursing_home.xlsx")

# Read the first worksheet from the Workbook
sh1 <- read_xlsx("nursing_home.xlsx",sheet=1)

sh1[1:5,2:4]


sh2 <- read_xlsx("nursing_home.xlsx",sheet=2)

sh2[1:4,1:4]


library(RCurl)   # Install these if necessary
library(XML)

google.url <- "http://maps.googleapis.com/maps/api/geocode/xml?address="
query.url <- paste(google.url, "Atlanta,GA","&amp&sensor=false", sep="")
txt <- getURL(query.url)
xml.report <- xmlTreeParse(txt,useInternalNodes=TRUE)

place <- getNodeSet(xml.report,"//GeocodeResponse/result[1]/geometry/location[1]/*")
as.numeric(sapply(place,xmlValue))
