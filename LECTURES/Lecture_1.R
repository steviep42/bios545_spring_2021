getOption("defaultPackages")

search()

library(help="stats")

data(package="stats")

install.packages("actuar",dependencies=TRUE)
library(actuar) 

search()

install.packages("devtools")
library(devtools)
install_github("Displayr/flipPlots")

# Help

help.start()         # Launches a web browser with search capability

help(function_name)  # Get help on "function_name"

?function_name        # Equivalent to the above

args(function_name)    # See what arguments the function accepts

example(function_name) # See an example of the function

example(mean)

help.search("time series") 

??"time series"

# Walkthrough

url <- "https://raw.githubusercontent.com/steviep42/bios545_spring_2021/master/DATA.DIR/table_7_3.csv"

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

# The null hypothesis is that the data is normal

shapiro.test(engine$co)

log.engine <- log(engine$co)

shapiro.test(log.engine)

par(mfrow=c(2,2))

log.engine <- log(engine$co)

boxplot(log.engine,main="Carbon Monoxide")

hist(log.engine,main="Carbon Monoxide")

qqnorm(log.engine,main="QQ Plot for the Log of the 
                        Carbon Monoxide")

qqline(log.engine)

# Let's build a confidence interval

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

# Test H0: mu = 5.4
# HA:mu != 5.4

lNull <- log(5.4) - error

rNull <- log(5.4) + error

c(lNull,rNull)

my.mean

p.val <- 2*(1-pt((my.mean-log(5.4))/se,df=n-1))

p.val

t.test(log.engine,mu = log(5.4),alternative = "two.sided")

###

?mean             # Get help on the mean function

example(kmeans)   # Run an example of kmeans (if it exists)

pi                # Some popular quantities are built-?‐in to R

sqrt(2) # Basic arithmetic

print(pi) 

X <- 3; Y <- 4 # Semicolon lets you enter 2 commands on the same line

Z <- sqrt(X^2 + Y^2) # Variables contain information

# List all variables in the "environment"

ls()

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

ls()

my.lm <- lm(mpg ~ wt,mtcars)

ls(my.lm)

save(my.lm,file="/Users/myhome/mylmresults")

# You can come back later and load this file

mylmstuff <- load("/Users/myhome/mylmresults")

A <- 2.5    # The "<-" is the preferred method of assignment

A = 2.5     # This is equivalent to the above although using the "=" is 
# discouraged except in setting function arguments.

A
mynewvar <- X + 3

MYNEWVAR <- X + 3    # Two different variables


head(mtcars)
write.table(mtcars,file="mtcars.csv",
            row.names=TRUE,               # Row names get saved
            col.names=TRUE,               # Header gets saved
            sep=",")                      # Field seperator is ,

mycars <- read.table("mtcars.csv",header=TRUE,sep=",")

head(mycars)


### web scraping

library(rvest)
url <- "https://coinmarketcap.com/all/views/all/"
bc <- read_html(url)

bc_table <- bc %>% 
  html_nodes('table') %>% 
  html_table() %>% .[[3]]

bc_table <- bc_table[,c(2:3,5)]
bc_table <- bc_table %>% mutate(Price=gsub("\\$","",Price))
bc_table <- bc_table %>% mutate(Price=gsub(",","",Price))
bc_table <- bc_table %>% mutate(Price=round(as.numeric(Price),2))

# There are four rows wherein the Price is missing NA
bc_table <- bc_table %>% filter(complete.cases(bc_table))

# Let's get the Crypto currencies with the Top 10 highest prices 
top_10 <- bc_table %>% arrange(desc(Price)) %>% head(10)
top_10

# Next we want to make a barplot of the Top 10
ylim=c(0,max(top_10$Price)+10000)
main="Top 10 Crypto Currencies in Terms of Price"
bp <- barplot(top_10$Price,col="aquamarine",
              ylim=ylim,main=main)
axis(1, at=bp, labels=top_10$Symbol,  cex.axis = 0.7)
grid()

#

# Let's take the log of the price
ylim=c(0,max(log(top_10$Price))+5)
main="Top 10 Crypto Currencies in Terms of log(Price)"
bp <- barplot(log(top_10$Price),col="aquamarine",
              ylim=ylim,main=main)
axis(1, at=bp, labels=top_10$Symbol,  cex.axis = 0.7)
grid()

