# Test_myFuncs.r
# Testing routines for the functions in myFuncs.R

source("myFuncs.R")

# Test vectors for OddMean
x1<- c(3, 4, 155, -1, 5656, 23, 46, 0, -12:200 , 45)
x2<- -189:43
x3<--100:100

test.OddMean <- function(x, posonly=FALSE, calc.mean, test.name) {
   # Comparing the OddMean calculated here to the calculated value for given x vector and posonly
  if((OddMean(x, posonly)-calc.mean)<1e-10){
    # print passed test
    cat("OddMean passed ", test.name, " test.\n")
  } else {
    # print failed test 
    stop("OddMean failed ", test.name, " test.")
  }
}


test.LehmerMean <- function(x, p, calc.mean, test.name) {
    # Comparing the LehmerMean calculated here to the calculated value for given x vector and p value
  if((LehmerMean(x, p)- calc.mean)<1e-10){
    # print passed test
    cat("LehmerMean passed ", test.name, ", p =", p,  " test.\n")
  } else {
    # print failed test 
    stop("LehmerMean failed ", test.name,", p =", p, " test.")
  }
}

# -189-43 OddMean test
calc.mean<--73.00000000000
test.OddMean(x2, FALSE, calc.mean, "-189-43")

# strange vector OddMean test
calc.mean<-91.79279279279
test.OddMean(x1, FALSE, calc.mean, "strange-vector")

# strange vector test with posonly OddMean test
calc.mean<-98.32692307692
test.OddMean(x1, TRUE, calc.mean, "strange-vector-with-posmean")


# -189-43 LehmerMean test with p=0.33
calc.mean<-14.08171610671
test.LehmerMean(x2, 0.33, calc.mean, "-189-43")

# strange vector test with p=-5
calc.mean<-1.02263168476
test.LehmerMean(x1, -5, calc.mean, "strange-vector")

# -100-100 test with p=2.2
calc.mean<-69.09282419695
test.LehmerMean(x3, 2.2, calc.mean, "-100-100")

