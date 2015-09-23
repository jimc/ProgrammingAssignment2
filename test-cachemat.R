
# basic tests of
source("cachematrix.R")

library(testthat)
library(MASS) # for ginv

m1 <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3)
ma <- matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1), nrow = 4, ncol = 4)
max <- 4 * ma
imax <- ginv(max)

cmax <- makeCacheMatrix(max)

expect_message(
        icmax <- cacheSolve(cmax, msg = "cmax 1st"), 
        "computing")

expect_message(
        icmax2 <- cacheSolve(cmax, msg = "cmax 2nd"),
        "getting cached")


cm1 <- makeCacheMatrix(m1)

expect_message(
        icm1 <- cacheSolve(cm1, msg = "cm1 1st"),
        "computing")

expect_message(
        ixxx <- cacheSolve(cmax, msg = "cmax 3rd"),
        "getting cached")

expect_message(
        icm1 <- cacheSolve(cm1, msg = "cm1 2nd"),
        "getting cached")

odd <- makeCacheMatrix()
odd$set(imax)

expect_message(
        cacheSolve(odd, msg = "set after create"),
        "computing")

expect_message(
        ixxx <- cacheSolve(cmax, msg = "cmax 3rd"),
        "getting cached")

expect_message(
        cacheSolve(odd, msg = "get again"),
        "getting cached")

expect_message(
        cacheSolve(odd, msg = "get again"),
        "getting cached")

expect_that(icmax, equals(icmax2))
expect_that(icmax, equals(icmax2))

