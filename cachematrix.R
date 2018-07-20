##  Catching the Inverse of the Matrix

## makeCacheMatrix creates a special "matrix" that can store matrix and its inverse 

## matrix is stored in variable 'x'
## inverse matrix is stored in variable 'inv'

## Special matrix has a list of following functions to:
##  1. set the value of the matrix  - set(<matrix>)
##  2. get the value of the matrix  - get()
##  3. set the value of the inverse - setinv(<inverse matrix>)
##  4. get the value of the inverse - getinv()

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL   
   set <- function(y = matrix()) {
      x <<- y
      inv <- NULL
   }
   get <- function() x
   setinv <- function(invmat) inv <<- invmat
   getinv <- function() inv
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve function computes the inverse of the special "matrix" 
## If the inverse is already present in special matrix then inverse is fetched from there
## else inverse is caluclated using solve fuction and stored in special matrix using setinv()
## matrix inverse is displayed as the final outout

cacheSolve <- function(x, ...) {
   invm <- x$getinv()
   if(!is.null(invm)) {
      message("getting cached data")
      return(invm)
   }
   mat <- x$get()
   invm <- solve(mat, ...)
   x$setinv(invm)
   invm
}



## Test Cases:


## Test Case 1

test1 <- makeCacheMatrix(matrix(1:4, 2, 2))
#check the data in Special Matrix
test1$get()
#check if matrix inverise is present in Special Matrix
test1$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
cacheSolve(test1)
#check again if matrix inverise is present in Special Matrix
test1$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
#Inverse matrix is fetched from Special matrix
cacheSolve(test1)
#Cross-Checking the matrix inverse using Solve function 
solve(matrix(1:4, 2, 2))



## Test Case 2

test2 <- makeCacheMatrix(matrix(7:10, 2, 2))
#check the data in Special Matrix
test2$get()
#check if matrix inverise is present in Special Matrix
test2$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
cacheSolve(test2)
#check again if matrix inverise is present in Special Matrix
test2$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
#Inverse matrix is fetched from Special matrix
cacheSolve(test2)
#Cross-Checking the matrix inverse using Solve function 
solve(matrix(7:10, 2, 2))


## Test Case 3

test3 <- makeCacheMatrix(matrix(c(4,7,2,6), 2, 2))
#check the data in Special Matrix
test3$get()
#check if matrix inverise is present in Special Matrix
test3$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
cacheSolve(test3)
#check again if matrix inverise is present in Special Matrix
test3$getinv()
#Pass special matrix to cacheSolve function to calculate matrix inverse
#Inverse matrix is fetched from Special matrix
cacheSolve(test3)
#Cross-Checking the matrix inverse using Solve function 
solve(matrix(c(4,7,2,6), 2, 2))