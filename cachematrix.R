## Coursera: R Programming (rprog-030)
## Programming Assignment 2: Caching the Inverse of a Matrix
## 26 July 2015

## Example Use: 
## Step 1: Create square matrix (b<-matrix(c(1,2,3,4), nrow=2)
## Step 2: Define new function a<-makeCacheMatrix(b)
## Step 3: Solve for matrix inverse using cacheSolve(a)

## makeCacheMatrix creates a list of functions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of a special matrix,
## after checking if the inverse has been calculated already.

## Use "solve" function to get matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

