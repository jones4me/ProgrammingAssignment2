## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function, makeCacheMatrix creates a special "matrix", which is really a list 
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
                     setinverse = setinverse, 
                     getinverse = getinverse)
}

## Write a short comment describing this function

# The cacheSolve function calculates the inverse of the special "matrix" created with 
# the makeCacheMatrix function. However, it first checks to see if the inverse has already 
# been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
# cache via the setinverse function.


cacheSolve <- function(x, ...){
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached inverse")
                return(m)
        }
        else{
                message("no inverse found in cache, calculating inverse")
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}

# Testing
# a <- matrix(data= 3:6,nrow=2,ncol=2)
# print("Original Matrix to be inverted:")
# print(a)
# ## 
# z <- makeCacheMatrix(a)
# # Run makeCacheMatrix
# print("First time running cacheSolve ...Should not have cached inverse")
# # Run cacheSolve
# zinv <- cacheSolve(z)
# # Run second time... should get cached inverse
# print("2nd time running cacheSolve...Should get message: Getting cached inverse")
# zinv<-cacheSolve(z)
# print(zinv)

