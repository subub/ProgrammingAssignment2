## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library('MASS')

makeCacheMatrix <- function(x = matrix()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
  
  invOfMatrix <- NULL
  
  set <- function(y){
    x <<- y
    invOfMatrix <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) invOfMatrix <<- inverse
  
  getinverse <- function() invOfMatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
  
  invOfMatrix <- x$getinverse()
  
  if(!is.null(invOfMatrix)) {
         message("getting cached data of matrix inversion")
         return(invOfMatrix)
       }
  
  data <- x$get()
  
  invOfMatrix <- ginv(data)
  
  x$setinverse(invOfMatrix)
  
  invOfMatrix
  
}

### TEST OUTPUT ###
# > source('D:/Projects/Coursera/DataScience/Rprogramming/ProgrammingAssignment2/cachematrix.R')
# > testMatrix = matrix(c(2, 4, 3, 1), nrow=2,ncol=2)
# > ginv(testMatrix)
# [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2
# > cachedMatrix = makeCacheMatrix(testMatrix)
# > cachedMatrix$get()
# [,1] [,2]
# [1,]    2    3
# [2,]    4    1
# > cacheInverse(cachedMatrix)
# [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2
# > cacheInverse(cachedMatrix)
# getting cached data of matrix inversion
# [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2
# > 

