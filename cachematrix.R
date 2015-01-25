## Put comments here that give an overall description of what your
## functions do
#Function makeCacheMatrix stores the data matrix and the inverse matrix and works
#like cache. getinv and setinv variables help retrieve and set the inverse matrix. 
#Function cacheSolve gets the cached data matrix and calculates inverse matrix using solve()
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  # Matrix data 
  
  m <- NULL
  set <- function(y) {  
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Write a short comment describing this function

#Acceses the data matrix from makeCacheMatrix function using get method.
#After getting the data, computes inverse matrix by multipying data matrix with the matrix obtained after solve()
#setinv sets the answer in the cacheMatrix. getinv displays this inverse matrix if it has been set succesfully.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) %*% x
  x$setinv(m)
  m
}

# Usage: a<-matrix(c(12,45,65,23,12,78,45,98,77),3,3)
# inv<-makeCacheMatrix(a)
# inv$getinv() gives NULL
# inv$setinv(solve(a))
# inv$getinv() gives            [,1]         [,2]         [,3]
#                 [1,] -0.06163724  0.015950470  0.015721165
#                 [2,]  0.02664526 -0.018353589  0.007787205
#                 [3,]  0.02504013  0.005127264 -0.008172438

