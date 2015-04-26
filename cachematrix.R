## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Accepts a matrix and creates a list of functions that:
#0.) Saves the original matrix(set)
#1.) Returns the original matrix (get)
#2.) Sets the inverse matrix(setInverse)
#3.) Returns the inverse matrix(getInverse)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  if(nrow(x) != ncol(x)) #warns user that matrix is not square
  {
    message("Matrix is not square")
  }
  
  set <- function(y) { #the setter function
    x <<- y # save the original matrix
    inv <<- NULL #set default value of the inverse matrix
  }
  
  get <- function() x # function to return the original matrix
  setInverse <- function(solve) inv <<- solve #function that saves the inverse
  getInverse <- function() inv #function to get the cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## Write a short comment describing this function
# Function that returns the inverse. If inverse has been
# already calculated, return the cache, otherwise calculate the 
# inverse, cache it and then return result

cacheSolve <- function(x, ...) { # x should be a matrix created by a call to makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("returning cached data")
    return (inv)
  }
  
  data <- x$get()
  inv <- tryCatch({
    solve(data)
  }, error = function(e)
  {
    message(e)
    message("\nSetting cache to NULL")
  },
  finally ={
    inv <- NULL;
  })
  
  x$setInverse(inv) #set the inverse cache
  inv
}
