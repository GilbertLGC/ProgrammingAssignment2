## The following two functions can get the inverse matrix   ##
## of a matrix, which has been already calculated, from     ##
## the cache and avoid the repeated and time-consuming      ##
## computation.                                             ##
  
  ## This function creats a list containing 4 functions:    ##
  ## set/get the value of the matrix/its inverse matrix.    ##
  makeCacheMatrix <- function(x = matrix())
  {
    i <- NULL
    set <- function(y)
    {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  }
  
  ## This function calculates the inverse matrix of the     ##
  ## list created by the above function. It first checks in ##
  ## the cache too see whether the inverse matrix has       ##
  ## already been calculated. If so, it gets the inverse    ##
  ## matrix from the cache, skips the calculation, and      ##
  ## states "getting cached data". If not, it calculates    ##
  ## the inverse matrix and stores it in the cache.         ##
  cacheSolve <- function(x, ...)
  {
    i <- x$getinverse()
    if(!is.null(i))
    {
      message("getting cached matrix")
      return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
  }