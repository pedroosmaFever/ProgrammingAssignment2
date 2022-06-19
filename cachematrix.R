## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function (mat=matrix())
{
  matInv <- NULL
  set <- function(maty)
  {
    mat <<- maty
    matInv <<- NULL
  }
  get <- function() mat
  setinverse <- function(inverse) matInv <<- inverse
  getinverse <- function() matInv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if (!is.null(inv))
  {
    print("return inverted cache")
    return(inv);
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
