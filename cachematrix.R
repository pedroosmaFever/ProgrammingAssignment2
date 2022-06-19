## Put comments here that give an overall description of what your
## functions do

## Creates a special object which will contain a cached inverted matrix
makeCacheMatrix <- function (mat=matrix())
{
  #initialize the cached matrix
  matInv <- NULL
  #function which will set the matrix data
  set <- function(maty)
  {
    mat <<- maty
    matInv <<- NULL
  }
  get <- function() mat
  #these functions will set and get the inverse matrix inside this object
  setinverse <- function(inverse) matInv <<- inverse
  getinverse <- function() matInv
  #the object itself is a list of functions for setting and getting the matrix and its inverse and also the cached inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function will use the special object created for containing the inversed matrix
cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  #return the inverse matrix in case there is value
  if (!is.null(inv))
  {
    print("return inverted cache")
    return(inv);
  }
  #in case there is no cached matrix, inverse it and return
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
