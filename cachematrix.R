## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{ 
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #Search for the result
  if(!is.null(m))	#If the result was calculated before
  {
    message("getting cached data")
    return(m)
  }
  #If the inverse is not found then it will calculate it
  data <- x$get()	#Search for the matrix
  m <- solve(data, ...)	#Execute solve function
  x$setinverse(m)		#Save result 
  m				#Show result
}
