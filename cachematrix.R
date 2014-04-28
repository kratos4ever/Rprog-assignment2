## functions to implement creation a cached matrix inverse closure

## makeCacheMatrix function is a closure that implements a simple cache method of saving the inverse of a matrix.
## the getinv and setinv functions act as the set 
makeCacheMatrix <- function(x = matrix()) {
  # define matrix to store the inverse - this is the cached value. initialize to a NA valued matrix
  m_inv <- matrix(,nrow(x),ncol(x))
  
  # Sets the value of the matrix variable "x" as the argument passed in and resets the value of the inverse matrix "m_inv"
  set <- function(y){
    x <<-y
    m_inv <<- matrix(,nrow(x),ncol(x))
  }
  
  # returns the value of the matrix variable "x"
  get <- function() x
  
  # sets the value of the inverse matrix as the argument passed in
  setinv <- function(arginv) m_inv <<- arginv 
  
  # returns the value of the inverse matrix
  getinv <- function() m_inv
  
  # return a list of functions defined in the closure to be operated on the matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Gets the cached matrix inverse value for a makeCacheMatrix closure
cacheSolve <- function(x, ...) {
  # get the cached inverse matrix value of argument x
  m_inverse <- x$getinv()
  
  # if the value of the inverse matrix is all NA, the cache function needs to set the inverse matrix value 
  if(length(m_inverse[!is.na(m_inverse)]) > 0){
    message("Using the cached inverse matrix")
    return(m_inverse)
  }
  
  # as the m_inverse is NULL, the inverse has never been computed for this x
  # run solve and save the inverse as the cached matrix inverse value and return it
  matval <- x$get()
  m_inverse <- solve(matval)  
  x$setinv(m_inverse)
  
  m_inverse
}
