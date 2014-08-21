## Course rprog-006 Assignment 2 - Due Sun Aug 24 - 
## https://github.com/post2it/ProgrammingAssignment2
## Thanks for assistance from Bill Hilton , Tim,Teichmann & other forum writers.

#This function creates a matrix that holds the cache matrix and
#the inverse of the matrix. The inverse is set as NULL until run
#for the first time by the user. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x  #gets the data structure i think do a print to verify
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  #calls all the functions by having them set to variables
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## This function just verifies if the inverse already exist and if it does it 
#returns data to user otherwise it generates inverse and saves the data in 
#the matrix.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

#Example make Vector supplied by Professors
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x  #gets the data structure i think do a print to verify
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  #calls all the functions by having them set to variables
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}

#Example program supplied by Professors
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}