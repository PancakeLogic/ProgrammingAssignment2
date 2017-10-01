## Week 3: Programming assignment - Cache the inverse of a matrix

## Avoid unnecessary computation by checking to see if a mean has
## already been calculated, and using that cached value rather
## than repeatedly looping.

## Function 1: create a special vector which is effectively a list
## of values/means that can be re-used by function 2

makeCacheMatrix <- function(x = matrix()) {
  inval <- NULL             ## create the inverse object
  set <- function(y) {
    x <<- y                 ## <<- operator assigns a value in an
    inval <<- NULL          ## enviro different from current enviro
  }
  
  get <- function() x
  setInv <- function(inverse) inval <<- inverse  ## invoke inversion
  getInv <- function() inval  ## fetch the cached inverse that is inval
  
  list(set = set, get = get,  ## create a list function 2 can refer to
       setInv = setInv,
       getInv = getInv)
}


## Function 2: calculates the mean of the special vector; however
## it will first check to see if the mean was already calculated
## If YES it fetches the mean and skips ahead. If NO it calculates
## the mean of the data and sets the value of the mean in the cache
## via setmean

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inval <- x$getInv()
  
  if(!is.null(inval)) {             ## if inval is NOT null, get its value
    message("getting cached data")
    return(inval)
  }
  
  data <- x$get()
  inval <- solve(data, ...) ## swap in solve FUN to compute inverse of square matrix 
  x$setInv(inval)              ## if X=square invertible matrix, solve returns inverse
  inval
}
