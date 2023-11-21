# 1

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# 2

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# Checking functions

# Create a sample matrix and a cache object for it
sample_matrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
cache_object <- makeCacheMatrix(sample_matrix)

# Calculate the inverse and time it
start_time <- Sys.time()
inv1 <- cacheSolve(cache_object)
end_time <- Sys.time()
cat("Time taken to calculate inverse first time: ", end_time - start_time, "\n")

# Get the cached inverse and time it
start_time <- Sys.time()
inv2 <- cacheSolve(cache_object)
end_time <- Sys.time()
cat("Time taken to retrieve cached inverse: ", end_time - start_time, "\n")

# Ensure the inv1 and inv2 are the same
stopifnot(identical(inv1, inv2))

# Modify the matrix and update the cache object
new_matrix <- matrix(c(2,3,4,5), nrow = 2, ncol = 2)
cache_object$set(new_matrix)

# Recalculate the inverse with the new matrix and time it
start_time <- Sys.time()
inv3 <- cacheSolve(cache_object)
end_time <- Sys.time()
cat("Time taken to calculate inverse after updating matrix: ", end_time - start_time, "\n")

# Ensure the new inverse is different from the previous one
stopifnot(!identical(inv1, inv3))
