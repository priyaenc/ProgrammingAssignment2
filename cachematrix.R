rm(list = ls())
## Let's learn how to calculate inverse of Matrix
## For example we have below Matrix and we wnat to find out it's inverse:
##      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# =1/1*4-3*2 [ 4   -3]
#            [ -2   1]

# Output will be:
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Now, let's write function for the same


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y #Note here we have used scoping assignment. This value can be accessed outside function also
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) { inv = x$getinv()

if (!is.null(inv)){
  message("Fetching from Cache Memory")
  return(inv)
}

mat.data = x$get()
inv = solve(mat.data, ...) # This is the direct function available in R. Methods in Package Matrix for Function solve()
#use ?solve for more information.
x$setinv(inv)
return(inv)      ## Return a matrix that is the inverse of 'x'
}



# To test the above function:

mat = matrix(c(20:1), nrow=2, ncol=2)
mat
temp = makeCacheMatrix(mat)
mat
# Output
# > mat
# [,1] [,2]
# [1,]   20   18
# [2,]   19   17
cacheSolve(temp)
# Output
# > cacheSolve(temp)
# [,1] [,2]
# [1,] -8.5    9
# [2,]  9.5  -10
# When you run cacheSolve(temp) again. It will directly fetch from the memory
cacheSolve(temp)
# Output
# Fetching from Cache Memory
# [,1] [,2]
# [1,] -8.5    9
# [2,]  9.5  -10

