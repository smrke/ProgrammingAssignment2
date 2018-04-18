## The two functions below are meant to be used together. 
## makeCacheMatrix takes as an argument a matrix, and returns a list 
## of functions (get, set, setinv and getinv) that can be used to act on that matrix
## if we store makeCacheMatrix as a variable, we can subset the variable
## to carry out the functions on the current value of the matrix

## the makeCacheMatrix function (modelled after the makeVector function) 
## creates the list of functions from the matrix, and because of lexical scoping
## allows us to perform these on the submitted matrix (or whatever it is eventually set to)


makeCacheMatrix <- function(x = matrix()) { #first we initialize an empty matrix
        #then we create a null inverse
        i <- NULL
        #then we define a set of 4 functions
        
        set <- function(y) { #set can be used to input a new matrix
                x <<- y
                i <<- NULL #importantly inputting a new matrix resets the inverse value to NULL to ensure we don't store an incorrect value
        }
        get <- function() x #get returns the matrix
        setinv <- function(inv) i <<- inv #setinv sets the inverse (but does no calculation, that's why we need cacheSolve)
        getinv <- function() i #getinv returns the "inverse value" which could be a matrix or NULL
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) #then we return a list of these 4 functions
}


## the cacheSovle function (modelled after the cacheMean function)
## takes the returned list of makeCacheMatrix as its argument
## first checks whether there is already an inverse by calling getinv()
## and if a non-null value exists, returns that with a printed message
## if there is not already a non-null value, it calculates it, stores
## the new value in makeCacheMatrix and returns it too

cacheSolve <- function(x, ...) { #cacheSolve takes x as its argument, where x has to be the output of the makeCacheMatrix function
        i <- x$getinv() #then we pull up the inverse value
        if(!is.null(i)) { # if the value is not null
                message("getting cached data") #display a message saying it's cached
                return(i) #return the existant value
        }
        #if, on the other hand, i is a NULL value, we calculate the inverse
        data <- x$get() 
        i <- solve(data, ...)
        x$setinv(i)
        i #and return the value
}



