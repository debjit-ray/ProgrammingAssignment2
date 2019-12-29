## This is the sample explained. Uploading just for my future reference.
# -------------------------------------------------------------------------------------------------------------------
# Function Name - makeVector()
# -------------------------------------------------------------------------------------------------------------------
    # the formals part of the function declaration define the default value of x as an empty numeric vector.
    # Initialization of the vector with a default value is important because without a default value, 
    # data <- x$get() generates the following error message.
    # Error in x$get() : argument "x" is missing, with no default
makeVector <- function(x = numeric()) {
    m <- NULL

    # When set() is executed, it does two things:
    #   a. Assign the input argument to the x object in the parent environment, and
    #   b. Assign the value of NULL to the m object in the parent environment. 
    #      This line of code clears any value of m that had been cached by a prior execution of cachemean().
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Since the symbol x is not defined within get(), R retrieves it from the parent environment of makeVector().    
    get <- function() {x}

    # Since m is defined in the parent environment and we need to access it after setmean() completes, 
    # the code uses the <<- form of the assignment operator to assign the input argument to the value of m 
    # in the parent environment.
    setmean <- function(mean) {m <<- mean}
    
    # Since the symbol m is not defined within get(), R retrieves it from the parent environment of makeVector().  
    getmean <- function() {m}

    # The last section of code assigns each of these functions as an element within a list(), 
    # and returns it to the parent environment.
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    # When the function ends, it returns a fully formed object of type makeVector() to be used by downstream R code.
}

# -------------------------------------------------------------------------------------------------------------------
# Function Name - cachemean()
# -------------------------------------------------------------------------------------------------------------------
    # Without cachemean(), the makeVector() function is incomplete. 
    # As designed, cachemean() is required to populate and/or retrieve the mean from an object of type makeVector().
cachemean <- function(x, ...) {

    # The function attempts to retrieve a mean from the object passed in as the argument. 
    # First, it calls the getmean() function on the input object.
    m <- x$getmean()
    
    # Then it checks to see whether the result is NULL. 
    # Since makeVector() sets the cached mean to NULL whenever a new vector is set into the object, 
    # if the value here is not equal to NULL, we have a valid, cached mean and can return it to the 
    # parent environment
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }    

    # If the result of !is.null(m) is FALSE, cachemean() gets the vector from the input object, 
    # calculates a mean(), uses the setmean() function on the input object to set the mean in the input object, 
    # and then returns the value of the mean to the parent environment by printing the mean object.    
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m    
    
    # Note that cachemean() is the only place where the mean() function is executed, 
    # which is why makeVector() is incomplete without cachemean().    
}

# Execuction
# 
# aVector <- makeVector(1:10)
# aVector$get()               # retrieve the value of x
# aVector$getmean()           # retrieve the value of m, which should be NULL
# aVector$set(30:50)          # reset value with a new vector
# cachemean(aVector)          # notice mean calculated is mean of 30:50, not 1:10
# aVector$getmean()           # retrieve it directly, now that it has been cached