
## From the Art

## An S3 class consists of a list, with a class name attribute and dispatch capability added. The latter enables the use of generic functions.

## S4 classes were developed later, with goal of adding safety, meaning that you cannot accidentally access a class component that is not already in existence.

## From http://www.stat.umn.edu/geyer/3701/notes/generic.html

#Everything in R is an object

#Every object has a class, whether or not it has been assigned one,



## funciones

?lm

x <- c(1,2,3)

y <- c(1,3,8)

lmout <- lm(y ~ x)


## ¿qué esperaríamos aquí?

class(lmout)

typeof(lmout)

str(lmout)

## funciones genericas

print

### y el ambiente?
environment(print)


## ?? y esto?
print(lm)

print(lmout)

unclass(lmout)

lmout


###

## objeto S3

j <- list(name="Joe", salary=55000, union=T)
class(j) <- "employee"
attributes(j) # let's check


#creando un objeto, paso a paso y una funcion generica print

test01  <- list()

test01$datos  <-  c(1,2,3)

test01$conteo  <-  5

test01$letras  <-  "This is ok"

class(test01)  <- "myFunk"

class(test01)

typeof(test01)

str(test01)

print.myFunk <- function(x, ...){
  "Si!!!!!!!!!!!!!!!!!!, lo logramos!!"
}

print(test01)

class(test01) <- c("myFunk" ,"A", "Bb", "CCc")


class(test01)

typeof(test01)

str(test01)

##  ??
## ¿qué debe pasar?

print(test01)



print.A <- function(x, ...){
  "ni idea si lo logramos!!"
}

print(test01)





print.myFunk.A <- function(x, ...){
  "ni idea si esta vez si lo logramos!!"
}


print(test01)

class(test01)





## veamos los metodos

methods(print)


## para nuestros empleados

print.employee <- function(wrkr) {
cat(wrkr$name,"\n")
cat("salary",wrkr$salary,"\n")
cat("union member",wrkr$union,"\n")
}

print(j)

## y para los asteriscos [no visible]

 getAnywhere()


######################################

## para los ratos libres

### Adv-R

######{r, include = FALSE}
source("common.R")
######
######{r setup, messages = FALSE}
library(sloop)
######
######{r}
f <- factor(c("a", "b", "c"))

typeof(f)
attributes(f)
######
######{r}
unclass(f)
######
######{r}
ftype(print)
ftype(str)
ftype(unclass)
######
######{r}
print(f)

# stripping class reverts to integer behaviour
print(unclass(f))
######
######{r}
time <- strptime(c("2017-01-01", "2020-05-04 03:21"), "%Y-%m-%d")
str(time)

str(unclass(time))
######
######{r}
s3_dispatch(print(f))
######
######{r}
ftype(t.test)
ftype(t.data.frame)
######
######{r, error = TRUE}
weighted.mean.Date

s3_get_method(weighted.mean.Date)
######
    ######{r}
    set.seed(1014)
    some_days <- as.Date("2017-01-31") + sample(10, 5)

    mean(some_days)
    mean(unclass(some_days))
    ######
    ######{r}
    x <- ecdf(rpois(100, 10))
    x
    ######
    ######{r}
    x <- table(rpois(100, 5))
    x
    ######
######{r}
# Create and assign class in one step
x <- structure(list(), class = "my_class")

# Create, then set class
x <- list()
class(x) <- "my_class"
######
######{r}
class(x)
inherits(x, "my_class")
inherits(x, "your_class")
######
######{r, error = TRUE}
# Create a linear model
mod <- lm(log(mpg) ~ log(disp), data = mtcars)
class(mod)
print(mod)

# Turn it into a date (?!)
class(mod) <- "Date"

# Unsurprisingly this doesn't work very well
print(mod)
######
######{r}
new_Date <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "Date")
}

new_Date(c(-1, 0, 1))
######
######{r}
new_difftime <- function(x = double(), units = "secs") {
  stopifnot(is.double(x))
  units <- match.arg(units, c("secs", "mins", "hours", "days", "weeks"))

  structure(x,
    class = "difftime",
    units = units
  )
}

new_difftime(c(1, 10, 3600), "secs")
new_difftime(52, "weeks")
######
######{r, error = TRUE}
new_factor <- function(x = integer(), levels = character()) {
  stopifnot(is.integer(x))
  stopifnot(is.character(levels))

  structure(
    x,
    levels = levels,
    class = "factor"
  )
}

new_factor(1:5, "a")
new_factor(0:1, "a")
######
######{r, error = TRUE}
validate_factor <- function(x) {
  values <- unclass(x)
  levels <- attr(x, "levels")

  if (!all(!is.na(values) & values > 0)) {
    stop(
      "All `x` values must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  if (length(levels) < max(values)) {
    stop(
      "There must be at least as many `levels` as possible values in `x`",
      call. = FALSE
    )
  }

  x
}

validate_factor(new_factor(1:5, "a"))
validate_factor(new_factor(0:1, "a"))
######
    ######{r, error = TRUE}
    new_difftime(1:10)
    ######
    ######{r}
    difftime <- function(x = double(), units = "secs") {
      x <- as.double(x)
      new_difftime(x, units = units)
    }
    
    difftime(1:10)
    ######
    ######{r, error = TRUE}
    factor <- function(x = character(), levels = unique(x)) {
      ind <- match(x, levels)
      validate_factor(new_factor(ind, levels))
    }
    
    factor(c("a", "a", "b"))
    ######
    ######{r}
    POSIXct <- function(year = integer(), 
                        month = integer(), 
                        day = integer(), 
                        hour = 0L, 
                        minute = 0L, 
                        sec = 0, 
                        tzone = "") {
      ISOdatetime(year, month, day, hour, minute, sec, tz = tzone)
    }
    
    POSIXct(2020, 1, 1, tzone = "America/New_York")
    ######
######{r}
mean
######
######{r}
my_new_generic <- function(x) {
  UseMethod("my_new_generic")
}
######
######{r}
x <- Sys.Date()
s3_dispatch(print(x))
######
######{r}
x <- matrix(1:10, nrow = 2)
s3_dispatch(mean(x))

s3_dispatch(sum(Sys.time()))
######
######{r}
s3_methods_generic("mean")

s3_methods_class("ordered")
######
    ######{r, results = FALSE}
    x <- structure(1:10, class = "test")
    t(x)
    ######
    ######{r}
    g <- function(x) {
      x <- 10
      y <- 10
      UseMethod("g")
    }
    g.default <- function(x) c(x = x, y = y)

    x <- 1
    y <- 1
    g(x)
    ######
    ######{r}
    x <- as.POSIXlt(ISOdatetime(2020, 1, 1, 0, 0, 1:3))
    x
    
    length(x)
    length(unclass(x))
    
    x[[1]] # the first date time
    unclass(x)[[1]] # the first component, the number of seconds
    ######
    ######{r}
    x <- data.frame(x = 1:100, y = 1:100)
    length(x)
    nrow(x)
    ######
    ######{r}
    mod <- lm(mpg ~ wt, data = mtcars)
    length(mod)
    ######
    ######{r}
    class(ordered("x"))
    class(Sys.time())
    ######
    ######{r}
    s3_dispatch(print(ordered("x")))
    s3_dispatch(print(Sys.time()))
    ######
    ######{r}
    s3_dispatch(ordered("x")[1])
    s3_dispatch(Sys.time()[1])
    ######
######{r}
new_secret <- function(x = double()) {
  stopifnot(is.double(x))
  structure(x, class = "secret")
}

print.secret <- function(x, ...) {
  print(strrep("x", nchar(x)))
  invisible(x)
}

x <- new_secret(c(15, 1, 456))
x
######
######{r}
s3_dispatch(x[1])
x[1]
######
######{r}
`[.secret` <- function(x, i) {
  new_secret(x[i])
}
######
######{r}
`[.secret` <- function(x, i) {
  x <- unclass(x)
  new_secret(x[i])
}
x[1]
######
######{r}
`[.secret` <- function(x, i) {
  new_secret(NextMethod())
}
x[1]
######
######{r}
s3_dispatch(x[1])
######
######{r}
new_secret <- function(x, ..., class = character()) {
  stopifnot(is.double(x))

  structure(
    x,
    ...,
    class = c(class, "secret")
  )
}
######
######{r}
new_supersecret <- function(x) {
  new_secret(x, class = "supersecret")
}

print.supersecret <- function(x, ...) {
  print(rep("xxxxx", length(x)))
  invisible(x)
}

x2 <- new_supersecret(c(15, 1, 456))
x2
######
######{r}
`[.secret` <- function(x, ...) {
  new_secret(NextMethod())
}

x2[1:3]
######
######{r}
vec_restore.secret <- function(x, to, ...) new_secret(x)
vec_restore.supersecret <- function(x, to, ...) new_supersecret(x)
######
######{r}
`[.secret` <- function(x, ...) {
  vctrs::vec_restore(NextMethod(), x)
}
x2[1:3]
######
    ######{r, eval = FALSE}
    generic2 <- function(x) UseMethod("generic2")
    generic2.a1 <- function(x) "a1"
    generic2.a2 <- function(x) "a2"
    generic2.b <- function(x) {
      class(x) <- "a1"
      NextMethod()
    }

    generic2(structure(list(), class = c("b", "a2")))
    ######
######{r}
class(matrix(1:5))
######
######{r}
s3_class(matrix(1:5))
######
######{r}
s3_dispatch(print(matrix(1:5)))
######
######{r}
x1 <- 1:5
class(x1)
s3_dispatch(mean(x1))

x2 <- structure(x1, class = "integer")
class(x2)
s3_dispatch(mean(x2))
######
######{r}
s3_dispatch(Sys.time()[1])
######
######{r}
s3_dispatch(sum(Sys.time()))
######
######{r}
y <- as.difftime(10, units = "mins")
s3_dispatch(abs(y))
######
######{r}
Math.difftime <- function(x, ...) {
  new_difftime(NextMethod(), units = attr(x, "units"))
}
######
######{r}
date <- as.Date("2017-01-01")
integer <- 1L

date + integer
integer + date
######
    ######{r}
    length.integer <- function(x) 10
    
    x1 <- 1:5
    class(x1)
    s3_dispatch(length(x1))
    
    x2 <- structure(x1, class = "integer")
    class(x2)
    s3_dispatch(length(x2))
    ######
