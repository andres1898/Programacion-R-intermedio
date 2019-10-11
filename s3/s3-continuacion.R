

###########################################################


## http://www.cyclismo.org/tutorial/R/s3Classes.html#s3classesmethods


NorthAmerican <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  me <- list(
    hasBreakfast = eatsBreakfast,
    favoriteBreakfast = myFavorite
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"NorthAmerican")
  return(me)
}


bubba <- NorthAmerican()
bubba


louise <- NorthAmerican(eatsBreakfast=TRUE,myFavorite="fried eggs")
louise




NordAmericain <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## The Methods for this class normally go here but are discussed
    ## below. A simple placeholder is here to give you a teaser....
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"NordAmericain")
  return(me)
}


environment()

## ejem

bubba <- NordAmericain()

bubba



get("hasBreakfast",bubba$getEnv())

get("favoriteBreakfast",bubba$getEnv())


bubba <- NordAmericain(myFavorite="oatmeal")

bubba


get("favoriteBreakfast",bubba$getEnv())

louise <- bubba

assign("favoriteBreakfast","toast",louise$getEnv())

get("favoriteBreakfast",louise$getEnv())

get("favoriteBreakfast",bubba$getEnv())



setHasBreakfast <- function(elObjeto, newValue)
{
  print("Calling the base setHasBreakfast function")
  UseMethod("setHasBreakfast",elObjeto)
  print("Note this is not executed!")
}

setHasBreakfast.default <- function(elObjeto, newValue)
{
  print("You screwed up. I do not know how to handle this object.")
  return(elObjeto)
}


setHasBreakfast.NorthAmerican <- function(elObjeto, newValue)
{
  print("In setHasBreakfast.NorthAmerican and setting the value")
  elObjeto$hasBreakfast <- newValue
  return(elObjeto)
}



bubba <- NorthAmerican()

bubba$hasBreakfast

bubba <- setHasBreakfast(bubba,FALSE)

bubba$hasBreakfast

bubba <- setHasBreakfast(bubba,"No type checking sucker!")

bubba$hasBreakfast


## manejo del error

someNumbers <- 1:4

someNumbers

someNumbers <- setHasBreakfast(someNumbers,"what?")

someNumbers


getHasBreakfast <- function(elObjeto)
{
  print("Calling the base getHasBreakfast function")
  UseMethod("getHasBreakfast",elObjeto)
  print("Note this is not executed!")
}

getHasBreakfast.default <- function(elObjeto)
{
  print("You screwed up. I do not know how to handle this object.")
  return(NULL)
}


getHasBreakfast.NorthAmerican <- function(elObjeto)
{
  print("In getHasBreakfast.NorthAmerican and returning the value")
  return(elObjeto$hasBreakfast)
}



bubba <- NorthAmerican()
bubba <- setHasBreakfast(bubba,"No type checking sucker!")

result <- getHasBreakfast(bubba)

result

## el resto al ver environments
