## R - Redis connection using redux package ##

require(redux)
# install packages if require failed
# install.packages('redux')

r <- hiredis()
r

# Setting simple string foo -> bar
r$SET('foo', 'bar')
r$GET('foo')
r$SET('hello', 'world')

# Setting list hello world
r$RPUSH('mylist', 'hello')
r$RPUSH('mylist', 'world')

## Getting list of character
mylist <- r$LRANGE('mylist',0,-1)
class(mylist)

## Convert list into char vector
mylist <- unlist(mylist)
mylist
class(mylist)

## DELETE list
r$DEL('mylist')

