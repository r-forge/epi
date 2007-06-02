### Name: fcut1
### Title: Cut follow-up time at a failure time.
### Aliases: fcut1
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 10 ), 1 )
two <- round( runif( 15, 0, 10 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators and failure times
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
dof <- sample( c(one,two), 15 )
# So what have we got
data.frame( doe, dox, fail, dof )
# Cut follow-up at dof
fcut1( doe, dox, fail, dof )



