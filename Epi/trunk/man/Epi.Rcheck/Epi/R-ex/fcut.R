### Name: fcut
### Title: Cuts follow-up time at multiple failure times.
### Aliases: fcut
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
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# Failure times in a list
dof <- sample( c(one,two), 15 )
l.dof <- list( f1=sample( c(one,two), 15 ),
               f2=sample( c(one,two), 15 ),
               f3=sample( c(one,two),15 ) )
# The same, but with events prior to entry removed
lx.dof <- lapply( l.dof, FUN=function(x){ x[x<doe] <- NA ; x } )
# So what have we got
data.frame( doe, dox, fail, l.dof, lx.dof )
# Cut follow-up at event times
fcut( doe, dox, lx.dof, fail, data=data.frame( doe, dox, lx.dof ) )



