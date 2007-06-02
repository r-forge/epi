### Name: ex1
### Title: Split follow-up time along a timescale
### Aliases: ex1
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
# Split follow-up:
ex1( doe, dox, fail, breaks=0:10 )



