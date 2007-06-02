### Name: icut
### Title: Function to cut the follow-up in cohort at a point in time.
### Aliases: icut
### Keywords: manip datagen

### ** Examples

one <- round( runif( 15, 0, 15 ), 1 )
two <- round( runif( 15, 0, 15 ), 1 )
doe <- pmin( one, two )
dox <- pmax( one, two )
# Goofy data rows to test possibly odd behaviour
doe[1:3] <- dox[1:3] <- 8
dox[2] <- 6
dox[3] <- 7.5
# Some failure indicators
fail <- sample( 0:1, 15, replace=TRUE, prob=c(0.7,0.3) )
# So what have we got
data.frame( doe, dox, fail )
# Cut follow-up at 5
icut( doe, dox, fail, cut=5 )



