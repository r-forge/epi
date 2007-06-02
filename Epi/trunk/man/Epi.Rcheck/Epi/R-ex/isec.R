### Name: isec
### Title: Determine the intersection between follow-up intervals and a
###   fixed interval.
### Aliases: isec
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
# So what have we got?
data.frame( doe, dox, fail )
# Find intersection with interval (4,8)
isec( doe, dox, fail, int=c(4,8) )
# See how it compares to original data
merge( data.frame( Expand=1:15, doe, dox, fail ),
       data.frame( isec( doe, dox, fail, int=c(4,8) ) ), all=TRUE )
  


