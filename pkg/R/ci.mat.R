ci.mat <-
function( alpha=0.05 )
{
ciM <- rbind( c(1,1,1), qnorm(1-alpha/2)*c(0,-1,1) )
colnames( ciM ) <- c("Estimate", 
   paste( formatC( 100*   alpha/2 , format="f", digits=1 ), "%", sep="" ),
   paste( formatC( 100*(1-alpha/2), format="f", digits=1 ), "%", sep="" ) )
ciM
}
