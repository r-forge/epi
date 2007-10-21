Poisson.Lexis <-
function( x, st1, st2, model )
{
from <- st1
to   <- st2
resp.nam <- paste( "state", st2, sep="" )
assign( resp.nam, ( x[,"lex.status2"]==to )*1 )
model.form <- as.formula( paste( resp.nam, " ~ ", model ) )
print( model.form )
glm( model.form,
     offset=log( x$lex.deltat ),
     family=poisson,
     data=subset(x,lex.status1==from) )
}