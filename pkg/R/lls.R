lls <-
# A function that expands the functionality of ls()
function( pos = 1, pat = "" )
{
# First a function that returns length/dim when you ask for it
dimx <- function(dd) if (is.null(dim(dd))) length(dd) else dim(dd)
# A vector of object names
lll <- ls( pos=pos, pat=pat )
# Are there any objects at all?
if( length(lll) > 0 )
{
obj.mode <-
obj.cls <-
obj.dim <- character(0)
# Then find mode, class, name and dimension of them and return it
for(i in 1:length(lll))
{
obj.mode[i] <-        eval( parse(t = paste( "mode(", lll[i], ")")))
obj.cls[i]  <- paste( eval( parse(t = paste("class(", lll[i], ")"))), collapse=" " )
obj.dim[i]  <- paste( eval( parse(t = paste( "dimx(", lll[i], ")"))), collapse=" " )
}
data.frame( name=lll,
            mode=obj.mode,
           class=obj.cls,
             dim=obj.dim )
}
}

