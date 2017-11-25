# The levels method is already defined (in the utils package)
# and hence imported in the NAMESPACE file
levels.Lexis <-
function( x )
{
union( levels(x$lex.Cst), levels(x$lex.Xst) )
}

# The Relevel method
Relevel <- function (x, ...) UseMethod("Relevel")

# Utility to group a factor from a 2-column table;
# called from Relevel if second argument is a matrix or table
tabRelevel <-
function( ff, # factor to be grouped
          gg, # 2-column matrix or table with levels resp. grouping
     xlevels = TRUE ) # include also grouped levels not present in ff
{
if( any( is.na( match( unique(as.character(ff)),
                       unique(as.character(gg[,1])) ) ) ) )
    stop( "All values of x must be in ref[,1].\n" )
if( any( wh <- apply( table( gg[,1], gg[,2] )>0, 1, sum )>1 ) )
    stop( "Factor level", names(wh)[wh], "is grouped to more than one group.\n" )
if( any( wh <- apply( table( gg[,1], gg[,2] ), 1, sum )>1 ) )
    warning( "Factor level", names(wh)[wh], "appear more than once.\n" )
# indices of the original factor levels
ixff <- as.integer( ff )
# where they are in the translation table
ixg1 <- as.integer( factor( gg[,1], levels=levels(ff) ) )
# indices of the new levels in the translation table
ixg2 <- as.integer( g2 <- factor(gg[,2]) )
# where in ixg2 are the integers ixff - match() 
grff <- factor( ixg2[match(ixff,ixg1)], labels=levels(g2) )
# keep all levels from second column or not?
if( xlevels ) grff else factor(grff)
}

# The factor method is the default method
Relevel.default <-
Relevel.factor <-
  function( x, ref, first=TRUE, collapse="+", xlevels=TRUE, ... )
  {
  # Function that collapses multiple sets of levels of a factor
  #
  # If ref is a 2-dim structure
  if( is.matrix(ref) |
      is.table(ref) |
      is.array(ref) |
      is.data.frame(ref) )
      {
   if( length(dim(ref)) !=2 ) stop("ref must be 2-dimensional\n")    
   return( tabRelevel( x, ref, xlevels ) )    
  } else {
      
  # Otherwise use the old version
  #    
  if( !is.factor(x) )
    {
    argnam <- deparse( substitute(x) )
    f <- factor( x )
    cat( "WARNING: ", argnam,
         "has been converted to a factor with levels:\n",
         levels( f ) )
    }
  else
    f <- x

  # This is a copy of the relevel function from the base package:
  #
  relev <- function (x, ref, ...)
  {
    lev <- levels(x)
    if ( is.character( ref ) )
         ref <- match(ref, lev)
    if ( any( is.na( ref ) ) )
         stop( "any values in ref must be an existing level\n" )
    nlev <- length( lev )
    if ( any( ref < 1 ) || any( ref > nlev ) )
         stop( paste( "ref=", paste( ref, collapse="," ),
                      ": All elements must be in 1:", nlev, sep="" ) )
    factor(x, levels = lev[c(ref, seq(along = lev)[-ref])])
  }

  # If called with a non-list argument assume reshuffling of levels
  #
  if( !is.list( ref ) )
    fnew <- relev( f, ref )

  # If called with a list collapse levels in each list element.
  #
  if( is.list( ref ) )
    {
      fnew <- f
      newnames <- levels( f )
      uninames <- character( length( ref ) )
      for( s in 1:length(ref) )
         if ( is.character( ref[[s]] ) ) ref[[s]] <- match( ref[[s]], levels(f) )
      # Check for replicates in levels to be grouped
      if( any( (tt<-table(unlist(ref))) > 1 ) )
        stop("Factor level(s) ", levels( f )[as.numeric(names(tt)[tt>1])],
             " has been allocated to more than one new level.")
      for( s in 1:length(ref) )
         {
         uninames[s] <- if( is.null( names( ref ) ) )
                          {
                          paste( levels( f )[ref[[s]]], collapse=collapse )
                          }
                        else if( names( ref )[s]=="" )
                          {
                          paste( levels( f )[ref[[s]]], collapse=collapse )
                          }
                        else names( ref )[s]
          newnames[ref[[s]]] <- rep( uninames[s], length( ref[[s]] ) )
          }
      levels( fnew ) <- newnames
      if( !is.null( first ) )
        {
      if( !first ) fnew <- factor( fnew, c( levels( f )[-unlist( ref )], uninames ) )
      if(  first ) fnew <- factor( fnew, c( uninames, levels( f )[-unlist( ref )] ) )
        }
    }

  # This is in order to merge levels with identical names
  #
  return( factor( fnew, levels=levels(fnew) ) )
  }
}
