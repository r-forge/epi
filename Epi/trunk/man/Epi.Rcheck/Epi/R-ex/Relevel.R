### Name: Relevel
### Title: Reorder and combine levels of a factor
### Aliases: Relevel
### Keywords: manip

### ** Examples

ff <- factor( sample( letters[1:5], 100, replace=TRUE ) )
table( ff, Relevel( ff, list( AB=1:2, "Dee"=4, c(3,5) ) ) )
table( ff, rr=Relevel( ff, list( 5:4, Z=c("c","a") ), coll="-und-", first=FALSE ) )



