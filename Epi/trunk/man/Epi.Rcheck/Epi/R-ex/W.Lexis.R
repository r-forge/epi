### Name: W.Lexis
### Title: Split follow-up time in cohort studies.
### Aliases: W.Lexis
### Keywords: manip

### ** Examples

# A small bogus cohort
#
xcoh <- structure( list( id = c("A", "B", "C"),
                      birth = c("14/07/1952", "01/04/1954", "10/06/1987"),
                      entry = c("04/08/1965", "08/09/1972", "23/12/1991"),
                       exit = c("27/06/1997", "23/05/1995", "24/07/1998"),
                       fail = c(1, 0, 1) ),
                     .Names = c("id", "birth", "entry", "exit", "fail"),
                  row.names = c("1", "2", "3"),
                      class = "data.frame" )

# Convert the character dates into numerical variables (fractional years)
#
xcoh$bt <- cal.yr( xcoh$birth, format="%d/%m/%Y" )
xcoh$en <- cal.yr( xcoh$entry, format="%d/%m/%Y" )
xcoh$ex <- cal.yr( xcoh$exit , format="%d/%m/%Y" )

# See how it looks
#
xcoh 

# Split time along one time-axis
#
W.Lexis( entry = en,
          exit = ex,
          fail = fail,
         scale = 1,
        origin = bt,
        breaks = seq( 5, 40, 5 ),
       include = list( bt, en, ex, id ),
          data = xcoh )

# Split time along two time-axes
#
( x2 <- 
W.Lexis( entry = en,
          exit = ex,
          fail = fail,
         scale = 1,
        origin = list( per=0,                 age=bt          ),
        breaks = list( per=seq(1900,2000,10), age=seq(0,80,5) ),
       include = list( bt, en, ex, id ),
          data = xcoh ) )

# Tabulate the cases and the person-years
#
tapply( x2$Fail, list( x2$age, x2$per ), sum )
tapply( x2$Exit - x2$Entry, list( x2$age, x2$per ), sum )



