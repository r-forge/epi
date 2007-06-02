### Name: apc.plot
### Title: Plot the estimates from a fitted Age-Period-Cohort model
### Aliases: apc.plot
### Keywords: hplot

### ** Examples

data( lungDK )
attach( lungDK )
apc1 <- apc.fit( A=Ax, P=Px, D=D, Y=Y/10^5 )
fp <- apc.plot( apc1 )
apc.lines( apc1, frame.par=fp, drift=1.01, col="red" )
for( i in 1:11 )
  apc.lines( apc1, frame.par=fp, drift=1+(i-6)/100, col=rainbow(12)[i] )



