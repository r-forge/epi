### Name: effx.match
### Title: Function to calculate effects for individually matched
###   case-control studies
### Aliases: effx.match
### Keywords: models regression

### ** Examples

library(Epi)
library(survival)
data(bdendo)

# d is the case-control variable, set is the matching variable.
# The variable est is a factor and refers to estrogen use (yes,no)
# The variable age is numeric and refers to estrogen use (yes,no)
# effect of est on the odds of being a case
effx.match(d,exposure=est,match=set,data=bdendo)
# effect of age on the odds of being a case
effx.match(d,exposure=age,match=set,data=bdendo)



