#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(h2o)
h2o.init()

huismodelRF = readRDS("huismodelRF.RDs")


#* @apiTitle Mijn huisprijs voorspeller

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /huiswaarde
function(a, b) {
  print(huismodelRF)
    as.numeric(a) + as.numeric(b)
}
