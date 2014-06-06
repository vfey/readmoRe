# TODO: Check if a given number is a whole number
# 
# Author: vidal
###############################################################################


is.wholenumber <-
        function(x, tol = .Machine$double.eps^0.5)
{
        abs(x - round(x)) < tol
}
