# TODO: Calculate the standard error
# 
# Author: vidal
###############################################################################


se <-
		function(x)
{
	return(sqrt(var(x)/length(x)))
}

