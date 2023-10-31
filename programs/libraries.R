####################################
# global libraries used everywhere #
####################################

#mran.date <- "2019-09-01"
#options(repos=paste0("https://cran.microsoft.com/snapshot/",mran.date,"/"))
# we use the rocker Docker image, but if run locally, need to use a switch here


pkgTest <- function(x)
{
	if (!require(x,character.only = TRUE))
	{
		install.packages(x,dep=TRUE)
		if(!require(x,character.only = TRUE)) stop("Package not found")
	}
	return("OK")
}

global.libraries <- c("dplyr","stringr","devtools","rprojroot","tictoc","DBI",
                      "RSQLite","skimr","tidyr")

results <- sapply(as.list(global.libraries), pkgTest)
