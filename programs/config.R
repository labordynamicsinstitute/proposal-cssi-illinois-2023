#Any libraries needed are called and if necessary installed through `libraries.R`:

# Kranz files with filenames
kranz.src <- "http://econ.mathematik.uni-ulm.de/ejd/files.zip"
kranz.src2<- "http://econ.mathematik.uni-ulm.de/ejd/articles.zip"
kranz.zip <- file.path(acquired,"kranz_files.zip") 
kranz.zip2 <- file.path(acquired,"kranz_articles.zip") 
kranz.sql <- file.path(interwrk,"files.sqlite")
kranz.sql2 <- file.path(interwrk,"articles.sqlite")
