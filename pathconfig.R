# ###########################
# CONFIG: define paths and filenames for later reference
# ###########################

# Change the basepath depending on your system

basepath <- rprojroot::find_root(rprojroot::has_file("LICENSE"))

# Main directories
datadir  <- file.path(basepath, "data")
acquired <- file.path(datadir,"acquired")
interwrk <- file.path(datadir,"interwrk")
generated<- file.path(datadir,"generated")
programs <- file.path(basepath,"programs")

for ( dir in list(datadir,acquired,interwrk,generated)){
	if (file.exists(dir)){
	} else {
	dir.create(file.path(dir))
	}
}
