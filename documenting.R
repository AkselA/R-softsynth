linewrap <- function(txt, width=90, strip.cr=FALSE, add.cr=TRUE) {
	lin <- paste(txt, collapse=" ")
	lin <- gsub("#'", " ", lin)
	if (strip.cr) lin <- gsub("\\\\cr", "", lin)
	lin <- gsub("[ ]+", " ", lin)
	words <- strsplit(lin, '\\{[^}]+ (*SKIP)(*FAIL)| ', perl=TRUE)[[1]]
	words <- words[!nchar(words) == 0]
	words.clean <- gsub("\\\\[a-zA-Z]+\\{([^}]+)\\}", "\\1", words)
	words.length <- nchar(words.clean) + 1
	words.length[words.clean == "\\cr"] <- 0
	
	llist <- vector()
	llength <- 0
	
	for (i in seq_along(words.length)) {
		llength <- llength + words.length[i]
		if (length(words.clean[i-1]) != 0 && words.clean[i-1] == "\\cr") {
		    llength <- width + 1
		}
		if (llength > width) {
			llength <- words.length[i]
		}
		llist[[i]] <- llength
	}
	words[words.clean == "\\cr"] <- ""
	lline <- cumsum(diff(c(0, llist)) < 0) + 1
	llines <- aggregate(words, list(lline), paste, collapse=" ")[[2]]
	llines <- sub(" +$", "", llines)
	llines <- sub("^ +", "", llines)
	
	if (add.cr) llines <- paste(llines, "\\cr")
	
	llines
}

# turns text into roxygen2 comments
# cut/copy text, run roxcomm(), paste
roxcomm <- function(action="add", max.width=0, strip.cr=FALSE, add.cr=FALSE) {
    action <- match.arg(action, c("revert", "add", "detab"))
    pat <- switch(action,
                  revert=c("^#'[ ]*", ""),
                     add=c("(.*)", "#' \\1"),
                   detab=c("\t", "    "))

    copy <- pipe("pbpaste")
    lines <- readLines(copy)
    
    if (max.width > 0) {
    	lines <- linewrap(lines, width=max.width, strip.cr=strip.cr, add.cr=add.cr)
    }
    
    lines <- gsub(pat[1], pat[2], lines)

    clip <- pipe("pbcopy", "w")                       
    writeLines(text=lines, con=clip)                               

    close(clip)
    close(copy)
}
# roxcomm("add", 0)
# roxcomm("add", 80, add.cr=TRUE)


require(roxygen2)
require(devtools)

setwd("~/Documents/R/prosjekter")
projname <- "softsynth"
# create(projname)

# turns objects found in "projname"/data.R (project root)
# into data files available through data()
# by saving them as .rda files in "projname"/data
add_data <- function(projname) {
	if (!dir.exists(projname)) {
		stop(paste("Could not find", projname, "in current directory"))
	}
	datapath <- file.path(projname, "data.R")
	if (!file.exists(datapath)) {
		stop(paste("Could not find", datapath))
	}
    dir.create(file.path(projname, "data"), showWarnings=FALSE)
    tmp.env <- new.env()
	source(datapath, local=tmp.env)
	tmp.list <- as.list(tmp.env, sorted=TRUE)
    files <- file.path(projname, "data", paste0(names(tmp.list), ".rda"))
    obj <- mapply(save, list=names(tmp.list), file=files, 
      MoreArgs=list(compress="xz", envir=tmp.env))
    if (length(files) == 1) {
        cat("File added:")
    } else {
    	cat("Files added:")
    }
    dtf <- data.frame(x=paste(files, ""), 
      y=paste(sprintf("%.1f", file.size(files)/1000), "kB"))
    names(dtf) <- c(" ", " ")
    dtf
}

load_all(projname)
add_data(projname)
document(projname)

# unload(projname)
use_build_ignore(c("data.R", "documenting.R", "commit.command"), pkg=projname)

# check(projname, manual=TRUE)

show_pdf <- function(package, lib.loc=NULL, opt="--force") { 
     path <- find.package(package, lib.loc) 
     system(paste(shQuote(file.path(R.home("bin"), "R")), 
                  "CMD", "Rd2pdf", paste(opt, collapse=" "),
                  shQuote(path))) 
} 
show_pdf(projname)


# run convenience script to add, commit and maybe push change
system(paste0("open ", projname, "/commit.command"))

# dev_example(projname)

install_github(paste0("AkselA/R-", projname))
library(projname, character.only=TRUE)

