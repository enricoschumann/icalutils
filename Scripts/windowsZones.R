win <- trimws(readLines("https://raw.githubusercontent.com/unicode-org/cldr/master/common/supplemental/windowsZones.xml"))

win <- win[grep("<mapZone", win)]

from <- gsub('<mapZone other="([^"]+)" .*', '\\1', win)
to <- gsub('<mapZone other.*territory.*type="([^" ]+?)[" ].*', '\\1', win)

ii <- match(unique(from), from)
.tznames <- data.frame(Windows = from[ii],
                       Olson = to[ii],
                       stringsAsFactors = FALSE)


## license <- readLines("https://www.unicode.org/license.html")
## i <- grep("COPYRIGHT AND PERMISSION NOTICE", license)
## j <- grep("</pre>", license) - 1

## license <- paste0(paste("##", license[i:j]), collapse = "\n")
    

filename <- "~/Packages/icalutils/R/tznames.R"
cat("## The data in this file are auto-generated from file 'windowsZones.xml'\n",
    "## in the Unicode Common Locale Data Repository (http://cldr.unicode.org/).\n",
    "## See https://www.unicode.org/copyright.html and https://www.unicode.org/license.html.\n\n",
    file = filename, append = FALSE)
dump(".tznames", file = filename, append = TRUE)

library("formatR")
tidy_file("~/Packages/icalutils/R/tznames.R",
          width.cutoff = 20, wrap = FALSE)
