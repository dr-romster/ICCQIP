

### ---- package management and initialisation ----
required_pkgs <- c("dplyr", "readxl", "ggplot2",
                   #"ggbump", # removed from CRAN Dec 2025!
                   "ggrepel",
                   "ggtext",
                   'MetBrewer',
                   "patchwork",
                   "showtext")

missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
missing_pkgs
if(length(missing_pkgs) > 0){ 
  stop(paste("Please install ", 
             missing_pkgs, " before proceeding.\n ", sep = ""))
}

if(length(missing_pkgs)>0) {
  install.packages(missing_pkgs, dependencies = FALSE)
}

install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggtext")
install.packages("patchwork")
install.packages("MetBrewer")
install.packages("showtext")
# 
# invisible(lapply(required_pkgs, library, character.only = TRUE))

# install ggbump from CRAN archive
install.packages("https://cran.r-project.org/src/contrib/Archive/ggbump/ggbump_0.1.0.tar.gz")


# for those who like to use R environments considering using the 
# following:
install.packages("renv")
renv::init()
renv::snapshot()
#renv::restore()
