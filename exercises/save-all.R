library(here)

path <- paste0(here(), "/exercises/")
source_files <- list.files(path, "*.R$")
source_files <-
    source_files[source_files != c("plot-save.R", "save-all.R")]
files <- paste0(path, source_files)

for (f in files) {
    source(f)
}

files
