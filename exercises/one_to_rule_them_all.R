path <- paste0(here(), "/exercises/")

source_files <- list.files(path, "*.R$")
source_files <- source_files[source_files != "save-all.R"]
source_files <- source_files[source_files != "plot-save.R"]

for (file in source_files) {
  source(paste0(path, file))
  print(file)
}
