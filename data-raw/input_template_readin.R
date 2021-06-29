input_template <- read.delim("data-raw/input_simulation_template.txt", stringsAsFactors = FALSE, header = FALSE)
colnames(input_template) <- NA
usethis::use_data(input_template, internal = TRUE, overwrite = TRUE)
