input_template <- read.delim("data-raw/input_simulation_template.txt", stringsAsFactors = FALSE, header = FALSE)
colnames(input_template) <- NA

vignetteoutput <- read.csv2("vignettes/example_sim/readme_simfile.csv", sep = ",", header = TRUE)

usethis::use_data(input_template, vignetteoutput, internal = TRUE, overwrite = TRUE)
