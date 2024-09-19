# Load required library
library(knitr)

# Specify the input .R file and output .qmd file
r_file <- "LSOA_example2.R"
qmd_file <- "output_document.qmd"

# Convert R to Rmd
rmd_file <- tempfile(fileext = ".Rmd")
spin_output <- spin(r_file, knit = FALSE, format = "Rmd")
writeLines(spin_output, rmd_file)

# Read the Rmd content
rmd_content <- readLines(rmd_file)

# Replace the YAML header for Word output
yaml_header <- c("---",
                 "title: \"Your Title Here\"",
                 "format: docx",
                 "---",
                 "")

# Combine the new YAML header with the rest of the content
qmd_content <- c(yaml_header, rmd_content)

# Write the Qmd file
writeLines(qmd_content, qmd_file)

# Clean up temporary file
file.remove(rmd_file)

cat("Conversion complete. Output file:", qmd_file, "\n")