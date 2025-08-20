# Vignette precompiler for cs9 package
# This script converts .Rmd.orig files to .Rmd files for package building

# List of vignettes to precompile
vignettes <- c(
  "cs9",
  "creating-a-task", 
  "file-layout",
  "getting-started"
)

# Precompile each vignette
for (vignette in vignettes) {
  input_file <- paste0("vignettes/", vignette, ".Rmd.orig")
  output_file <- paste0("vignettes/", vignette, ".Rmd")
  
  if (file.exists(input_file)) {
    cat("Precompiling:", input_file, "->", output_file, "\n")
    knitr::knit(input_file, output_file)
  } else {
    cat("Warning: Input file not found:", input_file, "\n")
  }
}

cat("Vignette precompilation complete!\n")