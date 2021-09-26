## The render code to output the README.md file
## This code assumes that the working directory is 1 level up from the /_Rmd directory

rmarkdown::render("../_Rmd/README.Rmd", 
                  output_format = "github_document",
                  output_dir = "./", 
                  output_options = list(
                    toc = FALSE, 
                    html_preview = FALSE, 
                    keep_html = FALSE
                  )
)