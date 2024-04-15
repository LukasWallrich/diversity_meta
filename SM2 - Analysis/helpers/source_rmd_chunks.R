

source_rmd_chunks <- function(file, chunk_labels, skip_plots = TRUE, output_temp = FALSE){

  extract_code_chunks <- function(filepath) {
    content <- read_file(filepath)

    pattern <- "(?s)```\\{r.*?```" # (?s) makes . match newlines

    code_chunks <- str_extract_all(content, pattern, simplify = FALSE) %>% unlist()

    code_chunks[nzchar(code_chunks)] # Return non-empty code chunks

  }

  text <- extract_code_chunks(file)
  temp <- tempfile(fileext=".R")

  chunks <- purrr::map(chunk_labels %>% set_names(), ~stringr::str_subset(text, glue::glue("```\\{{r {.x}")))

  if (length(setdiff(names(chunks), chunk_labels)) > 0) warning("Not all chunks found")

  chunks %>% map_chr(~str_remove_all(.x, "(^|\n)```.*")) %>%
    readr::write_file(temp)

  if(skip_plots) {
    old_dev <- getOption('device')
    options(device = function(...) NULL)
  }

  source(temp)

  if(skip_plots) {
    options(device = old_dev)
  }

  if(output_temp) return(temp)
}
