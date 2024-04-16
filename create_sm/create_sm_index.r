# Nested list of reports
report_files <- list(
  "SM1: Search & Screening" = list(
    "A: Details of Search Strategy" = c("SM1 - Search and Screening/SM1a - Details on search strategy.pdf"), 
    "E: PRISMA Flow Diagram numbers" = c("SM2 - Analysis/1a_prisma_flowchart_data.html"),
    "Code Repository" = "https://github.com/LukasWallrich/diversity_meta/tree/main/SM1%20-%20Search%20and%20Screening"),
  "SM2: Analysis" = list(
    "A: Dataset description" = c("SM2 - Analysis/1_prep_and_describe_data.html"),
    "B: Meta-Analysis Results" = c("SM2 - Analysis/2_main_meta.html"),
    "C: Moderator tests" = c("http://NA"), # Not ready yet
    "D: Publication Bias Tests" = c("SM2 - Analysis/4_publication-bias.html"),
    "E: Exploratory Analyses" = c("SM2 - Analysis/5_exploration.html"),
    "F: Robustness checks" = c("SM2 - Analysis/6_robustness.html"),
    
    "Code Repository" = "https://github.com/LukasWallrich/diversity_meta/tree/main/SM2%20-%20Analysis"
  )
)

# Function to recursively copy files and generate HTML for reports
generate_html <- function(reports, base_path = "docs/") {
  html_content <- ""
  for (name in names(reports)) {
    report <- reports[[name]]
    # Sanitize ID to avoid special characters in HTML IDs
    safe_id <- gsub("[^A-Za-z0-9]", "", name)
    if (is.list(report)) {
      # Recurse into subreports
      sub_reports_html <- ""
      for (sub_name in names(report)) {
        sub_report <- report[[sub_name]]
        for (file_path in sub_report) {
          if (grepl("^http[s]?://", file_path, ignore.case = TRUE)) {
            # External URL
            sub_reports_html <- paste(sub_reports_html, sprintf('<a class="dropdown-item" href="%s" target="_blank">%s</a>', file_path, sub_name), sep="\n")
          } else {
            # Local file
            target_path <- file.path(base_path, basename(file_path))
            if (!file.exists(target_path)) {
              file.copy(file_path, target_path, overwrite = TRUE)
            }
            sub_reports_html <- paste(sub_reports_html, sprintf('<a class="dropdown-item" href="%s" target="reportFrame">%s</a>', basename(file_path), sub_name), sep="\n")
          }
        }
      }
      html_content <- paste(html_content, sprintf('<li class="nav-item dropdown">
        <a class="nav-link dropdown-toggle" href="#" id="%s" role="button" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">%s</a>
        <div class="dropdown-menu" aria-labelledby="%s">%s</div>
      </li>', safe_id, name, safe_id, sub_reports_html), sep="\n")
    } else {
      # Single files or URLs
      for (file_path in report) {
        if (grepl("^http[s]?://", file_path, ignore.case = TRUE)) {
          html_content <- paste(html_content, sprintf('<li class="nav-item"><a class="nav-link" href="%s" target="_blank">%s</a></li>', file_path, name), sep="\n")
        } else {
          target_path <- file.path(base_path, basename(file_path))
          if (!file.exists(target_path)) {
            file.copy(file_path, target_path, overwrite = TRUE)
          }
          html_content <- paste(html_content, sprintf('<li class="nav-item"><a class="nav-link" href="%s" target="reportFrame">%s</a></li>', basename(file_path), name), sep="\n")
        }
      }
    }
  }
  return(html_content)
}
# Ensure the "docs/" directory exists
if (!dir.exists("docs")) {
  dir.create("docs", recursive = TRUE)
}

# Generate the full HTML content for the <ul> element
html_content <- generate_html(report_files)

# Read and modify the template
template <- readLines("create_sm/template.html", warn = FALSE)
output <- gsub("\\{\\{content\\}\\}", html_content, template)
writeLines(output, "docs/index.html")
