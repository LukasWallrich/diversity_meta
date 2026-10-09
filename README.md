# Supplementary Materials for diversity & team-performance meta-analysis

This meta-analysis was conducted as a registered report; see the [Stage 1 registration](https://osf.io/f5qdn/).

Wallrich, L., Opara, V., Wesołowska, M., Barnoth, D., & Yousefi, S. (2024). The relationship between team diversity and team performance: Reconciling promise and reality through a comprehensive meta-analysis registered report. *Journal of Business and Psychology, 39*, 1303–1354. https://doi.org/10.1007/s10869-024-09977-0

[Published article](https://doi.org/10.1007/s10869-024-09977-0) · [Open manuscript](https://osf.io/nscd4/)

# Overview

The supplementary materials are divided into two sections:

- `SM1`: Contains code and details on the search and screening process (not directly reproducible due to intervening manual steps)
- `SM2`: Contains code and results of the analysis (fully reproducible)

The materials are best accessible through the [webpage](https://lukaswallrich.github.io/diversity_meta/) that is build using the code in `create_sm` and saved in `docs`. The data can also be explored through an [interactive web application](https://2ly.link/1xekY).

## Shiny app deployment

The [live app](https://lukaswallrich.shinyapps.io/diversity_meta/) is deployed from `web_app/shiny_code` to the `lukaswallrich` account on shinyapps.io (app ID `11790098`). The deployment record is in `web_app/shiny_code/rsconnect/shinyapps.io/lukaswallrich/diversity_meta.dcf`. GitHub pushes do not automatically redeploy the app.

`web_app/generate_app.Rmd` generates the app. Keep its citation, contact, and data links in step with the About section in `web_app/shiny_code/ui.R` so regeneration preserves these details.

After configuring the shinyapps.io account locally, deploy from the repository root:

```r
rsconnect::deployApp(
  appDir = "web_app/shiny_code",
  appName = "diversity_meta",
  appId = "11790098",
  account = "lukaswallrich",
  server = "shinyapps.io"
)
```

Deployment credentials belong in private local configuration, outside the repository.
