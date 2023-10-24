### script to download and store data

## params
is_internal <- FALSE
# if publishing something RETROSPECTIVELY, change this
as_of_date <- Sys.Date()

## packages etc

if (!require(here)) install.packages("here")
source(here::here("R/funcs/helpers.R"))

### TO INSTALL INTERNAL PACKAGES USE THIS SCRIPT
### will not run if already installed - go into script to update packages!
source(here::here("R/scripts/install_internal_pkgs.R"))

## install and load BIOCONDUCTOR PKGS
load_pkg(tidytree, phytools, ggtree, bioc = T)

## LOAD required packages (install if not already)
## THIS HAS TO BE COMMENTED OUT SINCE rsconnect does not recognise these as deps
load_pkg(tidyverse, here, janitor, lubridate, phifunc, magrittr, gt, rmarkdown, knitr, whomapper, glue, shiny,
         shinyjs, shinydashboard, shinydashboardPlus, rsconnect, htmltools, downloadthis, gtsummary, gsheet)



# set repos options
library(BiocManager)
options(repos = BiocManager::repositories())

# LOAD FUNCTIONS FROM REPO
walk(list.files(here("R/funcs"), full.names = TRUE, pattern = ".R$"), source)


### data downloading 

all_data <- download_all_data(as_of = as_of_date, internal = is_internal, format = "json")

phylo <- preprocess_nextstrain()

## save data locally

write_path <- here::here("data", ifelse(is_internal, "internal", "external"), "report_data.rds")

# check dir exists
if (!dir.exists(dirname(write_path))) {
  dir.create(dirname(write_path), recursive = TRUE)
}

# write data
write_rds(
  all_data,
  here::here("data", ifelse(is_internal, "internal", "external"), "report_data.rds")
)

write_rds(
  phylo,
  here::here("data/phylo/phylo_proc.rds")
)

## run reports and publish
if (is_internal) {
  rmarkdown::render(here::here("mpx_global_internal.Rmd"), params = list("download_new_data" = FALSE))
} else {
  # clear the prerendered cache
  rmarkdown::shiny_prerendered_clean(here::here("mpx_global.Rmd"))
  # re-render the report
  # rmarkdown::render(here::here("mpx_global.Rmd"), params = list("download_new_data" = FALSE))
  
  # # define files and directories not to publish
  # files_to_ignore <- c("ui.R", "server.R", "global.R", 
  #                      "mpx_global_internal.Rmd", "mpx_global_internal.html",
  #                      "outputs", "figures")
  
  # if not set up, connect to who server
  rsconnect::setAccountInfo(name='worldhealthorg',
                            token='F3A11100434CB7F12DDC21D7D82CF409',
                            secret='V23SBGkwMg+I0u9fB3qx1Vo9xsqn4zAUb9bbPKNw')
  
  