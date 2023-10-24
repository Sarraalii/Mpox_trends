## packages etc
source(here::here("R/funcs/helpers.R"))

## LOAD required packages (install if not already)
load_pkg(c(
  "tidyverse", "here", "janitor", "lubridate", "phifunc", "glue", "magrittr"
))

# LOAD FUNCTIONS FROM REPO
walk(
  list.files(here::here("R/funcs"), pattern = ".R$"),
  ~ source(here::here("R/funcs", .x))
)

ll <- pull_xmart("LINELIST_CORE", mart = "MPX", auth = TRUE, format = "csv")

vis_agesex(ll) %>% save_plot("agesex.png")

vis_symptoms(ll) %>% save_plot("symptoms.png", height = 5.2)