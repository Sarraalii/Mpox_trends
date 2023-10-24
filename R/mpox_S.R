## packages etc
setwd("../..")
source("R/funcs/helpers.R")
devtools::load_all("~/Dropbox/who/covid/soc_test")

## ## ## update SOC2 package
## Sys.setenv(GITHUB_PAT = "d9b68eb06316f7e322c660103b187cbbcf024f72")
## devtools::install("~/Dropbox/who/covid/soc_test", dependencies = FALSE)

## LOAD required packages (install if not already)
load_pkg(c(
  "tidyverse",
  "here",
  "janitor",
  "lubridate",
  "magrittr",
  "rio"
))

# LOAD FUNCTIONS FROM REPO
walk(list.files(here::here("R/funcs"), pattern = ".R$"), ~source(here::here("R/funcs", .x)))

## get outputs
outputs <- run_soc_pipeline(
  data_folder = here("data/monkeysox"),
  outputs_folder = here("data/monkeysox"),
  include_gisaid = FALSE,
  update_gisaid = FALSE,
  update_timeseries = TRUE,
  update_results = TRUE,
  include_hospitalisations = FALSE,
  pathogen = "monkeypox",
  countries = c("GBR", "COG")
)

results <- outputs$results

results %>%
  filter(!map_lgl(stan_fit, is.null)) %>%
  transmute(
    Country = country,
    `WHO Region` = who_region,
    vax = map2(
      stan_fit, stan_data,
      ~ get_stan_distribution("vaccines", .x, .y) %>%
        apply(1, cumsum) %>%
        apply(1, quantile, seq(0.1, 0.9, 0.1)) %>%
        round() %>% t() %>% as_tibble() %>% slice_tail(n = 1)
    )
  ) %>%
  unnest(vax) %>%
  ## filter(`90%` > 0) %>%
  arrange(`WHO Region`, desc(`50%`)) %>%
  export(here("data/monkeysox/vaccine_projection.xlsx"), overwrite = TRUE)