### death figure


# if (FALSE) {
require(tidyverse)
require(here)
require(janitor)
require(lubridate)
require(phifunc)
require(glue)
require(magrittr)
require(gt)
require(rmarkdown)
require(knitr)
require(whomapper)
require(shiny)
require(shinyjs)
require(shinydashboard)
require(shinydashboardPlus)
require(downloadthis)
require(gsheet) 

# }
source(here::here("R/funcs/helpers.R"))

# LOAD FUNCTIONS FROM REPO
walk(list.files(here("R/funcs"), full.names = TRUE, pattern = ".R$"), source)

if (FALSE) {
  
  dat <- download_all_data(format = "json")
  write_rds(dat, paste0("data/deaths_manuscript/snapshot_data_", Sys.Date(), ".rds"))
  
} else {
  dat <- read_rds(here::here("data/deaths_manuscript/snapshot_data_2023-09-27.rds"))
}

retroadjust_deaths <- function(x) {
  
  x <- x %>% 
    arrange(date) %>% 
    mutate(lead_new = lead(new_death),
           new_total_death = ifelse(lead_new < 0, total_death + lead_new, total_death),
           total_death = coalesce(new_total_death, total_death)) %>% 
    filter(new_death > 0) %>% 
    mutate(new_death = total_death - lag(total_death, default = 0))
  
  if (min(x$new_death, na.rm = TRUE) < 0) {
    return(retroadjust_deaths(x))
  } else {
    return(x)
  }
  
}

fix_date <- function(date) {
  
  
  if (!year(date) %in% c(2022, 2023)) {
    if (grepl("3$", as.character(year(date)))) {
      year(date) <- 2023
    } else {
      year(date) <- 2022
    }
  } 
  
  return(date)
  
}


death_df_temp <- dat$agg %>% 
  group_by(country) %>% 
  arrange(date) %>% 
  filter(new_death != 0)

death_negs <- death_df_temp %>% 
  mutate(has_retro = ifelse(min(new_death) < 0, TRUE, FALSE)) %>% 
  filter(has_retro) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, retroadjust_deaths)) %>% 
  unnest(cols = data) %>% 
  select(-has_retro, -lead_new, -new_total_death)

death_agg <- death_df_temp %>% 
  filter(!iso3 %in% death_negs$iso3) %>% 
  bind_rows(death_negs) %>% 
  filter(new_death != 0)


deaths_ll <- dat$ll %>% 
  filter(case_class %in% c(NA, "CONFIRMED")) %>% 
  mutate(outcome = ifelse(!is.na(date_death) & country == "Peru", "D", outcome),
         across(c(date_onset, date_death, date_diagnosis), as_date),
         outcome_delay = case_when(outcome == "D" ~ "Death", TRUE ~ "Alive")) %>% 
  mutate(date_diagnosis = ifelse(date_diagnosis - date_onset > 365, date_diagnosis - 365, date_diagnosis),
         date_diagnosis = as_date(date_diagnosis__)) %>% 
  rowwise() %>% 
  mutate(across(c(date_diagnosis, date_onset), fix_date)) %>% 
  ungroup()

## check deaths align

deaths_ll %>% 
  filter(outcome == "D", case_class %in% c("CONFIRMED", NA)) %>%
  group_by(country) %>% 
  summarise(num_ll = n()) %>% 
  full_join(
    dat$agg %>% 
      group_by(country) %>% 
      summarise(n_total = sum(new_death))
  ) %>% 
  mutate(pct_total = num_ll / n_total) %>% 
  filter(num_ll !=0 & n_total != 0)



deaths_ll %>% 
  filter(outcome == "D", case_class %in% c("CONFIRMED", NA)) %>%
  group_by(who_region) %>% 
  summarise(num_ll = n())


iqr = function(z, lower = 0.25, upper = 0.75) {
  data.frame(
    y = median(z, na.rm = TRUE),
    ymin = quantile(z, lower, na.rm = TRUE),
    ymax = quantile(z, upper, na.rm = TRUE)
  )
}

pv_stars <- function(pv) {
  case_when(pv < 0.001 ~ "***",
            pv < 0.01 ~ "**",
            pv < 0.05 ~ "*",
            TRUE ~ NA_character_)
}


plot_df <- deaths_ll %>% 
  mutate(delay_death = date_death - date_onset,
         delay_diag = date_diagnosis - date_onset) %>% 
  select(outcome_delay, delay_death, delay_diag) %>% 
  pivot_longer(cols = c(delay_death, delay_diag),
               names_to = "type", values_to = "delay") %>% 
  mutate(type_lab = ifelse(type == "delay_death", "Onset to death", "Onset to diagnosis")) %>% 
  filter(!(outcome_delay == "Alive" & type == "delay_death"),
         type == "delay_diag" & between(as.numeric(delay), 0, 100) | 
           type == "delay_death" & delay > 0,
  ) %>% 
  mutate(od2 = case_when(
    outcome_delay == "Death" & type == "delay_diag" ~ "Death",
    outcome_delay == "Death" ~ " Death",
    TRUE ~ "Alive"
  )) 



days_alive <- plot_df %>% 
  filter(type == "delay_diag" & od2 == "Alive") %>% 
  pull(delay) %>% 
  as.double()

days_death <- plot_df %>% 
  filter(type == "delay_diag" & od2 == "Death") %>% 
  pull(delay) %>% 
  as.double()

test_delay <- wilcox.test(days_alive, days_death)


p1 <- plot_df %>% 
  ggplot(aes(x = delay, y  = outcome_delay)) +
  # geom_density(aes(fill = outcome_delay)) +
  # ggbeeswarm::geom_beeswarm(position = position_dodge(), aes(fill = outcome_delay)) +
  # geom_boxplot(position = position_dodge(), aes(fill = outcome_delay)) +
  geom_violin(position = position_dodge(), aes(fill = outcome_delay)) +
  # geom_text(
  #   data = tibble(
  #     delay = max(days_death) * 1.1, y = "Death", outcome_delay = "Death",
  #     type_lab = "Onset to diagnosis",
  #     lab = pv_stars(test_delay$p.value)
  #   ), 
  #   aes(label = lab), fill = NA, size = 7) +
  theme_mpx() +
  scale_fill_manual(values = c("#66c2a5", "#f46d43"), name = "Delay Between:") +
  # facet_grid(outcome_delay ~ ., space = "free", shrink = TRUE, scales = "free") +
  theme(strip.text = element_blank(), strip.text.y = element_blank(),
        legend.position = "none") +
  labs(y = NULL, x = "Delay (days)") +
  scale_x_continuous(breaks = seq(0, 200, by = 25)) +
  stat_summary(fun.data = iqr) +
  ggforce::facet_col(~type_lab, scales = "free", shrink = TRUE, space = "free") +
  # facet_grid(rows = vars(var_label), scales = "free_y", shrink = TRUE, space = "free_y", strip.position = "top") +
  theme(strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(colour = "black"),
        axis.text = element_text(colour = "black"),
        panel.border = element_rect(fill = NA)) +
  theme(panel.grid.major.y = element_blank())

p1


deaths_ll %>% 
  filter(outcome == "D") %>% 
  mutate(delay_death = date_death - date_onset,
         delay_diag = date_diagnosis - date_onset) %>% 
  select(delay_death, hiv_status) %>% 
  filter(!is.na(delay_death)) %>% 
  group_by(hiv_status) %>% 
  summarise(n = n(), delay = iqr(delay_death))


p2 <- deaths_ll %>% 
  filter(outcome == "D") %>% 
  select(who_region, date_onset, date_death) %>% 
  pivot_longer(cols = c(date_onset, date_death),
               names_to = "date_type", values_to = "date") %>% 
  filter(!is.na(date)) %>% 
  group_by(who_region, date_type, date) %>% 
  summarise(deaths = n()) %>% 
  bind_rows(
    death_agg %>% 
      ungroup() %>% 
      select(who_region, date, deaths = new_death) %>% 
      mutate(date_type = "date_report")
  ) %>% 
  group_by(date_type) %>% 
  mutate(who_reg_long = long_whoreg(who_region),
         date_lab = case_when(
           date_type == "date_death" ~ paste0("Date of death (n = ", sum(deaths), ")"),
           date_type == "date_onset" ~ paste0("Date of onset (n = ", sum(deaths), ")"),
           date_type == "date_report" ~ paste0("Date of report (n = ", sum(deaths), ")"),
         ),
         date_week = floor_date(date, "week", 1)) %>% 
  group_by(date_week, date_lab, who_reg_long) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>% 
  ggplot(aes(x = date_week + days(3), y = deaths, fill = who_reg_long)) +
  geom_col(width = 7, col = "black") + 
  theme_mpx() + 
  expand_limits(y = 5) + 
  scale_y_continuous(breaks = scales::pretty_breaks()) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%d %b\n%Y") +
  facet_wrap(~fct_relevel(date_lab,  ~grep("onset", .x, value = TRUE), after = 0), ncol = 1) +
  scale_fill_manual(values = mpx_pal("qual1"), name = NULL) +
  labs(x = NULL, y = "Number of deaths") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(plot.margin = unit(c(0.2, 1, 0.2, 0.2), units = "cm")) + 
  theme(legend.position = "bottom")



labels <- c(
  asy = "Asymptomatic",
  conj = "Conjunctivitis",
  cough = "Cough",
  oral = "Oral rash",
  other = "Other",
  throat = "Sore throat",
  vomit = "Vomiting",
  chills = "Chills",
  musc = "Muscle ache",
  fatigue = "Fatigue",
  headache = "Headache",
  locallymph = "Local lymphadenopathy",
  lymph = "General lymphadenopathy",
  rash = "Systemic rash",
  fever = "Fever",
  genital = "Genital rash",
  rash = "Systemic rash",
  rash_all = "Any rash",
  lymph_all = "Any lymphadenopathy",
  rashlocunk = "Rash, unknown location",
  lymphlocunk = "Lymphadenopathy, location unknown",
  diarr = "Diarrhea",
  genitedem = "Genital oedema",
  proct = "Anogenital pain and/or bleeding"
)

chi_test <- deaths_ll %>% 
  filter(iso3 != "USA") %>% 
  mutate(
    outcome2 = ifelse(outcome == "D", "Death", "Alive"),
    outcome2 = replace_na(outcome2, "Alive"),
    symp_rash_all = coalesce(symp_genital, symp_oral, symp_rashlocunk, symp_rash),
    symp_lymph_all = coalesce(symp_lymph, symp_locallymph, symp_lymphlocunk)) %>% 
  filter(if_any(contains("symp"), ~ .x == "YES")) %>% 
  #rowwise() %>% 
  mutate(ns = sum(!is.na(c_across(contains("symp"))))) %>% 
  group_by(country) %>% 
  filter(max(ns, na.rm = TRUE) > 1) %>% 
  ungroup() %>% 
  select(starts_with("symp"), outcome = outcome2) %>% 
  mutate(across(starts_with("symp"), ~replace_na(., "NO")))

do_chisq <- function(dat, dep_var) {
  broom::tidy(chisq.test(dat$outcome, dat[[dep_var]])) %>% 
    mutate(symp_raw = dep_var)
}

symps <- c("symp_rash", "symp_fever", "symp_genital", #"symp_lymph_all",
           "symp_headache", "symp_musc", "symp_oral", "symp_locallymph")

results_p3 <- map_df(symps, ~do_chisq(chi_test, .x)) %>% 
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>% 
  mutate(symp_raw = str_remove(symp_raw, "symp_"),
         symp = labels[symp_raw]) %>% 
  select(-method)


sig_results <- results_p3 %>% 
  filter(p_fdr < 0.05) %>% 
  mutate(p_lab = pv_stars(p_fdr))

all_symps <- vis_symptoms(deaths_ll %>% filter(outcome != "D", iso3 != "USA"), return_data = TRUE) %>% 
  mutate(outcome = "Alive") %>% 
  bind_rows(
    vis_symptoms(deaths_ll %>% filter(outcome == "D", iso3 != "USA"), return_data = TRUE) %>% 
      mutate(outcome = "Death")
  ) %>% 
  mutate(total = n / value)


symps_out <- all_symps %>% 
  rename(pct = value, symp = name) %>% 
  mutate(pct = scales::percent(pct, accuracy = 0.1)) %>% 
  pivot_wider(names_from = outcome, values_from = c(n, pct, total)) %>% 
  arrange(desc(n_Death)) %>% 
  left_join(results_p3 %>% select(symp, p_value = p.value, p_fdr)) %>% 
  mutate(across(c(p_value, p_fdr), ~case_when(.x < 0.001 ~ "<0.001", TRUE ~ as.character(signif(.x, 2))))) %>% 
  group_by(used_stas = is.na(p_value)) %>% 
  group_split()

s1 <- symps_out[[1]] %>%
  rowwise() %>% 
  mutate(ors = pmap(list(n_Alive, n_Death, total_Alive, total_Death),
                    ~calc_ors(..1, ..3 - ..1, ..2, ..4 - ..2))) %>% 
  unnest_wider(ors) %>% 
  mutate(ors_lab = str_glue("{round(as.numeric(or), 2)} ({round(lower, 2)} - {round(upper, 2)})"))

reduce(symps_out, bind_rows) %>% 
  mutate(ors = )
write_csv("symps_deaths.csv", na = "-")

p3 <- all_symps %>% 
  group_by(name) %>% 
  filter(min(n) > 4) %>% 
  # filter(sum(value) > 0.4) %>% 
  ggplot(aes(name, value, fill = outcome, group = outcome)) +
  geom_col(col = 'black', position = position_dodge())+
  scale_x_discrete() +
  coord_flip() +
  theme_mpx() +
  theme(panel.grid.major.y = element_blank()) +
  scale_fill_manual(values = c("#fb8072", "#bebada"), name = NULL) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = scales::percent
  ) +
  labs(
    x = NULL,
    y = "Proportion of cases with reported symptom"
  ) +
  theme(legend.position = "bottom")

p3

layout = 
  "AAABB
   AAACC
   AAACC"


# layout <- c(
#   area(t = 0, r = 300, b = 30, l = 0),
#   area(t = 0, l = 32, b = 10, r = 50),
#   area(t = 11, l = 30, r = 31, b = 30),
#   area(t = 11, l = 31, b = 30, r = 50)
# )


require(patchwork)

# p13 <- plot_grid(p1, p3, ncol = 1, rel_heights = c(1, 2))
# 
# p2 + p13 + plot_layout(nrow = 1, widths = c(2, 3)) + plot_annotation(tag_levels = 'A')
#p3 

fig1 <- p2 + p1 + p3 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A') 

ggsave(here::here("figures/deathf1.pdf"), fig1, width = 14, height = 8)



# paper numbers ------------------------------------------------------------

cat_props <- function(x, var) {
  x %>% 
    rename(voi = any_of(var)) %>% 
    filter(voi != "UNK", !is.na(var)) %>% 
    group_by(voi) %>% 
    summarise(num = n()) %>% 
    ungroup() %>% 
    mutate(pct = num / sum(num) * 100,
           total = sum(num))
}

# max date
dat$agg %>% 
  pull(date) %>% max()

sum(dat$agg$new_conf)

sum(dat$agg$new_death)

length(unique(dat$agg$country))

cat_props(deaths_ll, "gender")

cat_props(deaths_ll %>% mutate(age_group = bin_numbers(age_years, breaks = c(0, 18))),
          "age_group")




cat_props(deaths_ll,
          "sexual_orientation_2")

deaths_ll %>% 
  mutate(has_hiv = case_when(
    hiv_status == "UNK" ~ "NO", 
    is.na(hiv_status) ~ "NO",
    TRUE ~ "YES"
  )) %>% 
  cat_props("has_hiv")

cat_props(deaths_ll,
          "hiv_status")


dat$agg %>% 
  group_by(who_region) %>% 
  summarise(deaths = sum(new_death)) %>% 
  ungroup() %>% 
  mutate(pct = deaths / sum(deaths))


cat_props(deaths_ll)

nrow(deaths_ll %>% filter(outcome == "D"))

deaths_ll %>% 
  mutate(delay_death = date_death - date_onset,
         delay_diag = date_diagnosis - date_onset) %>% 
  select(outcome_delay, delay_death, delay_diag) %>% 
  pivot_longer(cols = c(delay_death, delay_diag),
               names_to = "type", values_to = "delay") %>% 
  filter(!is.na(delay)) %>% 
  group_by(outcome_delay, type) %>% 
  reframe(
    iqr = list(quantile(delay, probs = c(0.25, 0.5, 0.75)))
  ) %>% 
  unnest_wider(iqr)


cat_props(deaths_ll %>% filter(outcome == "D"), "gender")



cat_props(deaths_ll %>% 
            filter(outcome == "D") %>% 
            mutate(age_group = bin_numbers(age_years, breaks = c(0, 15, 45))),
          "age_group")

cat_props(deaths_ll %>% 
            filter(outcome == "D"),
          "sexual_orientation_2")


vis_categoricals(deaths_ll %>% 
                   filter(outcome == "D"), var = "transmission", return_data = TRUE) %>% 
  mutate(total = sum(n), pct = n / total * 100)


vis_categoricals(deaths_ll %>% 
                   filter(outcome == "D"), var = "exposuresetting", return_data = TRUE)

deaths_ll %>% 
  filter(!is.na(concurrent_sti), outcome == "D", concurrent_sti != "NO", concurrent_sti != "UNK") %>%
  pull(concurrent_sti)


cat_props(deaths_ll %>% 
            filter(outcome == "D"),
          "icu")


cat_props(deaths_ll %>% 
            filter(outcome == "D"),
          "hospitalised")

vis_symptoms(deaths_ll %>% 
               filter(outcome == "D", iso3 != "USA"), return_data = TRUE) %>% 
  arrange(desc(n))



vis_symptoms(deaths_ll %>% 
               filter(outcome_delay == "Alive", iso3 != "USA"), return_data = TRUE) %>% 
  arrange(desc(n))

# model section -----------------------------------------------------------






ll_death <- ll_reg %>% 
  filter(!is.na(death_out)) %>% 
  group_by(country) %>%
  filter(sum(grepl("^Y", death_out)) != 0,
         sum(hiv_immuno != "Unknown") != 0) %>% 
  ungroup()


m0_glm <- glm(death_out ~ 1, family = binomial, data = ll_death)

m0_glmr <- lme4::glmer(death_out ~ 1 + (1 | country), family = "binomial", data = ll_death)

AIC(m0_glm) ; AIC(m0_glmr) ; AIC(m0_glmr) < AIC(m0_glm)
null.id = -2 * logLik(m0_glm) + 2 * logLik(m0_glmr)
pchisq(as.numeric(null.id), df=1, lower.tail=F)



m1_glmr <- update(m0_glm, .~.+ age_group)
anova(m1_glmr, m0_glmr, test = "Chi") 
overdisp_fun(m1_glmr)

m2_glmr <- update(m0_glm, .~. + hiv_immuno)
anova(m2_glmr, m0_glmr, test = "Chi") 
overdisp_fun(m2_glmr)

m3_glmr <- update(m2_glmr, .~. + sex)
anova(m3_glmr, m2_glmr, test = "Chi") 

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


death_tab_glmr <- tbl_regression(m2_glmr, 
                                 exponentiate = TRUE, 
                                 include = c(hiv_immuno),
                                 label = model_labs)

death_tab_glmr



model_labs <- list(age_group = "Age group", sex = "Sex",
                   hiv_immuno = "HIV/immunocompromised status")




dat$agg %>% 
  group_by(who_region = long_whoreg(who_region)) %>% 
  summarise(cases = sum(new_conf), deaths = sum(new_death)) %>% 
  mutate(cfr = deaths / cases * 100 %>% round(2)) %>% 
  arrange(desc(cases)) %>% clipr::write_clip()
