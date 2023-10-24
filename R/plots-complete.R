

is_reported <- function(x) {
  
  out <- x %>% 
    str_to_upper() %>% 
    str_replace("UNK|UNKNOWN", NA_character_)
  
  sum(!is.na(out), na.rm = TRUE)
}

all_data <- download_all_data()


to_plot <- all_data$ll %>% 
  mutate(who_region = long_whoreg(who_region)) %>% 
  group_by(who_region) %>% 
  select(-symp_other, -symp_asy) %>% 
  mutate(across(contains("symp"), ~ifelse(iso3 == "USA", NA_character_, .))) %>% 
  # rowwise() %>% 
  mutate(has_symps = coalesce(symp_chills, symp_conj, symp_cough, symp_fatigue, symp_fever,
                              symp_genital, symp_headache, symp_locallymph, symp_lymph, symp_musc, symp_oral, 
                              symp_rash, symp_throat, symp_vomit, symp_diarr, symp_genitedem, symp_lymphlocunk, 
                              symp_rashlocunk, symp_proct),
         has_vax = coalesce(smallpox_vaccine, monkeypox_vaccine_1, monkeypox_vaccine),
         across(c(has_symps, has_vax), ~ifelse(. < 1, NA_character_, "YES"))) %>%
  # ungroup() %>% 
  summarise(across(
    c(gender, age_years, hospitalised, icu, hiv_status, has_symps, has_vax, travel_history, date_onset,
      sexual_orientation, health_worker,
      outcome, transmission, exposuresetting),
    # ~sum(!is.na(.), na.rm = TRUE)
    is_reported
  ), total = n()) %>% 
  pivot_longer(cols = c(-who_region, -total), names_to = "var", values_to = "reported") %>% 
  mutate(pct = reported / total,
         lab = case_when(
           var == "gender" ~ "Sex",
           var == "age_years"  ~"Age",
           var == "hospitalised" ~ "Hospitalisation",
           var == "icu" ~ "ICU",
           var == "hiv_status" ~ "HIV status",
           var == "has_symps" ~ "Symptoms",
           var == "has_vax" ~ "Vaccination",
           var == "travel_history" ~ "Travel history",
           var == "date_onset" ~ "Onset date",
           var == "outcome" ~ "Outcome",
           var == "transmission" ~ "Transmission",
           var == "exposuresetting" ~ "Exposure setting",
           var == "sexual_orientation" ~ "Sexual orientation",
           var == "health_worker" ~ "Health worker",
           TRUE ~ var
         ) %>% 
           factor(levels = c("Age", "Sex", "Onset date", "Hospitalisation", "Outcome", "ICU",
                             "Transmission", "Exposure setting", "Sexual orientation",
                             "HIV status", "Health worker", "Travel history",
                             "Symptoms", "Vaccination")))



p1 <- ggplot(to_plot, aes(y = fct_rev(lab), x = reported, fill = pct)) + 
  geom_col(col = "black") + 
  geom_vline(aes(xintercept = total), col = "darkblue", size = 1, linetype = "solid") +
  facet_wrap(~str_wrap(who_region, 20), scales = "free_x", nrow = 3) +
  # scale_x_continuous(labels = scales::percent) +
  theme_mpx() + 
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(), legend.title = element_text(face = "bold")) +
  labs(x = "Number of complete records", y = "Variable") +
  scale_fill_gradient(high = "#1a9641", low = "#d7191c", name = "Percent of records complete",
                      labels = scales::percent) +
  guides(fill = guide_legend(title.position = "top", keywidth = "3"))

save_plot(p1, "figs2.eps", width = 8, height = 6)
save_plot(p1, "figs2.pdf", width = 8, height = 6)
save_plot(p1, "figs2.png", width = 8, height = 6)

save_plot(p1, "oxf4.png", width = 7, height = 8)