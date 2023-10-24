# Setup ------------------------------------------------------------------------
# load packages 
require(tidyverse)
require(lubridate)
require(lme4)
require(gt)
require(gtsummary)
require(here)
require(janitor)
require(phifunc)



# load functions
walk(list.files(here("R/funcs"), full.names = TRUE, pattern = ".R$"), source)


# load data 
llp <- read_csv("C:\\Users\\mpoxdata\\Downloads\\2023-01-31-snapshot-V_MPX_LINELIST.csv") %>% 
  rename_all(., str_to_lower) %>% 
  filter(case_class %in% c("CONFIRMED", NA)) 

all_data <- download_all_data(internal = TRUE)

llpf <- bind_rows(
  llp %>% 
    filter(who_region != "AFRO"),
  all_data$ll %>% 
    filter(who_region == "AFRO", report_date <= ymd("2023-01-29") | is.na(report_date))
)
ll <- llpf %>% 
  mutate(gender = ifelse(gender == "UNK", NA_character_, gender))


# write functions
surv_parse <- function(x, var = "hosp_out") {
  
  if (sum(grepl("NO", x[[var]])) == 0) {
    x %>% 
      mutate(across(all_of(var), ~replace_na(., "NO")))
  }
  x
}

rf_mod <- function(x, rv, outvar = "hosp_out") {
  x %>% 
    select(resp = one_of(outvar), country, rf = one_of(rv)) %>% 
    mutate(rv_lab = rv)
}

# Data Preparation -------------------------------------------------------------
ll_reg <- ll %>% 
  mutate(
    hosp_out = case_when(
      hospitalised == "NO" ~ "NO",
      hospitalised == "ND" ~ "NO",
      hospitalised == "YISOL" ~ "NO",
      grepl("^Y", hospitalised) ~ "YES",
      hospitalised == "UNK" ~ NA_character_,
      TRUE ~ hospitalised
    ),
    hosp_out2 = case_when(
      hospitalised == "NO" ~ "NO",
      hospitalised == "ND" ~ "NO",
      hospitalised == "YISOL" ~ "NO",
      hospitalised == "YES" ~ "YTREAT",
      hospitalised == "YTREAT" ~ "YTREAT",
      hospitalised == "YUNK" ~ "YUNK",
      hospitalised == "UNK" ~ NA_character_,
      TRUE ~ hospitalised
    ),
    icu_out = case_when(
      icu == "YES" ~ "YES",
      icu == "UNK" ~ NA_character_,
      TRUE ~ icu
    ),
    age_years,
    age_group = bin_numbers(age_years, c(0, 5, 15, 45, 65)) %>% fct_relevel("15-44") %>% fct_explicit_na("Unknown"),
    sex = case_when(
      gender == "MALE" ~ "Male",
      gender == "FEMALE" ~ "Female",
      TRUE ~ "Unknown",
      TRUE ~ NA_character_
    ),
    sexor = case_when(
      sexual_orientation == "MSM" ~ "MSM",
      sexual_orientation == "HETERO" ~ "Heterosexual",
      sexual_orientation %in% c("O", "BISEXUAL", "LESBIAN") ~ "Other",
      sexual_orientation == "UNK" ~ "Unknown",
      TRUE ~ "Unknown"
    ),
    hiv_status = case_when(
      hiv_status == "POS" ~ "HIV+",
      hiv_status == "NEG" ~ "HIV-",      
      hiv_status == "UNK" ~ NA_character_,
      TRUE ~ hiv_status
    ),
    immunosuppression = case_when(
      grepl("^Y|^S", immunosuppression) ~ "YES",
      immunosuppression == "NO" ~ "NO",
      TRUE ~ NA_character_
    ),
    hiv_immuno = case_when(
      hiv_status == "HIV+" & immunosuppression == "YES" ~ "HIV+/Immunosuppressed",
      hiv_status == "HIV+" & immunosuppression == "NO" ~ "HIV+/Not Immunosuppressed",
      #hiv_status == "HIV+" & is.na(immunosuppression) ~ "HIV+/Unknown",
      hiv_status == "HIV-" & immunosuppression == "YES" ~ "HIV-/Immunosuppressed",
      hiv_status == "HIV-" & immunosuppression == "NO" ~ "HIV-/Not Immunosuppressed",
      # hiv_status == "HIV-" & is.na(immunosuppression) ~ "HIV-/Unknown",
      # is.na(hiv_status) & immunosuppression == "YES" ~ "Unknown/Immunosuppressed",
      # is.na(hiv_status) & immunosuppression == "NO" ~ "Unknown/Not Immunosuppressed",
      # is.na(hiv_status) & is.na(immunosuppression) ~ "Unknown/Unknown",
      TRUE ~ "Unknown"
    )
  ) %>% 
  group_by(country) %>% 
  filter(sum(grepl("^Y", hosp_out)) != 0) %>% 
  nest() %>% 
  mutate(
    data = map(data, ~surv_parse(x = .x, var = "hosp_out")),
    data = map(data, surv_parse, "icu_out")) %>%
  unnest(cols = c(data)) %>% 
  filter(!is.na(hosp_out)) %>%
  mutate(
    hosp_out = factor(hosp_out, levels = c("NO", "YES")),
    icu_out = factor(icu_out, levels = c("NO", "YES")),
    sex = factor(sex, levels = c("Male", "Female", "Unknown")),
    hiv_status = factor(hiv_status, levels = c("HIV-", "HIV+")),
    sexor = factor(sexor, levels = c("MSM", "Heterosexual", "Other", "Unknown")),
    immunosuppression = factor(immunosuppression, levels = c("YES", "NO", "UNK")),
    hiv_immuno = factor(hiv_immuno, levels = c("HIV-/Not Immunosuppressed", "HIV+/Not Immunosuppressed", 
                                               "HIV-/Immunosuppressed", "HIV+/Immunosuppressed","Unknown"
                                               #"HIV+/Unknown", "HIV-/Unknown", "Unknown/Immunosuppressed", "Unknown/Not Immunosuppressed", "Unknown/Unknown"
    ))
  )


# age association

m0_glm <- glm(hosp_out ~ 1, family = binomial, data = ll_reg)
# m1_glm <- update(m0_glm, .~.+ age_group)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



m0_glmr <- lme4::glmer(hosp_out ~ 1 + (1 | country), family = "binomial", data = ll_reg)

AIC(m0_glm) ; AIC(m0_glmr) ; AIC(m0_glmr) < AIC(m0_glm)

null.id = -2 * logLik(m0_glm) + 2 * logLik(m0_glmr)
pchisq(as.numeric(null.id), df=1, lower.tail=F)



m1_glmr <- update(m0_glmr, .~.+ age_group)
m1_gmlr <- summary(m1_glmr)


# modelfit.all <- lme4::allFit(m1_glmr)
# overdisp_fun(m1_glmr)
# 
# modelfit.all <- lme4::allFit(m1_glmr)

sigfit <- anova(m1_glmr, m0_glmr, test = "Chi") 
# inspect
sigfit_age <- sigfit
overdisp_fun(m1_glmr)

m2_glmr <- update(m1_glmr, .~. + hiv_immuno)
sigfit <- anova(m2_glmr, m1_glmr, test = "Chi") 
sigfit
overdisp_fun(m2_glmr)
m3_glmr <- update(m2_glmr, .~. + sex)

sigfit <- anova(m3_glmr, m2_glmr, test = "Chi") 
sigfit

ll_reg2 <- ll_reg %>% 
  mutate(hosp_out = ifelse(hosp_out == "YES", 1, 0))

probs = 1/(1+exp(-fitted(m1_glmr)))
probs = binomial()$linkinv(fitted(m1_glmr))
Hmisc::somers2(probs, as.numeric(ll_reg2$hosp_out))


m3_glmr@frame$country %>% unique() %>% as.character() %>% sort() %>% paste0(collapse = ", ") %>% clipr::write_clip()

# R2
1-logLik(m1_glmr)/logLik(m0_glmr)

summary(m1_glmr)

# plot(m1_glmr)
# qqnorm(m1_glmr)

model_labs <- list(age_group = "Age group", sex = "Sex",
                   hiv_immuno = "HIV/immunocompromised status")




reg_tab_glmr <- tbl_regression(m3_glmr, 
                               exponentiate = TRUE, 
                               include = c(age_group, sex, hiv_immuno),
                               label = model_labs)

reg_tab_glmr


unadjusted <- 
  map(c("age_group", "sex", "hiv_immuno"), ~paste0(paste0("hosp_out ~ ", .x))) %>% 
  map(~glm(.x, family = "binomial", data = ll_reg)) %>% 
  map(gtsummary::tbl_regression, exponetiate = TRUE) %>% 
  map_df(~.$table_body)

unadjusted %>% 
  filter(!header_row) %>% 
  select(estimate, ci, p.value) %>% 
  clipr::write_clip()

cross_tab_drop <- ll_reg %>%
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = c(age_group, sex, hiv_immuno),
    label = model_labs, 
    percent = "row", 
    NULL, digits = c(everything() ~ 1) 
  )
cross_tab_drop


reg_hosp_new <- tbl_merge(list(
  cross_tab_drop, reg_tab_glmr),
  tab_spanner = c("**Hospital Admission**", "**MODEL**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")


gt::gtsave(reg_hosp_new, filename = "model_final.rtf")

reg_hosp_out <- tbl_merge(list(
  cross_tab_drop, reg_tab_glm, reg_tab_glmr),
  tab_spanner = c("**Hospital Admission**", "**BINOMIAL**", "**MIXED EFFECTS**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")
reg_hosp_out

gt::gtsave(reg_hosp_out, "tab3_1.rtf")

## HIV 


ll_hiv <- ll_reg %>% 
  group_by(country) %>% 
  filter(sum(hiv_immuno == "Unknown") != n(),
         age_group %in% c("15-44", "45-64", "65+")) %>% 
  mutate(hosp_out = ifelse(hosp_out == "YES", 1, 0))

ll_hiv %>% 
  ggplot(aes(x = age_group, fill = hiv_immuno)) +
  geom_bar(position = "fill")


m0_glm <- glm(hosp_out ~ 1, family = "binomial", data = ll_hiv)
m1_glm <- glm(hosp_out ~ hiv_immuno + 1, family = "binomial", data = ll_hiv)

summary(m1_glm)

AIC(m0_glm) ; AIC(m1_glm) ; AIC(m1_glm) < AIC(m0_glm)

null.id = -2 * logLik(m0_glm) + 2 * logLik(m1_glm)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

plot(m1_glm)

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


reg_tab_glm <- tbl_regression(m1_glm, 
                              exponentiate = TRUE, 
                              include = hiv_immuno,
                              label = model_labs)

reg_tab_glm


m0_glmr <- lme4::glmer(hosp_out ~ 1 + (1 | country), family = "binomial", data = ll_hiv)
AIC(m0_glm) ; AIC(m0_glmr) ; AIC(m0_glmr) < AIC(m0_glm)

null.id = -2 * logLik(m0_glm) + 2 * logLik(m0_glmr)
pchisq(as.numeric(null.id), df=1, lower.tail=F)


m1_glmr <- update(m0_glmr, .~.+ hiv_immuno)

sigfit <- anova(m1_glmr, m0_glmr, test = "Chi") 
# inspect
sigfit



probs = 1/(1+exp(-fitted(m1_glmr)))
probs = binomial()$linkinv(fitted(m1_glmr))
Hmisc::somers2(probs, as.numeric(ll_hiv$hosp_out))


# R2
1-logLik(m1_glmr)/logLik(m0_glmr)


plot(m1_glmr)

model_labs <- list(hiv_immuno = "HIV/immuno")


reg_tab_glm <- tbl_regression(m1_glm, 
                              exponentiate = TRUE, 
                              include = hiv_immuno,
                              label = model_labs)

reg_tab_glm


reg_tab_glmr <- tbl_regression(m1_glmr, 
                               exponentiate = TRUE, 
                               include = hiv_immuno,
                               label = model_labs)

reg_tab_glmr


cross_tab_drop <- ll_hiv %>%
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = hiv_immuno,
    label = model_labs, 
    percent = "row", 
    NULL
  )
cross_tab_drop


reg_hosp_out <- tbl_merge(list(
  cross_tab_drop, reg_tab_glm, reg_tab_glmr),
  tab_spanner = c("**Hospital Admission**", "**BINOMIAL**", "**MIXED EFFECTS**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")

reg_hosp_out

gt::gtsave(reg_hosp_out, "tab3_2.rtf")


# younger hiv -------------------------------------------------------------



ll_hiv_y <- ll_hiv %>% 
  filter(age_group %in% c("15-44", "45-64"))

ll_hiv_o <- ll_hiv %>% 
  filter(age_group %in% c("65+"))

m0_glm_hy <- glm(hosp_out ~ 1, family = "binomial", data = ll_hiv_y)
m1_glm_hy <- glm(hosp_out ~ hiv_immuno + 1, family = "binomial", data = ll_hiv_y)

summary(m1_glm_hy)

AIC(m0_glm_hy) ; AIC(m1_glm_hy) ; AIC(m1_glm_hy) < AIC(m0_glm_hy)

null.id = -2 * logLik(m0_glm_hy) + 2 * logLik(m1_glm_hy)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


m0_glmr_hy <- lme4::glmer(hosp_out ~ 1 + (1 | country), family = "binomial", data = ll_hiv_y)
AIC(m0_glm_hy) ; AIC(m0_glmr_hy) ; AIC(m0_glmr_hy) < AIC(m0_glm_hy)

null.id = -2 * logLik(m0_glm_hy) + 2 * logLik(m0_glmr_hy)
pchisq(as.numeric(null.id), df=1, lower.tail=F)


m1_glmr_hy <- update(m0_glmr_hy, .~.+ hiv_immuno)

sigfit <- anova(m1_glmr_hy, m0_glmr_hy, test = "Chi") 
# inspect
sigfit


probs = 1/(1+exp(-fitted(m1_glmr_hy)))
probs = binomial()$linkinv(fitted(m1_glmr_hy))
Hmisc::somers2(probs, as.numeric(ll_hiv_y$hosp_out))


# R2
1-logLik(m1_glmr_hy)/logLik(m0_glmr_hy)

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


reg_tab_glm_hy <- tbl_regression(m1_glm_hy, 
                                 exponentiate = TRUE, 
                                 include = hiv_immuno,
                                 label = model_labs)

reg_tab_glm_hy


reg_tab_glmr_hy <- tbl_regression(m1_glmr_hy, 
                                  exponentiate = TRUE, 
                                  include = hiv_immuno,
                                  label = model_labs)

reg_tab_glmr_hy


cross_tab_drop_hy <- ll_hiv_y %>%
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = hiv_immuno,
    label = model_labs, 
    percent = "row", 
    NULL
  )
cross_tab_drop_hy


reg_hosp_out_hy <- tbl_merge(list(
  cross_tab_drop_hy, reg_tab_glm_hy, reg_tab_glmr_hy),
  tab_spanner = c("**Hospital Admission**", "**BINOMIAL**", "**MIXED EFFECTS**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")

reg_hosp_out_hy

gt::gtsave(reg_hosp_out_hy, "tab3_3.rtf")





m0_glm_ho <- glm(hosp_out ~ 1, family = "binomial", data = ll_hiv_o)
m1_glm_ho <- glm(hosp_out ~ hiv_immuno + 1, family = "binomial", data = ll_hiv_o)

summary(m1_glm_ho)

AIC(m0_glm_ho) ; AIC(m1_glm_ho) ; AIC(m1_glm_ho) < AIC(m0_glm_ho)

null.id = -2 * logLik(m0_glm_ho) + 2 * logLik(m1_glm_ho)
pchisq(as.numeric(null.id), df=1, lower.tail=F)

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


m0_glmr_ho <- lme4::glmer(hosp_out ~ 1 + (1 | country), family = "binomial", data = ll_hiv_o)
AIC(m0_glm_ho) ; AIC(m0_glmr_ho) ; AIC(m0_glmr_ho) < AIC(m0_glm_ho)

null.id = -2 * logLik(m0_glm_ho) + 2 * logLik(m0_glmr_ho)
pchisq(as.numeric(null.id), df=1, lower.tail=F)


m1_glmr_ho <- update(m0_glmr_ho, .~.+ hiv_immuno)

sigfit <- anova(m1_glmr_ho, m0_glmr_ho, test = "Chi") 
# inspect
sigfit


probs = 1/(1+exp(-fitted(m1_glmr_ho)))
probs = binomial()$linkinv(fitted(m1_glmr_ho))
Hmisc::somers2(probs, as.numeric(ll_hiv_o$hosp_out))


# R2
# 1-logLik(m1_glmr_hy)/logLik(m0_glmr_hy)

model_labs <- list(hiv_immuno = "HIV/immunocompromised status")


reg_tab_glm_ho <- tbl_regression(m1_glm_ho, 
                                 exponentiate = TRUE, 
                                 include = hiv_immuno,
                                 label = model_labs)

reg_tab_glm_ho


reg_tab_glmr_ho <- tbl_regression(m1_glmr_ho, 
                                  exponentiate = TRUE, 
                                  include = hiv_immuno,
                                  label = model_labs)

reg_tab_glmr_ho


cross_tab_drop_ho <- ll_hiv_o %>%
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = hiv_immuno,
    label = model_labs, 
    percent = "row", 
    NULL
  )
cross_tab_drop_ho

ll_reg %>% 
  group_by(country) %>% 
  filter(sum(hiv_immuno == "Unknown") != n(),
         age_years< 15) %>% 
  mutate(hosp_out = ifelse(hosp_out == "YES", 1, 0)) %>% 
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = hiv_immuno,
    label = model_labs, 
    percent = "row", 
    NULL
  )



reg_hosp_out_ho <- tbl_merge(list(
  cross_tab_drop_ho, reg_tab_glm_ho, reg_tab_glmr_ho),
  tab_spanner = c("**Hospital Admission**", "**BINOMIAL**", "**MIXED EFFECTS**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")

reg_hosp_out_ho

gt::gtsave(reg_hosp_out_ho, "tab3_4.rtf")


unadjusted <- map(c("age_group", "sex", "hiv_immuno"), ~paste0(paste0("hosp_out ~ ", .x))) %>% 
  map(~glm(.x, family = "binomial", data = ll_reg)) %>% 
  map2(imap(model_labs, ~list(.x) %>% set_names(.y)), ~gtsummary::tbl_regression(.x, label = .y, exponentiate = TRUE))



ll_reg %>% 
  pull(country) %>% 
  unique() %>% 
  sort() %>% 
  paste0(collapse = ", ") %>% 
  clipr::write_clip()

ll_hiv %>% 
  pull(country) %>% 
  unique() %>% 
  sort() %>% 
  paste0(collapse = ", ") %>% 
  clipr::write_clip()


qplot(hiv)

m2_glmr <- update(m1_glmr, .~.+ sexor)

sigfit <- anova(m2_glmr, m1_glmr, test = "Chi") 
# inspect
sigfit

summary(m2_glmr)

probs = 1/(1+exp(-fitted(m2_glmr)))
probs = binomial()$linkinv(fitted(m2_glmr))
Hmisc::somers2(probs, as.numeric(ll_hiv$hosp_out))



model_labs <- list(hiv_immuno = "HIV/immunocompromised status",
                   sexor = "Sexual orientation")


reg_tab_glmr2 <- tbl_regression(m2_glmr, 
                                exponentiate = TRUE, 
                                include = c(sexor, hiv_immuno),
                                label = model_labs)

reg_tab_glmr2


m3_glmr <- update(m2_glmr, .~.+ age_group)

sigfit <- anova(m3_glmr, m2_glmr, test = "Chi") 
# inspect
sigfit


model_labs <- list(hiv_immuno = "HIV/immunocompromised status",
                   sexor = "Sexual orientation",
                   age_group = "Age group")


reg_tab_glmr3 <- tbl_regression(m3_glmr, 
                                exponentiate = TRUE, 
                                include = c(age_group, sexor, hiv_immuno),
                                label = model_labs)


plot(m3_glmr)
probs = 1/(1+exp(-fitted(m3_glmr)))
probs = binomial()$linkinv(fitted(m3_glmr))
Hmisc::somers2(probs, as.numeric(ll_hiv$hosp_out))



ll_reg %>% 
  ggplot(aes(x = age_group, fill = hiv_immuno)) +
  geom_bar(position = "fill")



ll_hiv <- ll_reg %>% 
  group_by(country) %>% 
  filter(sum(hiv_immuno == "Unknown") != n(),
         age_group %in% c("18-44", "45-64", "65+")) %>% 
  mutate(hosp_out = ifelse(hosp_out == "YES", 1, 0))




m0_glm = glm(hosp_out ~ 1, family = binomial, data = ll_hiv) 
# base-line mixed-model
m0_glmer = glmer(hosp_out ~ (1|country), data = ll_hiv, family = binomial) 


aic.glmer <- AIC(logLik(m0_glmer))
aic.glm <- AIC(logLik(m0_glm))
aic.glmer; aic.glm

null.id = -2 * logLik(m0_glm) + 2 * logLik(m0_glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F)



m1_mix <- lme4::glmer(
  hosp_out ~ age_group + (1 | country),
  ll_hiv,
  family = "binomial"
)
anova(m1_mix, m0_glmer)

m2_mix <- lme4::glmer(
  hosp_out ~ sexor + (1 | country),
  ll_hiv,
  family = "binomial"
)

anova(m2_mix, m1_mix)

m3_mix <- lme4::glmer(
  hosp_out ~ hiv_immuno + (1 | country),
  ll_hiv,
  family = "binomial"
)
qqnorm(m3_mix)

m3_mix <- glm(
  hosp_out ~ hiv_immuno ,
  ll_hiv,
  family = "binomial"
)

anova(m3_mix, m0_glmer)


probs = 1/(1+exp(-fitted(m3_mix)))
probs = binomial()$linkinv(fitted(m3_mix))
Hmisc::somers2(probs, as.numeric(ll_hiv %>% 
                                   mutate(hosp2 = ifelse(hosp_out == "YES", 1, 0)) %>% 
                                   pull(hosp2)))

model_labs <- list(hiv_immuno ~ "HIV/Immunosuppression Status", 
                   age_group = "Age group")

reg_tab_drop_mixed <- tbl_regression(m3_mix, 
                                     exponentiate = TRUE, 
                                     include = c(, hiv_immuno),
                                     label = model_labs)

reg_tab_drop_mixed

cross_tab_drop <- ll_hiv %>%
  tbl_summary(
    # group by outcome 
    by = hosp_out,
    include = c(age_group, hiv_immuno),
    label = model_labs, 
    percent = "row", 
    NULL
  )
cross_tab_drop


reg_hosp_out <- tbl_merge(list(
  cross_tab_drop, reg_tab_drop_mixed),
  tab_spanner = c("**Hospital Admission**", "**Mixed effects logistic regression**")
) %>% 
  modify_footnote(update = list(
    all_stat_cols() ~ "n (%) for categorical;\n median (IQR) for continuous")
  ) %>% 
  as_gt() %>% 
  tab_source_note(source_note = "Not shown: country included as a random effects variable") %>% 
  tab_header(title = "Outcome: Hospitalisation")
reg_hosp_out


# base-line mixed-model
m0.nb = glmer.nb(hosp_out ~ 1 + (1 | country), data = ll_hiv)
# add Shots
m1.nb <- update(m0.nb, .~.+ hiv_immuno)
anova(m1.nb, m0.nb)



# extract pearson residuals
PearsonResiduals <- resid(m1.nb, type = "pearson")
# extract number of betas + predictors + sigma
NumberOfPredictors <- 2+1+1
# extract number of cases in model
Cases <- nrow(ll_hiv)
# calculate overdispersion parameter
Overdispersion <- sum(PearsonResiduals^2) / (Cases / NumberOfPredictors)# show overdispersion parameter
Overdispersion

m2.nb <- update(m1.nb, .~.+ sex)
anova(m2.nb, m1.nb)


m3.nb <- update(m1.nb, .~.+ sexor)
anova(m3.nb, m1.nb)


m4.nb <- update(m1.nb, .~.+ hiv_immuno)
anova(m4.nb, m1.nb)



m0.nb = glm(hosp_out ~ 1, data = ll_reg2, family = binomial)
# add Shots
m1.nb <- update(m0.nb, .~.+ age_group)
anova(m1.nb, m0.nb)

m2.nb <- update(m1.nb, .~.+ sex)
AIC(m2.nb);AIC(m0.nb)
plot(m2.nb)


m3.nb <- update(m2.nb, .~.+ sexor)
AIC(m3.nb);AIC(m2.nb)
plot(m3.nb)


m4.nb <- update(m3.nb, .~.+ hiv_immuno)
AIC(m4.nb);AIC(m3.nb)
plot(m4.nb)



make_cross_tab <- function(x, gv = "age_group") {
  x %>% 
    rename(var = all_of(gv)) %>% 
    group_by(var) %>% 
    summarise(
      not_hosp = sum(hosp_out2 == "NO", na.rm = TRUE),
      hosp_treat = sum(hosp_out2 == "YTREAT", na.rm = TRUE),
      hosp_unk = sum(hosp_out2 == "YUNK", na.rm = TRUE)
    ) %>% 
    mutate(
      across(contains("hosp"), ~replace_na(., 0)),
      across(contains("hosp"), list("pct" = ~. / (not_hosp + hosp_treat + hosp_unk)))
    )
}

bind_rows(
  list(
    make_cross_tab(ll_reg, "age_group"),
    make_cross_tab(ll_reg, "sex"),
    make_cross_tab(ll_reg, "sexor"),
    make_cross_tab(ll_reg, "hiv_immuno"),
    make_cross_tab(ll_reg %>% mutate(all = "total"), "all")
  )
)