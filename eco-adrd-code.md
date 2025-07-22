---
title: "Economical Model of AD Costs"
subtitle: "Modelization for France"
author: "Gagliardi Geoffroy"
date: "2025-Q1"
format: pdf
editor: visual
toc: true
number-sections: true
toc-depth: 2
bibliography: ../infos/medeco-25.bib
csl: ../infos/nature.csl
link-citations: true
---

```{r setup}
#| label: setup
#| include: false

rm(list = ls())
knitr::opts_chunk$set(echo = FALSE, fig.width = 20, fig.height = 10, 
                      message = FALSE, warning = FALSE, results = "asis")

# Rendering ----
library(knitr)
library(kableExtra)
library(patchwork)

# Analyses ----
library(gtsummary)

# Wrangling Packages ----
library(tidyverse)
```

```{r fcts}
# Patients ----
f_ps <- function(df, var_aid, var_inst, var_ald, var_apa, var_sit, var_pat){
  dat <- df |> filter(
    santeaidant_etat == var_aid &
      instit == var_inst & 
      ald == var_ald & 
      apa == var_apa
  ) |> 
    distinct() |> ungroup() |> 
    arrange(diagnostic, stade) |> 
    select(diagnostic, stade, santeaidant_etat, instit, ald, everything(), -stade_i)
  
  # Coûts médicaux
  dat_consdiag <- dat |> select(diagnostic, stade, starts_with("consdiag_"))
  dat_suivi <- dat |> select(diagnostic, stade, matches("^suivi_.*_patient$"))
  dat_medsma <- dat |> select(diagnostic, stade, matches("^medsma_.*_patient$"))
  dat_hdj <- dat |> select(diagnostic, stade, matches("^hdj_.*_annuel$"))
  dat_psy <- dat |> select(diagnostic, stade, matches("^psycho_.*_patient$"))
  dat_hosp <- dat |> select(diagnostic, stade, matches("^hospi_.*_patient$"))

  # Coûts médico-sociaux à domicile
  dat_accj <- dat |> select(diagnostic, stade, matches("^accj_.*_patient$"))
  dat_heber <- dat |> select(diagnostic, stade, matches("^heber_.*_patient$"))
  dat_add <- dat |> select(diagnostic, stade, matches("^add_.*_patient$"))
  dat_inf <- dat |> select(diagnostic, stade, matches("^ssiad_.*_patient$"))
  dat_kine <- dat |> select(diagnostic, stade, matches("^kine_.*_patient$"))
  dat_ortho <- dat |> select(diagnostic, stade, matches("^ortho_.*_patient$"))
  dat_esa <- dat |> select(diagnostic, stade, matches("^esa_.*_patient$"))

  # Coûts médico-sociaux en institution
  dat_inssoin <- dat |> select(diagnostic, stade, matches("^institsoin_.*_patient$"))
  dat_insheb <- dat |> select(diagnostic, stade, matches("^institheber_.*_patient$"))
  dat_insdep <- dat |> select(diagnostic, stade, matches("^institdep_.*_patient$"))

  # Accidents
  dat_accgest <- dat |> select(diagnostic, stade, matches("^accidents_.*_patient$"))
  dat_accrout <- dat |> select(diagnostic, stade, matches("^accidentsroute_.*_patient$"))

  # Aidants
  dat_tpsaid <- dat |> select(diagnostic, stade, matches("^tpsaidant_.*_patient$"))
  dat_santaid <- dat |> select(diagnostic, stade, matches("^santeaidant_.*_annuel$")) |>
    left_join(y = dur_stade, by = c("diagnostic", "stade")) |>
    relocate(dur_stade, prepost, .after = stade) |>
    mutate(across(.cols = starts_with("santeaidant_"), .fns = ~ . * dur_stade)) |>
    group_by(diagnostic, stade) |>
    summarise_if(.predicate = is.numeric, sum) |> ungroup()
  dat_amenag <- dat |> select(diagnostic, stade, matches("^amenagement_"))
  
  dat_all <- map(
    .x = list(dat_consdiag, dat_suivi, dat_medsma, dat_hdj, dat_psy,
              dat_hosp, dat_accj, dat_heber, dat_inf, dat_kine, dat_add,
              dat_ortho, dat_esa, dat_inssoin,dat_insheb, dat_insdep,
              dat_accgest, dat_accrout, dat_tpsaid, dat_santaid, dat_amenag),
    .f = ~ .x |>
      rename_with(.cols = everything(), .fn = ~ str_remove(., "_patient")) |>
      rename_with(.cols = everything(), .fn = ~ str_remove(., "_annuel"))
  ) |>
    plyr::join_all(by = c("diagnostic", "stade"), type = "left") |>
    select(-dur_stade) |>
    pivot_longer(
      cols = -c(diagnostic, stade),
      names_to = "poste",
      values_to = "cout"
    ) |>
    separate(col = poste, into = c("type", "financeur"), sep = "_") |>
    pivot_wider(names_from = "financeur", values_from = "cout") |> 
    mutate(
      patient = as.factor(var_pat),
      situation = as.factor(var_sit)
      ) |> 
    relocate(patient, situation, .after = stade)
  
  dat_p1 <- dat_all
  dat_p2 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0, .x))
    )
  dat_p3 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "tpsaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "santeaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "amenagement", .x * 0, .x)),
      
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0.4, .x))
    )
  dat_p4 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0.5, .x))
    )
  dat_p5 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0.5, .x))
    )
  dat_p6 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0.5, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0.4, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "tpsaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "santeaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "amenagement", .x * 0, .x))
    )
  dat_p7 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0, .x))
    )
  dat_p8 <- dat_all |> 
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0, .x))
    )
  dat_p9 <- dat_all |>
    mutate(
      across(c("cout", v_financeur), ~ ifelse(type == "medsma", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "hdj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "accj", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "heber", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "add", .x * 0.4, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ssiad", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "kine", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "ortho", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "esa", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "tpsaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "santeaidant", .x * 0, .x)),
      across(c("cout", v_financeur), ~ ifelse(type == "amenagement", .x * 0, .x))
    )

  return(list(df = dat_all,
              d1 = dat_p1, d2 = dat_p2, d3 = dat_p3,
              d4 = dat_p4, d5 = dat_p5, d6 = dat_p6, 
              d7 = dat_p7, d8 = dat_p8, d9 = dat_p9,
              consdiag = dat_consdiag, suivi = dat_suivi,
              medsma = dat_medsma, psycho = dat_psy, hdj = dat_hdj,
              hospi = dat_hosp, accj = dat_accj, heberg = dat_heber,
              ssiad = dat_inf, kine = dat_kine, ortho = dat_ortho, esa = dat_esa,
              add = dat_add, instsoin = dat_inssoin, instheberg = dat_insheb,
              instdep = dat_insdep, accgest = dat_accgest, accrout = dat_accrout,
              aid_tps = dat_tpsaid, aid_sant = dat_santaid, amneag = dat_amenag
  ))
}

# Tables ----
t_ps_cost_glob <- function(df){
  df |> 
    group_by(diagnostic, stade, situation) |> 
    summarise(total = sum(cout, na.rm = TRUE)) |> 
    mutate(annuel = total / 14) |> 
    group_by(diagnostic) |> 
    mutate(diag_tot = sum(total), diag_an = sum(annuel))
}
t_ps_cost_dets <- function(df){
  df |> 
    group_by(diagnostic, stade, situation, type) |> 
    summarise(total = sum(cout, na.rm = TRUE)) |> 
    pivot_wider(names_from = type, values_from = total)
}

# Plots ----
cbp <- c("#E69F00",  "#0072B2", "#009E73", "#CC79A7", "#999999", "#56B4E9", "#D55E00", "#F0E442", "#333333") 
p_ps_cost_det <- function(df){
  df |> 
    pivot_longer(
      cols = -c(diagnostic, stade, type, situation), 
      names_to = "var", 
      values_to = "vals"
    ) |> filter(var != "cout") |> 
    ggplot(aes(x = type, y = vals, fill = var, col = var)) + 
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(stade ~ diagnostic) + # , scales = "free"
    theme_bw() + theme(
      legend.position = "bottom", 
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
p_ps_cost_glob <- function(df){
  df |> 
    pivot_longer(
      cols = -c(diagnostic, stade, type, situation), 
      names_to = "var", 
      values_to = "vals"
    ) |> filter(var == "cout") |> 
    ggplot(aes(x = type, y = vals)) + 
    geom_bar(stat = "identity") +
    facet_wrap(stade ~ diagnostic) + # , scales = "free"
    theme_bw() + theme(
      legend.position = "bottom", 
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Comparaisons ----
f_1425_postcouts <- function(annee, type = "dets") {
    switch (type,
            "dets" = df <- rename(.data = annee[["t_pg_an_fn_diag"]], couts_gps = type),
            "gps" = df <- annee[["t_pg_an_fn_diag_coutgp"]]
    )
  df <- df |> 
      pivot_longer(
        cols = -c(diagnostic, couts_gps)
      )
  
  if (type == "dets") {
    df |> 
        filter(name != "cout") |> 
      ggplot(aes(x = diagnostic, y = value, fill = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(
        legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      labs(x = " ", y = " ") +
      scale_fill_manual(values = cbp) +
      theme(axis.text=element_text(size=10))
  } else {
    df |> 
      filter(name != "cout") |> 
      ggplot(aes(x = couts_gps, y = value, fill = name, col = name)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(
        legend.position = "top", 
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      facet_wrap(~ diagnostic) +
      labs(x = " ", y = " ") +
      scale_fill_manual(values = cbp) +
      theme(
        axis.text=element_text(size=10)
      )
  }
}

f_1425_stadescouts <- function(annee, type = "dets") {
    switch (type,
            "dets" = df <- rename(.data = annee[["t_pg_an_fn_diag"]], couts_gps = type),
            "gps" = df <- annee[["t_pg_an_fn_diag_coutgp"]]
    )
    
  p <- df |> 
    pivot_longer(cols = -c(diagnostic, couts_gps)) |> 
      filter(name == "cout") |> 
      ggplot(aes(x = diagnostic, y = value, fill = couts_gps))
  
  switch (type,
          "gps" = {p + geom_bar(stat = "identity", position = "fill") +
            theme(
              legend.position = "top", 
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(x = " ", y = " ") +
            scale_fill_manual(values = cbp)},
          "dets" = {p + geom_bar(stat = "identity", position = "dodge") +
            theme(
              legend.position = "top", 
              axis.text.x = element_text(angle = 45, hjust = 1)
            ) +
            labs(x = " ", y = " ") +
            theme(axis.text=element_text(size=10))}
  )
  
}

```

```{r vects}
ltx <- c("scale_down", "HOLD_position", "repeat_header")

v_diag <- c("Précoce", "Moyen", "Tardif")
v_stade <- c("Léger", "Modéré", "Sévère")
v_stade_i <- c("I1", "I2", "I3")
v_financeur <- c("famille", "sécu", "complementaire", "conseilgeneraux")
v_suivi_type <- c("Généraliste", "Spécialiste Seul", "Spé + Géné")

v_tbl_order <- c(
  "consdiag", "suivi", "medsma", "psycho", "hospi", "hdj", "accj", 
  "heber", "ssiad", "kine", "esa", "ortho", "add", 
  "institheber", "institsoin", "institdep", "accidents", 
  "accidentsroute", "tpsaidant", "santeaidant", "amenagement")

v_tbl_nms <- c(
  "Consultations Diagnostiques", "Consultations de Suivi", 
  "Médication Alzheimer", "Psychotropes", "Hospitalisation", 
  "Hôpital de Jour", "Accueil de Jour", "Hébergement Temporaire", 
  "SSIAD", "Kiné", "ESA", "Ortho", "Aides à Domicile", 
  "Insitution - Hébergement", "Institution - Soin", 
  "Instutition - Dépendance", "Accidents de Gestion", 
  "Accidents de la Route", "Aidant: Temps", "Aidant: Santé", 
  "Aménagements"
)
```

```{r import}
# 
```

\newpage

# Context

## Costs Types

Medical Costs

  * Diagnostic consultation costs
  * Follow-up consultation costs
  * Anti-Alzheimer medication costs.
  * Psychotropic medication costs.
  * Hospitalization costs
  * Day hospital follow-up costs

Medical and social costs at home

  * Home help costs
  * Paramedical expenses
  * Day care costs
  * Temporary accommodation costs
  
Medical and social costs in institutions

  * Care package
  * Institutional dependency package
  * Residential care costs
  
Accident-related costs

  * Road accident costs
  * Management accident costs
  
Caregiver-related costs

  * The cost of time spent by the caregiver on the patient
  * Costs of home improvements
  * Costs related to the caregiver's state of health


## Payors

Payors :

  * `Families`: Patients, caregivers, families,
  * `Social Security System`,
  * `Private`: Private insurance companies, etc.,
  * `Departmental Councils`

## Patients' Profiles

Patients :

  * AD Treatment : Y/N
  * Paramedical Treatment: Y/N
  * Day-Care Hospital Follow-up: Y/N
  
Patients' Situations: 

  * Extensive Treatment : 100% of the prescription
  * Moderate Treatment: 50%
  * Low Treatment: psychotropic drugs only
  
Caregivers: 

  * Day-Care: Y/N
  * Temporary Accomodation: Y/N
  * Homecare: Y/N
  * Health Status
  
Caregivers' Situation: 

  * Involved with a respite solution: 100%
  * Involved without a respite solution
  * No Caregiver, 40% (recommanded).
  
Typical Situations : 

  * ALD Help Access : Y/N
  * APA Help Access : 0%, 50%, 100%
  * Palliative solution for the end of life : Y/N + stage if yes. 

## Situations

```{r d_profiles}
d_profiles_patients <- tibble(
  qui = "patients",
  couts = rep(c("meds_ad", "meds_psy", "parameds", "hdj"), 3),
  profil = unlist(map(.x = 1:3, rep, 4)), 
  psc = c(
    1, 1, 1, 1, 
    0, 1, 0.5, 0, 
    0, 1, 0, 0
  )
)

d_profiles_aidant <- tibble(
  qui = "aidants",
  couts = rep(c("accj", "heber", "add", "aidantetat"), 3),
  profil = unlist(map(.x = 1:3, rep, 4)), 
  psc = c(
    1, 1, 1, 1, 
    0, 0, 0, 0.5, 
    0, 1, 0.4, NA
  )
)

d_profiles <- rbind(d_profiles_patients, d_profiles_aidant) |> 
  pivot_wider(names_from = profil, values_from = psc)

d_profiles |> 
  kable(caption = "Situations", format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)

d_pataid_profiles <- expand.grid(aidants = 1:3, patients = 1:3) |> 
  mutate(type = 1:9) |> 
  pivot_wider(names_from = patients, values_from = type) |> 
  rename("Aidants" = aidants) 

d_pataid_profiles |> 
  kable(caption = "Patients Types", format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx) |> 
  add_header_above(c(" ", "Patients" = 3))
```

```{r d_profiles_freq}
d_profils_freq <- expand.grid(
  prof_patient = 1:3,
  prof_aidant = 1:3
) |> 
  mutate(
    patient = case_when(
      prof_patient == 1 & prof_aidant == 1 ~ 1,
      prof_patient == 1 & prof_aidant == 2 ~ 2,
      prof_patient == 1 & prof_aidant == 3 ~ 3,
      prof_patient == 2 & prof_aidant == 1 ~ 4,
      prof_patient == 2 & prof_aidant == 2 ~ 5,
      prof_patient == 2 & prof_aidant == 3 ~ 6,
      prof_patient == 3 & prof_aidant == 1 ~ 7,
      prof_patient == 3 & prof_aidant == 2 ~ 8,
      prof_patient == 3 & prof_aidant == 3 ~ 9,
    ),
    patient = as.factor(patient),
    prop_patient = case_when(
      prof_patient == 1 ~  0.4,
      prof_patient == 2 ~  0.4,
      prof_patient == 3 ~  0.2
    ),
    prop_aidant = case_when(
      prof_aidant == 1 ~  0.42,
      prof_aidant == 2 ~  0.42,
      prof_aidant == 3 ~  0.16
    ),
    freq = prop_patient * prop_aidant
  )

d_profils_freq |> 
  kable(caption = "Fréquence des profils", format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)
```

```{r d_sit}
d_sit <- expand.grid(
  instit = c("Avec", "Sans"),
  ald = c("Avec", "Sans"),  
  apa = c(1, 0.5, 0)
) |> 
  arrange(instit, ald, desc(apa)) |> 
  mutate(
    instit_prop = ifelse(instit == "Avec", 0.4, 0.6),
    ald_prop = ifelse(ald == "Avec", 0.6, 0.4),
    apa_prop = case_when(
      apa == 1 ~ 0.04, 
      apa == 0.5 ~ 0.17, 
      apa == 0 ~ 0.79, 
      ),
    prop = instit_prop * ald_prop * apa_prop,
    pc_round = 100 * round(prop, 3),
    situation = factor(case_when(
      instit == "Avec" & ald == "Avec" & apa == 1 ~ 1,
      instit == "Avec" & ald == "Avec" & apa == 0.5 ~ 2,
      instit == "Avec" & ald == "Avec" & apa == 0 ~ 3,
      instit == "Sans" & ald == "Avec" & apa == 1 ~ 4,
      instit == "Sans" & ald == "Avec" & apa == 0.5 ~ 5,
      instit == "Sans" & ald == "Avec" & apa == 0 ~ 6,
      instit == "Sans" & ald == "Sans" & apa == 1 ~ 7,
      instit == "Sans" & ald == "Sans" & apa == 0.5 ~ 8,
      instit == "Sans" & ald == "Sans" & apa == 0 ~ 9,
      instit == "Avec" & ald == "Sans" & apa == 1 ~ 10,
      instit == "Avec" & ald == "Sans" & apa == 0.5 ~ 11,
      instit == "Avec" & ald == "Sans" & apa == 0 ~ 12
    ))
  ) |> relocate(situation)

d_sit |> 
  kable(caption = "Situations et Proportions", format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)
```

```{r dur_stade}
dur_stade <- expand.grid(
  diagnostic = v_diag,
  stade = v_stade, 
  prepost = c("pre", "post")
) |> 
  mutate(
    keep = case_when(
      diagnostic == "Précoce" & stade == "Modéré" & prepost == "pre" ~ FALSE, 
      diagnostic == "Moyen" & stade == "Léger" & prepost == "post" ~ FALSE, 
      diagnostic == "Tardif"  & stade == "Léger" & prepost == "post" ~ FALSE, 
      stade == "Sévère" & prepost == "pre" ~ FALSE,
      TRUE ~ TRUE
    ),
    dur_stade = case_when(
      diagnostic == "Précoce" & stade == "Léger" & prepost == "pre" ~ 3, 
      diagnostic == "Précoce" & stade == "Léger" & prepost == "post" ~ 2,
      diagnostic == "Précoce" & stade == "Modéré" ~ 6,
      diagnostic == "Précoce" & stade == "Sévère" ~ 3,
      diagnostic == "Moyen" & stade == "Léger" ~ 5, 
      diagnostic == "Moyen" & stade == "Modéré" & prepost == "pre" ~ 1, 
      diagnostic == "Moyen" & stade == "Modéré" & prepost == "post" ~ 4.5,
      diagnostic == "Moyen" & stade == "Sévère" ~ 3.5,
      diagnostic == "Tardif" & stade == "Léger" ~ 5, 
      diagnostic == "Tardif" & stade == "Modéré" & prepost == "pre" ~ 4, 
      diagnostic == "Tardif" & stade == "Modéré" & prepost == "post" ~ 1,
      diagnostic == "Tardif" & stade == "Sévère" ~ 4,
    )
  ) |> 
  filter(keep) |> select(-keep) |> arrange(diagnostic, stade) 

```

```{r d_sitadj}
d_sitadj <- d_sit |> 
  filter(situation %in% c(3, 6, 9, 12)) |> 
  mutate(
    pc_adj = prop / sum(prop)
  )

d_sit <- d_sit |> 
  left_join(
    y = d_sitadj, 
    by = c("situation", "instit", "ald", "apa", 
           "instit_prop", "ald_prop", "apa_prop", "prop", "pc_round"
    )
  )

d_sit |> 
  kable(caption = "Situations et Proportions + Ajustements pour P3, 6, 9, 12", 
        format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)
```

```{r d_combinaison}
d_combinaison <- d_sit |> 
  select(situation, instit, ald, apa, prop, pc_adj) |> 
  left_join(
    y = expand.grid(
      situation = as.factor(1:12), 
      santeaidant_etat = c("Bon", "Mauvais")), 
    by = "situation"
    ) |> 
  left_join(
    y = expand.grid(
      situation = as.factor(1:12), 
      patient = as.factor(1:9)), 
    by = "situation"
  ) |> 
  select(patient, situation, everything()) |> 
  arrange(patient, situation) |> 
  mutate(
    keep = case_when(
      patient %in% c(1, 4, 7) & santeaidant_etat == "Mauvais" ~ FALSE, # Bonne santé
      patient %in% c(3, 6, 9) & santeaidant_etat == "Mauvais" ~ FALSE, # Pas d'aidant
      patient %in% c(2, 5, 8) & santeaidant_etat == "Bon" ~ FALSE, # Mauvaise Santé aidants
      patient %in% c(2, 5, 8) & !situation %in% c(3, 6, 9, 12) ~ FALSE, # Que ces situations ici
      TRUE ~ TRUE
    ),
    freq = case_when(
      patient %in% c(2, 5, 8) & situation %in% c(3, 6, 9, 12) ~ pc_adj, 
      TRUE ~ prop
    )
  ) |> 
  filter(keep) |> 
  mutate(combinaison = glue::glue("P{patient}S{situation}")) |> 
  relocate(combinaison) |> 
  select(-c(prop, pc_adj, keep))
```

\newpage

# Model

```{r ma_prop_2022}
# PAQUID from 2014, prev 2020
ma_prop_2014 <- 1198259.202

# Gabelle et al., 2023 on prev in 2022
ma_gabelle_low <- 1720000
ma_gabelle_mid <- 2528000
ma_gabelle_high <- 3642000

# Source Data Ameli
# https://data.ameli.fr/pages/pathologies/?refine.patho_niv1=Maladies%20neurologiques&refine.patho_niv2=Démences%20(dont%20maladie%20d%27Alzheimer)

# Nombre de personnes 
demences_ameli_2022 <- 710450
demences_all_2022 <- demences_ameli_2022 * 2
ma_prop_2022 <- demences_all_2022 * 0.7
# ma_prop_2020 - ma_prop_2014
```

```{r ma_apa}
# APA MAJ ----
# https://www.service-public.fr/particuliers/vosdroits/F10009
d_apans <- tibble(
  revenus = c("<710", "710_1500", ">1500"),
  prop = c(16, 63, 21)
) |> 
  janitor::adorn_totals(where = "row") |> as_tibble()

d_gir <- tibble(
  gir = c(1, 2, 3, 4), 
  montant_max = c(2045.56, 1654.18, 1195.67, 797.96)
)
# mean(c(2045.56, 1654.18)) = 1849.87
# mean(c(1195.67, 797.96)) = 996.815
d_apastades <- tibble(
  stade = c("Sévère", "Modéré"),
  apa_montant = c(1850, 997)
)

d_apaduree <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 6)),
  stade = rep(unlist(map(.x = v_stade, .f = rep, 2)), 3),
  instit = rep(rep(c("Sans", "Avec"), 3), 3),
  apa_annees = c(
    # precoce 
    0, 0, 6, 3, 3, 0,
    # Modere
    0, 0, 4.5, 1.8, 3.5, 0,
    # Severe
    0, 0, 1, 0, 4, 0
  )
)

d_apa <- full_join(x = d_apastades, y = d_apaduree, by = "stade") |> 
  mutate(
    apa_cout_annuel = ifelse(apa_annees == 0, 0, apa_montant), 
    apa_cout_patient = apa_cout_annuel * apa_annees
    ) |> 
  select(diagnostic, stade, instit, everything())
```

```{r modai_consdiag}
# Consultation Diagnostic MAJ ----
tarif_hospi <- 1300

d_cout_consult_diag <- tibble(
  stade = v_stade, 
  parcours = c("hospit", "hospit", "hospit"),
  consult_diag_prix = c(tarif_hospi, tarif_hospi, 0)
)

d_consdiag_pec <- tibble(
  stade = unlist(map(.x = v_stade[1:2], rep, length(v_financeur))),
  financeur = rep(v_financeur, 2), 
  pec_consdiag_prop = c(
    1, 72, 27, 0,
    1, 70, 29, 0
  )
)

# Formatage ----

d_consult_diag <- d_cout_consult_diag |>
  left_join(
    expand.grid(diagnostic = v_diag, stade = v_stade), 
    by = "stade"
  ) |> 
  relocate(diagnostic) |>
  arrange(diagnostic, stade) |> 
  mutate(
    across(
      .cols = is.numeric, 
      .fns = ~ case_when(
        stade == "Sévère" ~ 0, 
        diagnostic == "Précoce" & stade == "Modéré" ~ 0,
        diagnostic == "Moyen" & stade == "Léger" ~ 0,
        diagnostic == "Tardif" & stade == "Léger" ~ 0,
        TRUE ~ .x
      )
    )
  ) |> 
  rename(consdiag_cout = consult_diag_prix) |> 
  left_join(pivot_wider(
    d_consdiag_pec,
    names_from = financeur, 
    values_from = pec_consdiag_prop, 
    names_prefix = "consdiag_"), 
    by = "stade"
  ) |> 
  mutate(
    across(
      .cols = contains(v_financeur), 
      .fns = ~ ifelse(is.na(.), 0, .)
    )
  ) |> 
  mutate(
    across(
      .cols = contains(v_financeur), 
      .fns = ~ consdiag_cout * (.x / 100)
    )
  )


```

```{r modai_suivi}
# Consultation de Suivi 2025 ----

# Source Ameli, 26, février 2025
# https://www.ameli.fr/medecin/textes-reference/convention-medicale-2024-2029/les-negociations-en-pratique/panorama-medecins-concernes
# Pour les Gériatres source : https://www.profilmedecin.fr/contenu/chiffres-cles-medecin-geriatre/#:~:text=En%202022%2C%20les%20gériatres%20sont,l'âge%20moyen%20par%20sexe.

# Sources:
# https://www.ameli.fr/assure/actualites/tarifs-des-medecins-generalistes-et-specialistes-ce-qui-change-le-22-decembre-2024#:~:text=La%20consultation%20du%20médecin%20généraliste,50%20€%20à%2037%20€.
# https://www.ameli.fr/medecin/exercice-liberal/facturation-remuneration/consultations-actes/tarifs/tarifs-conventionnels-medecins-generalistes-specialistes

v_prix_generaliste <- 30
v_prix_specialiste <- 50

d_conssuivi_tps <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 6)),
  stade = rep(unlist(map(.x = v_stade, .f = rep, 2)), 3),
  instit = rep(rep(c("Sans", "Avec"), 3), 3),
  duree_ma = c(
    # precoce 
    2, 2, 6, 3, 3, 0,
    # Modere
    0, 0, 4.5, 1.8, 3.5, 0,
    # Severe
    0, 0, 1, 0, 4, 0
  )
)

d_conssuivi_ns <- tibble(
  suivi_type_consult_k = unlist(map(.x = v_suivi_type, rep, 3)),
  stade = rep(v_stade, 3),
  stade_i = c("I1", "I2", "I3", rep(v_stade_i, 2)),
  suivi_pec_prop_p = c(rep(90, 3), rep(10, 3), rep(45, 3)), 
  suivi_avg_prix_c = c(rep(v_prix_generaliste, 3), rep(v_prix_specialiste, 6)),
  suivi_ncki = c(4, rep(12, 2), rep(3, 3), rep(1.5, 3)), 
)

d_conssuivi_pec <- tibble(
  financeur = unlist(map(.x = v_financeur, .f = rep, 2)),
  ald = rep(c("Avec", "Sans"), 4), 
  conssuivi_pec = c(0, 0, 100, 70, 0, 30, 0, 0)
)

# Formatage ----

d_conssuivi <- d_conssuivi_ns |> 
  mutate(cout = (suivi_pec_prop_p/100) * suivi_avg_prix_c * suivi_ncki) |> 
  group_by(stade, stade_i) |> 
  summarise(suivi_cout_annuel = sum(cout)) |> 
  left_join(d_conssuivi_tps, by = "stade") |> 
  mutate(suivi_cout_patient = suivi_cout_annuel * duree_ma) |> 
  cross_join(pivot_wider(
    d_conssuivi_pec,
    names_from = financeur, 
    values_from = conssuivi_pec)
  ) |> 
  mutate(
    across(
      .cols = v_financeur, 
      .fns = ~ suivi_cout_annuel * (. / 100), 
      .names = "suivi_{.col}_annuel"
    ), 
    across(
      .cols = v_financeur, 
      .fns = ~ suivi_cout_patient * (. / 100), 
      .names = "suivi_{.col}_patient"
    )
  ) |> 
  select(stade, stade_i, diagnostic, instit, ald, duree_ma, 
         ends_with("_annuel"), ends_with("_patient"))

```

```{r modai_medsma}
# Médicaments Alzheimer MAJ ----
# Sources: 
# https://www.vidal.fr/maladies/systeme-nerveux/maladie-alzheimer/medicaments.html
# https://www.vaincrealzheimer.org/la-maladie/traitements/
# https://www.pasteur.fr/fr/centre-medical/fiches-maladies/alzheimer-maladie#:~:text=Il%20existe%20quatre%20médicaments%20disponibles,améliorer%20certains%20troubles%20du%20comportement.
# https://linote.fr/blog/alzheimer-traitement/#:~:text=Les%20médicaments%20contre%20l'Alzheimer,50%20euros%20par%20mois%20environ.

# MAJ VIDAL = mardi 18 mars 2025

d_coutmedsma <- tibble(
  medicament = c("Aricept (10mg)", "Aricept (10mg)", "Donepezil (10mg)", "Exelon (9.5mg)", "Galantamine (16mg)", "Ebixa (10mg)"),
  labo = c("Eisai", "BB Farma", "Actavis/Biogaran/Arrow", "Novartis Pharma", "Biogaran", "Lundbeck"),
  prix = c(27.72, 26.30, 22.03, 26.03, 30.54, 45.02),
  n = c(28, 28, 28, 30, 28, 56),
  npj = c(1, 1, 1, 1, 1, 1) # N par jour
) |> 
  mutate(
    unitaire = prix / n,
    journalier = unitaire * npj,
    annuel = journalier * 365.25,
    Léger = ifelse(str_detect(medicament, "Aricept|Donepezil|Exelon"), TRUE, FALSE),
    Modéré = ifelse(str_detect(medicament, "Aricept|Donepezil|Exelon|Ebixa|Galantamine"), TRUE, FALSE),
    Sévère = ifelse(str_detect(medicament, "Ebixa"), TRUE, FALSE)
  ) |> 
  select(-c(labo, prix, n, npj, unitaire, annuel)) |> 
  pivot_longer(
    cols = v_stade, names_to = "stade", values_to = "cout"
  ) |> 
  arrange(stade) |> 
  filter(cout) |> 
  group_by(stade) |> 
  summarise(cout_medsma_journalier = mean(journalier)) |> 
  mutate(cout_medsma_Kcompl = 1.2, 
         cout_medsma_annuel_max = c(990, 1151, 869))

d_cout_medsma_pec <- tibble(
  financeur = v_financeur,
  cout_medsma_pec= c(100, 0, 0, 0)
)

#  Formatage ----
d_medsma <- d_coutmedsma |> 
  mutate(
    medsma_cout = 
      ifelse(
        stade == "Modéré", 
        365.25 * cout_medsma_journalier * cout_medsma_Kcompl, 
        365.25 * cout_medsma_journalier
      )
  )
  
d_medsma <- d_medsma |> 
  cross_join(
    pivot_wider(
      d_cout_medsma_pec, 
      names_from = financeur, 
      values_from = cout_medsma_pec, 
      names_prefix = "medsma_"
    )
  ) |> 
  mutate(
    across(
      .cols = str_c("medsma_", v_financeur), 
      .fns = ~ medsma_cout * (./100)
    )
  ) |> 
  select(
    stade, starts_with("medsma_")
  ) |> 
  rename_with(.cols = contains("medsma_"), .fn = ~ str_c(., "_annuel"))

d_medsma <- full_join(
  x = d_conssuivi_tps, 
  y = d_medsma, 
  by = "stade"
) |> 
  mutate(
    across(
      .cols = contains("_annuel"),
      .fns = ~ . * duree_ma,
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    )
  )
```

```{r modai_psycho}
# Psychotropes MAJ ----
# Source
# https://www.vidal.fr/medicaments/gammes/risperdal-8865.html
# https://www.has-sante.fr/jcms/c_1728502/fr/risperdal-risperdaloro-risperidone-antipsychotique

d_psycho_cout <- tibble(
  medicament = "risperdal",
  dosage_mg = c(1, 2, 4),
  prix = c(8.11, 16.13, 8.53),
  n = c(60, 60, 30)
) |> 
  mutate(
    unitaire = prix / n,
    annuel = unitaire * (6 * 7)
  ) |> 
  filter(dosage_mg == 1) |> 
  select(psycho_coutannuel_max = annuel) |> 
  cbind(tibble(stade = v_stade)) |> 
  relocate(stade) |> 
  mutate(psycho_coutannuel_max = ifelse(
    stade == "Léger", 0, psycho_coutannuel_max
  ))

d_psycho_pec <- tibble(
  financeur = unlist(map(.x = v_financeur, .f = rep, 2)),
  ald = rep(c("Avec", "Sans"), 4), 
  psycho_pec = c(0, 0, 100, 65, 0, 35, 0, 0)
)

# Formatage ----
d_psycho <- d_psycho_cout |>
  rename(psycho_cout = psycho_coutannuel_max) |> 
  cross_join(
    pivot_wider(
      d_psycho_pec, 
      names_from = financeur, 
      values_from = psycho_pec, 
      names_prefix = "psycho_"
    )
  ) |> 
  mutate(
    across(
      .cols = str_c("psycho_", v_financeur), 
      .fns = ~ psycho_cout * (./100)
    )
  ) |> 
  select(
    stade, ald, psycho_cout, matches(str_c("psycho_", v_financeur))
  ) |> 
  rename_with(.cols = contains("psycho_"), .fn = ~ str_c(., "_annuel"))

d_psycho <- full_join(
  x = d_conssuivi_tps, 
  y = d_psycho, 
  by = "stade"
) |> 
  mutate(
    across(
      .cols = contains("_annuel"),
      .fns = ~ . * duree_ma,
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    )
  )

d_psycho <- d_psycho |> 
  mutate(
    across(
      .cols = contains("psycho"), 
      .fns = ~ ifelse(stade == "Léger", 0, .x)
    )
  )

```

```{r modai_hdj}
# HDJ MAJ ----
# Sources: 
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/solutions-d-accueil-temporaire/aller-a-l-accueil-de-jour

d_hdj_coutmax <- tibble(
  stade = v_stade, 
  cout = c(0, 300, 0), 
  duree = c(0, 52, 0) 
  # hdj_cout_annuel_max = c(0, 15600, 0)
) |> 
  mutate(
    hdj_cout_annuel = cout * duree
  ) |> 
  select(-c(cout, duree))

d_hdj_pec <- tibble(financeur = v_financeur, pec_hdj_prop = c(1, 70, 29, 0))

# Formatage ----
d_hdj <- d_hdj_coutmax |> 
  cbind(
    pivot_wider(d_hdj_pec, names_from = financeur, values_from = pec_hdj_prop)
  ) |> 
  mutate(
    across(
      .cols = v_financeur, 
      .fns = ~ hdj_cout_annuel * (. / 100), 
      .names = "{.col}_annuel"
    )
  ) |> 
  select(stade, ends_with("_annuel")) |> 
  rename_with(
    .cols = str_c(v_financeur, "_annuel"), 
    .fn = ~ str_c("hdj_", .)
  )
```

```{r modai_hopsi}
# Hospi MAJ ----
# Source: 
# https://www.ameli.fr/assure/remboursements/rembourse/hospitalisation-chirurgie
forfait_hospi <- tibble(
  etablissement = c("hopital/clinique", "service psy/etab.sante"),
  cout = c(20, 15)
) |> 
  summarise(cout = mean(cout))

# Source: 
# https://www.smatis.fr/particuliers/mutuelle-sante/mutuelle-hospitalisation/combien-coute-une-hospitalisation-sans-mutuelle/#:~:text=hospitalisation%20en%20France-,Le%20coût%20moyen%20d'une%20journée%20d'hospitalisation%20en%20France,classiques%2C%20et%20les%20honoraires%20médicaux.

tarif_med_poly <- 1175.84 # médecine polyvalente
tarif_med_ger_addict <- 1135.96 # médecine gériatrique-addictologie
tarif_chir <- 1523.95 # chirurgie

tarif_hospi <- mean(c(tarif_med_poly, tarif_med_ger_addict, tarif_chir))
tarif_hospit_all <- (80*tarif_hospi)/100 + (20*tarif_hospi)/100 + forfait_hospi$cout
pec_hospi_secu <- ((80*tarif_hospi)/100 * 100) / tarif_hospit_all
pec_hospi_comp <- ((20*tarif_hospi)/100 * 100) / tarif_hospit_all
pec_hospi_fam <- forfait_hospi$cout * 100 / tarif_hospit_all

d_hospi_fixes <- tibble(hospi_avgcost = tarif_hospi, hospi_Rfact = 2)

d_hospi_cout <- tibble(
  age = c("65-79", "65-79", "+80", "+80"),
  diag_YN = rep(c("Non", "Oui"), 2), 
  hospi_cout = tarif_hospit_all,
  hospi_tps_jours = c(4.03, 4.03, 9.92, 9.92)
) |> 
  mutate(
    aug_diag = ifelse(diag_YN == "Non", 2, 1),
    hospi_cout_max = hospi_cout * hospi_tps_jours * aug_diag
  ) |> 
  select(-hospi_cout, -aug_diag)

d_hospi_pec <- tibble(
  financeur = v_financeur, 
  hospi_pec = c(pec_hospi_fam, pec_hospi_secu, pec_hospi_comp, 0)
)

# Hospi Formatage ----
d_hospi <- cbind(d_hospi_cout, d_hospi_fixes) |> 
  mutate(
    hospi_cout_annuel = ifelse(
      diag_YN == "Oui", 
      hospi_tps_jours * hospi_avgcost, 
      hospi_tps_jours * hospi_avgcost * hospi_Rfact
    )
  ) |> 
  cbind(
    pivot_wider(d_hospi_pec, names_from = financeur, values_from = hospi_pec)
  ) |> 
  mutate(
    across(
      .cols = v_financeur, 
      .fns = ~ hospi_cout_annuel * (. / 100), 
      .names = "{.col}_annuel"
    )
  ) |> 
  select(age, diag_YN, ends_with("_annuel")) |> 
  rename_with(
    .cols = str_c(v_financeur, "_annuel"), 
    .fn = ~ str_c("hospi_", .)
  )

d_hospi <- d_hospi |> 
  left_join(
    y = tibble(age = c("65-79", "65-79", "+80"), stade = v_stade), 
    by = "age"
  ) |> 
  mutate(prepost = ifelse(diag_YN == "Oui", "post", "pre")) |>
  relocate(stade, prepost) |>
  filter(!(stade == "Sévère" & prepost == "pre")) |> 
  select(-c(age, diag_YN)) |>
  left_join(y = dur_stade, by = c("prepost", "stade")) |>
  select(diagnostic, stade, prepost, dur_stade, everything()) |>
  arrange(diagnostic, stade) |>
  mutate(
    across(
      .cols = starts_with("hospi_"), 
      .fns = ~ . * dur_stade, 
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    )) |>
  group_by(diagnostic, stade) |>
  summarise_if(.predicate = is.numeric, sum) |> ungroup() |> 
  select(diagnostic, stade, contains("hospi_"))

```

```{r modai_accj}
# ACCJ MAJ ----
# Source: 
# https://www.capretraite.fr/choisir-une-maison-de-retraite/types-de-maison/laccueil-de-jour-faciliter-le-maintien-a-domicile/#:~:text=Le%20tarif%20journalier%20de%20l,le%20Président%20du%20Conseil%20départemental.
d_dur_accj <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 2)), 
  instit = rep(c("Sans", "Avec"), 3), 
  accj_tps = c(6, 3, 4.5, 1.8, 1, 0)
)

d_accj_coutmax <- tibble(
  stade = v_stade, 
  npan = c(0,78,0), 
  cout = c(0,50,0)
) |> 
  mutate(accj_coutmax = npan * cout) |> 
  select(-c(npan, cout))

# Source: 
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/solutions-d-accueil-temporaire/aller-a-l-accueil-de-jour#anchor3
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/aides-financieres/les-aides-en-accueil-de-jour
# d_accj_pec <- tibble(financeur = v_financeur, accj_pec_prop = c(64, 36, 0, 0))
# https://www.pour-les-personnes-agees.gouv.fr/preserver-son-autonomie/perte-d-autonomie-evaluation-et-droits/comment-fonctionne-la-grille-aggir

d_accj_pec <- tibble(financeur = v_financeur, accj_pec_prop = c(64, 36, 0, 0))

d_accj_coutmax <- d_accj_coutmax |> 
  cbind(
    pivot_wider(d_accj_pec, names_from = financeur, values_from = accj_pec_prop)
  ) |> 
  mutate(
    across(
      .cols = v_financeur, 
      .fns = ~ accj_coutmax * (.x / 100)
    ),
    across(
      .cols = is.numeric,
      .fns = ~ ifelse(stade == "Modéré", .x, 0)
    )
  )

# ACCJ Formatage ----
d_accj <- cross_join(d_dur_accj, d_accj_coutmax) |>
  rename(accj_cout_annuel = accj_coutmax) |> 
  mutate(
    accj_cout_patient = accj_cout_annuel * accj_tps, 
  ) |> 
  mutate(
    across(
      .cols = v_financeur,
      .fns = ~ .x, 
      .names = "accj_{.col}_annuel"
    ), 
    across(
      .cols = v_financeur,
      .fns = ~ .x * accj_tps, 
      .names = "accj_{.col}_patient"
    )
  ) |> 
  select(diagnostic, stade, instit, ends_with("_annuel"), ends_with("_patient"))

```

```{r modai_heber}
# Hebergement MAJ ----
# Source: 
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/solutions-d-accueil-temporaire/l-hebergement-temporaire-ce-qu-il-faut-savoir
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/solutions-d-accueil-temporaire/choisir-un-hebergement-temporaire

# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/aides-financieres/les-aides-en-hebergement-temporaire

# https://aides.francealzheimer.org/accompagnement/moins-60-ans/a-domicile-moins-60-ans/dispositifs-daccompagnement-scenario-6-1/soins-et-accompagnement-a-lexterieur-du-domicile-scenario-6-1/lhebergement-temporaire/
# https://www.pour-les-personnes-agees.gouv.fr/vivre-a-domicile/solutions-d-accueil-temporaire/dans-quelles-situations-faire-appel-a-un-hebergement-temporaire
heber_forf_soin_j <- 20 # PEC 100 Assurance Maladie/Sécu

# https://aides.francealzheimer.org/accompagnement/moins-60-ans/a-domicile-moins-60-ans/dispositifs-daccompagnement-scenario-6-1/soins-et-accompagnement-a-lexterieur-du-domicile-scenario-6-1/lhebergement-temporaire/

d_heber_tps <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 4)),
  stade = rep(unlist(map(.x = v_stade[2:3], .f = rep, 2)), 3),
  instit = rep(c("Sans", "Avec"), 6),
  heber_annees = c(
    # precoce
    6, 3, 3, 0,
    # Modere
    5.5, 2.8, 3.5, 0,
    # Severe
    5, 4, 4, 0
  )
) |> 
  mutate(
    heber_jparan = case_when(
      stade == "Modéré" ~ 29,
      stade == "Sévère" ~ 61,
    ), 
    heber_coutparj = case_when(
      stade == "Modéré" ~ 79,
      stade == "Sévère" ~ 85.3,
    ), 
    heber_coutmax = case_when(
      stade == "Modéré" ~ 2290,
      stade == "Sévère" ~ 5201,
    ), 
  )

d_heber_pec <- tibble(financeur = v_financeur, heber_pec_prop = c(70, 30, 0, 0))

d_heber_apa <- tibble(
  gir = 1:4,
  montant = c(2045.56, 1654.18, 1195.67, 797.96)
) |> 
  mutate(
    stade = case_when(
      gir %in% c(3,4) ~ "Léger", 
      gir == 2 ~ "Modéré", 
      gir == 1 ~ "Sévère", 
    )
  ) |> 
  summarise(apa_montant = mean(montant), .by = stade)

# Hebergement Formatage ----
d_heber <- d_heber_tps |> 
  mutate(
    heber_cout_annuel = heber_jparan * heber_coutparj, 
    heber_cout_patient = heber_cout_annuel * heber_annees
  ) |> 
  select(diagnostic, stade, instit, heber_annees, heber_cout_annuel, heber_cout_patient) |> 
  left_join(d_heber_apa, by = "stade") |> 
  cbind(
    pivot_wider(d_heber_pec, names_from = financeur, values_from = heber_pec_prop)
  ) |> 
  mutate(
    across(
      .cols = v_financeur, 
      .fns = ~ heber_cout_annuel * (. / 100), 
      .names = "heber_{.col}_annuel"
    ), 
    across(
      .cols = v_financeur, 
      .fns = ~ heber_cout_patient * (. / 100), 
      .names = "heber_{.col}_patient"
    ),
    heber_sécu_annuel = heber_sécu_annuel + apa_montant,
    heber_famille_annuel = heber_famille_annuel - apa_montant,
    heber_sécu_patient = heber_sécu_patient + (apa_montant * heber_annees),
    heber_famille_patient = heber_famille_patient - (apa_montant * heber_annees),
    across(
      .cols = contains("famille"), 
      .fns = ~ ifelse(.x < 0, 0, .x)
    )
  ) |> 
  select(diagnostic, stade, instit, ends_with("_annuel"), ends_with("_patient"))

d_heber_leg <- tibble(
  diagnostic = unlist(map(v_diag, rep, 2)),
  stade = rep(v_stade[1], 6), 
  instit = rep(c("Sans", "Avec"), 3)
) |> 
  cbind(
    select(
      pivot_wider(
        data = tibble(cols = colnames(d_heber), vals = 0), 
        names_from = cols, 
        values_from = vals
    ), -c(diagnostic, stade, instit))
  )

d_heber <- rbind(d_heber, d_heber_leg) |> arrange(stade, diagnostic)
```

```{r modai_params}
# Parameds MAJ ----

# SSIAD 
# Cout: https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000048092156
# PEC 100%: https://www.service-public.fr/particuliers/vosdroits/F246#:~:text=Quelle%20prise%20en%20charge%20d,de%20soins%20infirmiers%20à%20domicile.
ssiadj <- tibble(
  forfait_inter = 1:9,
  hebdo_euro = c(49.05, 71.29, 100.14, 111.69, 171.6, 98.45, 149.36, 185.85, 268.52)
) |> 
  mutate(journalier = hebdo_euro / 7) |> 
  summarise(ssiad_j = mean(journalier))

# https://www.ag2rlamondiale.fr/sante-prevoyance/mutuelle-sante/conseil-comment-etre-rembourse-des-seances-chez-le-kinesitherapeute#:~:text=la%20complémentaire%20santé%20%3F-,Combien%20coûte%20une%20séance%20de%20kinésithérapie%20%3F,tarif%20peut%20atteindre%2033€.
kinej <- mean(c(16.13, 33))

# https://www.ameli.fr/orthophoniste/exercice-liberal/facturation-remuneration/tarifs-conventionnels/tarifs
orthoj <- 50

d_parameds_fixes <- tibble(
  ssiad = ssiadj[[1]], 
  kine = kinej, 
  ortho = orthoj, 
  esa = 1250
)

d_parameds <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 6)),
  stade = rep(unlist(map(.x = v_stade, .f = rep, 2)), 3),
  instit = rep(c("Sans", "Avec"), 9), 
  parameds_tps_infir = c(2, 2, 6, 3, 3, 0, 0, 0, 4.5, 1.8, 3.5, 0, 0, 0, 1, 0, 4, 0),
  parameds_tps_kine = unlist(map(.x = c(2, 6, 3, 0, 4.5, 3.5, 0, 1, 4), rep, 2)),
  parameds_tps_ortho = unlist(map(.x = c(2, 6, 0, 0, 4.5, 0, 0, 1, 0), rep, 2)),
  parameds_recours_esa = c(0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0)
) |> 
  mutate(
    parameds_inf_npan = case_when(
      stade == "Léger" ~ 365, stade == "Modéré" ~ 547.5, stade == "Sévère" ~ 730 
    ), 
    parameds_kine_npan = case_when(
      stade == "Léger" ~ 50, stade == "Modéré" ~ 50, stade == "Sévère" ~ 100 
    ), 
    parameds_ortho_npan = case_when(
      stade == "Léger" ~ 50, stade == "Modéré" ~ 80, stade == "Sévère" ~ 0 
    ), 
    parameds_esa = case_when(
      stade == "Léger" ~ NA, stade == "Modéré" ~ 13, stade == "Sévère" ~ NA 
    ), 
    parameds_cout_annuel_max = case_when(
      stade == "Léger" ~ 9470, 
      stade == "Modéré" & instit == "Sans" ~ 13880, 
      stade == "Modéré" & instit == "Avec" ~ 4025, 
      stade == "Sévère" & instit == "Sans" ~ 15190,
      stade == "Sévère" & instit == "Avec" ~  2050
    ), 
  )

d_parameds_pec <- tibble(
  financeur = unlist(map(v_financeur, rep, 2)), 
  ald = rep(c("Avec", "Sans"), 4), 
  parameds_inf_pec = c(0, 0, 100, 60, 0, 40, 0, 0), 
  parameds_kine_pec = c(0, 0, 100, 60, 0, 40, 0, 0),
  parameds_ortho_pec = c(0, 0, 100, 60, 0, 40, 0, 0),
  parameds_esa_pec = c(0, 0, 100, 100, 0, 0, 0, 0),
)

# Parameds Formatage ----
d_params <- cbind(d_parameds, d_parameds_fixes) |> 
  rowwise() |> 
  mutate(
    esa = esa / parameds_esa,
    across(
      .cols = contains("esa"), 
      .fns = ~ ifelse(is.na(.), 0, .)
    ),
    parameds_inf_npan = ifelse(parameds_tps_infir == 0, 0, parameds_inf_npan),
    parameds_kine_npan = ifelse(parameds_tps_kine == 0, 0, parameds_kine_npan),
    parameds_ortho_npan = ifelse(parameds_tps_ortho == 0, 0, parameds_ortho_npan),
    kine_cout_annuel = kine * parameds_kine_npan,
    ssiad_cout_annuel = ssiad * parameds_inf_npan,
    ortho_cout_annuel = ortho * parameds_ortho_npan,
    kine_cout_patient = kine * parameds_kine_npan * parameds_tps_kine,
    ssiad_cout_patient = ssiad * parameds_inf_npan * parameds_tps_infir,
    ortho_cout_patient = ortho * parameds_ortho_npan * parameds_tps_ortho,
    parameds_cout_annuel = (kine_cout_annuel + ssiad_cout_annuel + ortho_cout_annuel), 
    parameds_cout_patient = (kine_cout_patient + ssiad_cout_patient + ortho_cout_patient)
  ) 

d_parameds_pec_bis <- d_parameds_pec |> 
  pivot_longer(
    cols = -c(financeur, ald), 
    names_to = "pec", 
    values_to = "prop") |> 
  separate(col = pec, into = c("params", "type", "pec")) |> 
  select(-c(params, pec)) |> 
  unite(col = pec, c(financeur, type)) |> 
  pivot_wider(names_from = pec, values_from = prop)

d_params <- d_params |> 
  cross_join(d_parameds_pec_bis) |> 
  relocate(ald, .after = instit)

d_esa <- d_params |> 
  select(diagnostic, stade, instit, ald, contains("esa")) |> 
  select(-c("parameds_recours_esa", "parameds_esa")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_remove(., "_esa")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_c("esa_", .)) |> 
  rename(esa_cout_patient = esa_esa) |>
  mutate(
    esa_cout_patient = ifelse(esa_cout_patient == 0, 0, 1250), 
    across(
      .cols = contains(v_financeur), 
      .fns = ~ esa_cout_patient * (. / 100), 
      .names = "{.col}_patient"
    )
    ) |> 
  select(-str_c("esa_", v_financeur))

d_iko <- d_params |> 
  select(
    -contains("esa"), -c(ssiad, kine, ortho),
    -contains("tps"), -contains("npan"),
    -contains("parameds")
    ) |> 
  mutate(
    # Infirmier
    across(
      .cols = str_c(v_financeur, "_inf"), 
      .fns = ~ ssiad_cout_annuel * (. / 100), 
      .names = "ssiad_{.col}_annuel"
    ),
    across(
      .cols = str_c(v_financeur, "_inf"), 
      .fns = ~ ssiad_cout_patient * (. / 100), 
      .names = "ssiad_{.col}_patient"
    ),
    # Ortho
    across(
      .cols = str_c(v_financeur, "_ortho"), 
      .fns = ~ ortho_cout_annuel * (. / 100), 
      .names = "ortho_{.col}_annuel"
    ),
    across(
      .cols = str_c(v_financeur, "_ortho"), 
      .fns = ~ ortho_cout_patient * (. / 100), 
      .names = "ortho_{.col}_patient"
    ),
    # Kine
    across(
      .cols = str_c(v_financeur, "_kine"), 
      .fns = ~ kine_cout_annuel * (. / 100), 
      .names = "kine_{.col}_annuel"
    ),
    across(
      .cols = str_c(v_financeur, "_kine"), 
      .fns = ~ kine_cout_patient * (. / 100), 
      .names = "kine_{.col}_patient"
    )
  ) |> 
  select(
    - unlist(map(
      .x = c("inf", "kine", "ortho"), 
      .f = ~ str_c(v_financeur, ., sep = "_")
      ))
    ) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_remove(., "_inf")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_remove(., "_kine")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_remove(., "_ortho")) |> 
  ungroup()


d_kine <- d_iko |>
  select(diagnostic, stade, instit, ald, contains("kine")) |>
  select(diagnostic, stade, instit, ald, ends_with("patient"))

d_inf <- d_iko |>
  select(diagnostic, stade, instit, ald, contains("ssiad")) |>
  select(diagnostic, stade, instit, ald, ends_with("patient")) 

d_ortho <- d_iko |>
  select(diagnostic, stade, instit, ald, contains("ortho")) |>
  select(diagnostic, stade, instit, ald, ends_with("patient"))


d_esa <- d_esa |> 
  rowwise() |>
  mutate(
    across(
      .cols = contains("esa"),
      .fns = ~ ifelse(diagnostic == "Tardif" & stade == "Modéré" & instit == "Avec", 0, .x)
    )
  ) |>
  ungroup()

```

```{r modai_add}
# ADD MAJ ----

# Source: 
# https://solidarites.gouv.fr/reforme-de-loffre-des-services-domicile
add_cout <- 23 + 3 # le plancher 
# Cout horaire: https://www.oxilia.fr/Blog/Quel-est-le-tarif-moyen-pour-une-aide-à-domicile-personne-âgée-en-France-?#:~:text=Les%20aides%20à%20domicile%20permettent,de%2020%20euros%20par%20heure.
# Repas : https://www.capretraite.fr/aide-a-domicile/perte-dautonomie/aide-a-domicile-tarif-et-aides-financieres-pour-les-personnes-agees/
d_add_fixes <- tibble(
  add_avg_cout_horaire = add_cout, # 20,
  add_cout_repas = mean(c(8,15)),
)

d_add <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 6)),
  stade = rep(unlist(map(.x = v_stade, .f = rep, 2)), 3),
  instit = rep(c("Sans", "Avec"), 9), 
  add_tps_annees = c(2, 2, 6, 3, 3, 0, 0, 0, 4.5, 1.8, 3.5, 0, 0, 0, 1, 0, 4, 0)
) |> 
  mutate(
    add_presence_h_mensuel = case_when(
      stade == "Léger" ~ 17, stade == "Modéré" ~ 57, stade == "Sévère" ~ 97, 
    ), 
    add_nrepas_hebdo = case_when(
      stade == "Léger" ~ 0, stade == "Modéré" ~ 7, stade == "Sévère" ~ 14, 
    ), 
    add_presence_h_annuel = add_presence_h_mensuel * 12, 
    add_nrepas_annuel = add_nrepas_hebdo * 52, 
    add_avg_cout_horaire = d_add_fixes$add_avg_cout_horaire,
    add_cout_repas = d_add_fixes$add_cout_repas,
    add_coutmax_annuel = case_when(
      stade == "Léger" ~ 2683, stade == "Modéré" ~ 11907, stade == "Sévère" ~ 21131, 
    ), 
    add_coutmax_annuel_recacl = 
      (add_presence_h_annuel * add_avg_cout_horaire) +
      (add_nrepas_annuel * add_cout_repas)
  )

d_add_pec <- expand.grid(
  apa = c(0, 0.5, 1.0), 
  financeur = str_c("apa_", v_financeur)
) |> mutate(pec = 0) |> 
  pivot_wider(names_from = financeur, values_from = pec)

# ADD Formatage ----
d_adds <- d_add |>
  select(diagnostic, stade, instit, add_tps_annees, 
         add_cout_annuel = add_coutmax_annuel_recacl) |> 
  mutate(add_cout_patient = add_cout_annuel * add_tps_annees)


d_adds <- d_adds |> 
  left_join(
    y = expand.grid(diagnostic = v_diag, stade = v_stade, apa = c(0, 0.5, 1.0)), 
    by = c("diagnostic", "stade")
  ) |> 
  relocate(apa, .after = instit) |> 
  arrange(diagnostic, stade, instit, apa) 

d_adds <- d_adds |> left_join(d_add_pec) |> 
  mutate(
    # Famille
    add_famille_annuel = case_when(
      apa == 0 ~ add_cout_annuel, 
      apa == 0.5 ~ add_cout_annuel / 2,
      apa == 1.0 ~ 0
    ),
    add_famille_patient = case_when(
      apa == 0 ~ add_cout_patient, 
      apa == 0.5 ~ add_cout_patient / 2,
      apa == 1.0 ~ 0
    ),
    # Sécu
    add_conseilgeneraux_annuel = case_when(
      apa == 1.0 ~ add_cout_annuel, 
      apa == 0.5 ~ add_cout_annuel / 2,
      apa == 0 ~ 0
    ),
    add_conseilgeneraux_patient = case_when(
      apa == 1.0 ~ add_cout_patient, 
      apa == 0.5 ~ add_cout_patient / 2,
      apa == 0 ~ 0
    ),
    add_complementaire_annuel = apa_complementaire, 
    add_sécu_annuel = apa_sécu,
    add_complementaire_patient = apa_complementaire, 
    add_sécu_patient = apa_sécu,
  ) |> 
  select(
    diagnostic, stade, instit, apa, starts_with("add_")
  )

```
  
```{r modai_inst}
# Institutionnalisation MAJ ----
# Forfait soins et dépendance
# https://www.fondation-mederic-alzheimer.org/wp-content/uploads/2023/03/2015-septembre-rapport-fma-cout-ma.pdf
inst_soin_j <- 31
inst_dep_j <- 13.10
# forfait hébergement
# https://www.annuaire-retraite.com/ehpad-alzheimer/comprendre-le-cout-dun-accueil-de-jour-alzheimer-tarifs-aides-financieres-et-solutions-pour-alleger-la-facture/
inst_heb_j <- 50

# Multiplication du tarif journalier par 29.5, soit le nombre moyen de jours par mois
jpm <- 29.5

d_instit_fixes <- tibble(
  instit_cout_heberg_mensuel = inst_heb_j * jpm,
  instit_cout_soins_mensuel = inst_soin_j * jpm,
  instit_cout_dependance_mensuel = inst_dep_j * jpm,
  instit_prop_couts_retrancher = 50,
)

d_instit_couts <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 3)),
  stade = rep(v_stade, 3),
  instit_tps_annees = c(0, 3, 3, 0, 2.75, 3.5, 0, 1, 4), 
) |> 
  cbind(d_instit_fixes) |> 
  mutate(
    across(
      .cols = contains("_mensuel"), 
      .fns = ~ (. * 12), 
      .names = "{str_replace(.col, '_mensuel', '_annuel')}"
    ),
    across(
      .cols = contains("heberg"),
      .fns = ~ . / 2, # Retranchement de 50%
      .names = "{.col}_retranche"
    )
  )

d_instit_pec <- tibble(
  financeur = unlist(map(.x = v_financeur, .f = rep, 3)),
  instit_type = rep(c("heberg", "soins", "dependance"), 4),
  conssuivi_pec = c(100, 0, 0, 0, 100, 0, 0, 0, 0, 0, 0, 100)
)

# Institutionnalisation Formatage ----
d_instit <- d_instit_couts |>
  select(
    diagnostic, stade, instit_tps_annees, 
    instit_cout_soins_annuel, 
    instit_cout_dependance_annuel, 
    instit_cout_heberg_annuel = instit_cout_heberg_annuel_retranche
  ) |> 
  mutate(
    instit_cout_annuel = rowSums(across(ends_with("_annuel"))),
    instit_cout_patient = instit_cout_annuel * instit_tps_annees
)

d_instit_pec_bis <- d_instit_pec |> 
  unite(col = pec, c(financeur, instit_type)) |> 
  pivot_wider(names_from = pec, values_from = conssuivi_pec)

d_instit <- d_instit |>
  cbind(d_instit_pec_bis) |>  
  rowwise() |> 
  mutate(
    across(
      .cols = str_c(v_financeur, "_heberg"),
      .fns = ~ instit_cout_heberg_annuel * (. / 100),
      .names = "instit_{.col}_annuel"
    ), 
    across(
      .cols = str_c("instit_", v_financeur, "_heberg_annuel"),
      .fns = ~ .  * instit_tps_annees,
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    ),
    across(
      .cols = str_c(v_financeur, "_soins"),
      .fns = ~ instit_cout_soins_annuel * (. / 100),
      .names = "instit_{.col}_annuel"
    ),
    across(
      .cols = str_c("instit_", v_financeur, "_soins_annuel"),
      .fns = ~ .  * instit_tps_annees,
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    ),
    across(
      .cols = str_c(v_financeur, "_dependance"),
      .fns = ~ instit_cout_dependance_annuel * (. / 100),
      .names = "instit_{.col}_annuel"
    ),
    across(
      .cols = str_c("instit_", v_financeur, "_dependance_annuel"),
      .fns = ~ .  * instit_tps_annees,
      .names = "{str_replace(.col, '_annuel', '_patient')}"
    )
  ) |> 
  ungroup() |> 
  select(-unlist(
    map(
      .x = c("soins", "heberg", "dependance"), 
      .f = ~ str_c(v_financeur, ., sep = "_")
    )
  )
  )

d_inssoin <- d_instit |> select(diagnostic, stade, contains("soins")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "instit_")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "_soins")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_c("institsoin_", .)) |> 
  mutate(instit = "Avec", 
         institsoin_cout_patient = rowSums(across(contains("_patient")))) |> 
  relocate(instit, institsoin_cout_patient, .after = stade)

d_insheb <- d_instit |> select(diagnostic, stade, contains("heberg")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "instit_")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "_heberg")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_c("institheber_", .)) |> 
  mutate(instit = "Avec", 
         institheber_cout_patient = rowSums(across(contains("_patient")))) |> 
  relocate(instit, institheber_cout_patient, .after = stade)

d_insdep <- d_instit |> select(diagnostic, stade, contains("dependance")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "instit_")) |> 
  rename_with(.cols = everything(), .fn = ~ str_remove(., "_dependance")) |> 
  rename_with(.cols = is.numeric, .fn = ~ str_c("institdep_", .)) |> 
  mutate(instit = "Avec", 
         institdep_cout_patient = rowSums(across(contains("_patient")))) |> 
  relocate(instit, institdep_cout_patient, .after = stade)

```

```{r modai_acc}
# Accidents MAJ ----
d_accidents_fixes <- tibble(
  accidents_n_annuel = 1, 
  accidents_avgcout = 1000
)

d_accidents_couts <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 3)),
  stade = rep(v_stade, 3),
  instit_tps_annees = c(3, 0, 0, 5, 1, 0, 5, 4, 0),
) |> 
  cbind(d_accidents_fixes) |> 
  mutate(
    accidents_cout_annuel = accidents_n_annuel * accidents_avgcout
  )


d_accidents_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(100, 0, 0, 0)
)

# Accidents Formatage ----
d_accgest <- d_accidents_couts |>
  select(diagnostic, stade, accidents_tps_annees = instit_tps_annees, 
         accidents_cout_annuel) |> 
  mutate(accidents_cout_patient = accidents_cout_annuel * accidents_tps_annees) |> 
  cross_join(
    pivot_wider(
      d_accidents_pec, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "accidents_"
    )
  )|> 
  mutate(
    across(
      .cols = str_c("accidents_", v_financeur), 
      .fns = ~ accidents_cout_annuel * (./100), 
      .names = "{.col}_annuel"
    ),
    across(
      .cols = str_c("accidents_", v_financeur), 
      .fns = ~ accidents_cout_patient * (./100), 
      .names = "{.col}_patient"
    )
  ) |> 
  select(
    -str_c("accidents_", v_financeur)
  )
```

```{r modai_accidents_conduite}
# Accident de Conduite MAJ ----
d_accidents_conduite_couts <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 2)),
  stade = rep(v_stade[1:2], 3),
  accidents_conduite_tps_annees = c(5, 0, 5, 1, 5, 4),
  accidents_conduite_avg_cout = 10080
) |> 
  mutate(
    accidents_conduite_risquema = ifelse(stade == "Léger", 3.13, 7),
    accidents_conduite_risque_acc = ifelse(stade == "Léger", 0.025, 0.044),
    accidents_conduite_prop_permis = ifelse(stade == "Léger", 79, 62)
  )

d_accidents_conduite_pec_2014 <- tibble(
  financeur = v_financeur,
  accidents_pec = c(50, 0, 50, 0)
)

# Source:
# cout accident: https://www.onisr.securite-routiere.gouv.fr/sites/default/files/2022-09/017%20Le%20coût%20de%20l%20insécurité%20routière_V5.pdf
v_maleg_risk_acc <- 2
v_mamod_risk_acc <- 7.9
v_acc_medcost <- 17875
v_acc_matcost <- 5482

n_accidents_corpo <- 185804
n_accidents_mat <- 1816807
prop_accidents_corpo <- n_accidents_mat/ n_accidents_corpo
pc_acc_corpo <- prop_accidents_corpo / 100


d_accidents_conduite_couts <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 2)),
  stade = rep(v_stade[1:2], 3),
  accidents_conduite_tps_annees = c(5, 0, 5, 1, 5, 4),
  accidents_conduite_avg_medical_cout = v_acc_medcost, 
  accidents_conduite_avg_materiel_cout = v_acc_matcost
) |> 
  mutate(
    accidents_conduite_risquema = ifelse(stade == "Léger", v_maleg_risk_acc, v_mamod_risk_acc),
    accidents_conduite_risque_acc = ifelse(stade == "Léger", 0.025, 0.044),
    accidents_conduite_prop_permis = ifelse(stade == "Léger", 79, 62)
  )

d_accidents_conduite_materiel_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(50, 0, 50, 0)
)

d_accidents_conduite_medical_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(0, 100, 0, 0)
)

# Accidents de Conduite Formatage ----
d_accroute <- d_accidents_conduite_couts |> 
  mutate(
    accidentsroute_cout_medical_annuel = 
      accidents_conduite_avg_medical_cout * accidents_conduite_risquema * 
      accidents_conduite_risque_acc * (accidents_conduite_prop_permis / 100),
    accidentsroute_cout_medical_patient = 
      accidentsroute_cout_medical_annuel * accidents_conduite_tps_annees,
    
    accidentsroute_cout_materiel_annuel = 
      accidents_conduite_avg_materiel_cout * accidents_conduite_risquema * 
      accidents_conduite_risque_acc * (accidents_conduite_prop_permis / 100),
    accidentsroute_cout_materiel_patient = 
      accidentsroute_cout_materiel_annuel * accidents_conduite_tps_annees
  ) 

d_accroute_medical <- d_accroute |> 
  select(
    diagnostic, stade,accidents_conduite_tps_annees, 
    accidentsroute_cout_medical_annuel, accidentsroute_cout_medical_patient
    ) |> 
  cross_join(
    pivot_wider(
      d_accidents_conduite_medical_pec, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "accidentsroute_medical_"
    )
  ) |> 
  mutate(
    across(
      .cols = str_c("accidentsroute_medical_", v_financeur), 
      .fns = ~ accidentsroute_cout_medical_annuel * (./100), 
      .names = "{.col}_annuel"
    ),
    across(
      .cols = str_c("accidentsroute_medical_", v_financeur), 
      .fns = ~ accidentsroute_cout_medical_patient * (./100), 
      .names = "{.col}_patient"
    )
  ) |> 
  select(
    -str_c("accidentsroute_medical_", v_financeur)
  )

d_accroute_medical <- d_accroute_medical |> 
  mutate(
    across(
      .cols = ends_with("_annuel"), 
      .fns = ~ . * pc_acc_corpo
    ),
    across(
      .cols = ends_with("_patient"), 
      .fns = ~ . * pc_acc_corpo
    )
  )
d_accroute_materiel <- d_accroute |> 
  select(
    diagnostic, stade,accidents_conduite_tps_annees, 
    accidentsroute_cout_materiel_annuel, accidentsroute_cout_materiel_patient
    ) |> 
  cross_join(
    pivot_wider(
      d_accidents_conduite_materiel_pec, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "accidentsroute_materiel_"
    )
  ) |> 
  mutate(
    across(
      .cols = str_c("accidentsroute_materiel_", v_financeur), 
      .fns = ~ accidentsroute_cout_materiel_annuel * (./100), 
      .names = "{.col}_annuel"
    ),
    across(
      .cols = str_c("accidentsroute_materiel_", v_financeur), 
      .fns = ~ accidentsroute_cout_materiel_patient * (./100), 
      .names = "{.col}_patient"
    )
  ) |> 
  select(
    -str_c("accidentsroute_materiel_", v_financeur)
  )


d_accroute <- left_join(
  x = d_accroute_medical, 
  y = d_accroute_materiel, 
  by = c("diagnostic", "stade", "accidents_conduite_tps_annees")
) |> 
  mutate(
    accidentsroute_cout_annuel = accidentsroute_cout_medical_annuel + accidentsroute_cout_materiel_annuel,
    accidentsroute_famille_annuel = accidentsroute_medical_famille_annuel + accidentsroute_materiel_famille_annuel,
    accidentsroute_sécu_annuel = accidentsroute_medical_sécu_annuel + accidentsroute_materiel_sécu_annuel,
    accidentsroute_complementaire_annuel = accidentsroute_medical_complementaire_annuel + accidentsroute_materiel_complementaire_annuel,
    accidentsroute_conseilgeneraux_annuel = accidentsroute_medical_conseilgeneraux_annuel + accidentsroute_materiel_conseilgeneraux_annuel,
    
    accidentsroute_cout_patient = accidentsroute_cout_medical_patient + accidentsroute_cout_materiel_patient,
    accidentsroute_famille_patient = accidentsroute_medical_famille_patient + accidentsroute_materiel_famille_patient,
    accidentsroute_sécu_patient = accidentsroute_medical_sécu_patient + accidentsroute_materiel_sécu_patient,
    accidentsroute_complementaire_patient = accidentsroute_medical_complementaire_patient + accidentsroute_materiel_complementaire_patient,
    accidentsroute_conseilgeneraux_patient = accidentsroute_medical_conseilgeneraux_patient + accidentsroute_materiel_conseilgeneraux_patient
  ) |> 
  select(
    -contains("medical"), 
    -contains("materiel")
  )

```

```{r modai_tpsaid}
# Temps Aidant MAJ ----
# Sources: 
# https://www.francealzheimer.org/le-quotidien-complique-des-aidants-en-activite-professionnelle/

# Taux horaire: https://www.insee.fr/fr/statistiques/8382421#:~:text=socioprofessionnelle%20en%202022-,Figure%201%20–%20Salaire%20horaire%20brut%20régulier%20moyen%20selon%20la%20catégorie,%2C6%20euros%20par%20heure).
v_update_taux_horaire_2022 <- 19.9

d_tpsaidant_fixes <- tibble(
  tpsaidant_nsem = 52, 
  tpsaidant_prop_actpro = 50, 
  tpsaidant_prop_aidants_concernés = 33,
  tpsaidant_tauxhoraire = v_update_taux_horaire_2022
)

# Chandler et al. 2024
v_tps_aid_leg_j <- 2.3
v_tps_aid_mod_j <- mean(c(3.8, 6.2))
v_tps_aid_sev_j <- 8.8

d_tpsaidant_cout <- tibble(
  diagnostic = unlist(map(.x = v_diag, .f = rep, 6)),
  stade = rep(unlist(map(.x = v_stade, .f = rep, 2)), 3),
  instit = rep(c("Sans", "Avec"), 9), 
  tpsaidant_tps_annees_nondiag = c(3, 3, 0, 0, 0, 0, 5, 5, 1, 1, 0, 0, 5, 5, 4, 4, 0, 0),
  tpsaidant_tps_annees_diag = c(2, 2, 6, 3, 3, 0, 0, 0, 4.5, 1.8, 3.5, 0, 0, 0, 1, 0, 4, 0)
) |> 
  cbind(d_tpsaidant_fixes) |> 
  mutate(
    tpsaidant_tps_hebdo_sansttt = case_when(
      stade == "Léger" ~ v_tps_aid_leg_j * 7, 
      stade == "Modéré" ~ v_tps_aid_mod_j * 7, 
      stade == "Sévère" ~ v_tps_aid_sev_j * 7,
    ), 
    tpsaidant_redacttt = case_when(
      stade == "Léger" ~ 3.7, 
      stade == "Modéré" ~ 6.2, 
      stade == "Sévère" ~ 17.5,
    ), 
    tpsaidant_cout_annuel_nondiag = 
      tpsaidant_tauxhoraire * tpsaidant_tps_hebdo_sansttt *
      tpsaidant_nsem * 
      (tpsaidant_prop_actpro / 100) * (tpsaidant_prop_aidants_concernés / 100),
    tpsaidant_cout_annuel_diag = 
      tpsaidant_tauxhoraire * (tpsaidant_tps_hebdo_sansttt - tpsaidant_redacttt) * 
      tpsaidant_nsem * 
      (tpsaidant_prop_actpro / 100) * (tpsaidant_prop_aidants_concernés / 100), 
    tpsaidant_cout_annuel_diag_max = case_when(
      stade == "Léger" ~ 1403,
      stade == "Modéré" ~ 4603, 
      stade == "Sévère" ~ 6325,
    ), 
    tpsaidant_cout_annuel_nondiag_max = case_when(
      stade == "Léger" ~ 1850,
      stade == "Modéré" ~ 5345, 
      stade == "Sévère" ~ 0,
    )
  )

d_tpsaidant_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(100, 0, 0, 0)
)

# Temps Aidant Formatage ----
d_tpsaidant <- d_tpsaidant_cout |>
  select(
    diagnostic, stade, instit, 
    tpsaidant_tps_annees_nondiag, tpsaidant_tps_annees_diag, 
    tpsaidant_cout_nondiag_annuel = tpsaidant_cout_annuel_nondiag, 
    tpsaidant_cout_diag_annuel = tpsaidant_cout_annuel_diag
    ) |> 
  mutate(
    tpsaidant_cout_nondiag_patient = tpsaidant_cout_nondiag_annuel * tpsaidant_tps_annees_nondiag,
    tpsaidant_cout_diag_patient = tpsaidant_cout_diag_annuel * tpsaidant_tps_annees_diag
  ) |> 
  cross_join(
    pivot_wider(
      d_tpsaidant_pec, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "tpsaidant_"
    )
  ) |> 
  mutate(
    across(
      .cols = str_c("tpsaidant_", v_financeur), 
      .fns = ~ tpsaidant_cout_nondiag_annuel * (./100), 
      .names = "{.col}_nondiag_annuel"
    ),
    across(
      .cols = str_c("tpsaidant_", v_financeur), 
      .fns = ~ tpsaidant_cout_diag_annuel * (./100), 
      .names = "{.col}_diag_annuel"
    ),
    across(
      .cols = str_c("tpsaidant_", v_financeur), 
      .fns = ~ tpsaidant_cout_nondiag_patient * (./100), 
      .names = "{.col}_nondiag_patient"
    ), 
    across(
      .cols = str_c("tpsaidant_", v_financeur), 
      .fns = ~ tpsaidant_cout_diag_patient * (./100), 
      .names = "{.col}_diag_patient"
    )
  ) |> 
  select(
    -str_c("tpsaidant_", v_financeur)
  ) 


d_tpsaidant <- d_tpsaidant |> 
  select(-contains("tpsaidant_tps_annees")) |> 
  pivot_longer(
    cols = -c(diagnostic, stade, instit), 
    names_to = "var", 
    values_to = "val"
  ) |> 
  separate(
    col = var,
    into = c("tpsaidant", "fin", "diag", "time"),
    sep = "_"
  ) |> 
  group_by(diagnostic, stade, instit, tpsaidant, fin, time) |> 
  summarise(val = sum(val)) |> 
  unite(c("tpsaidant", "fin", "time"), col = "var", sep = "_") |> 
  pivot_wider(names_from = var, values_from = val)
```
  
```{r modai_santaid}
# Santé Aidant 2014 ----
d_santeaidant_fixe_2014 <- tibble(
  santeaidant_consultcout = 23,
  santeaidant_comprimescout = 15.06,
  santeaidant_bcomprimes = 28,
  santeaidant_nsem = 52,
  santeaidant_maladieslatentes_cout = 550
)

d_santeaidant_cout_2014 <- tibble(
  stade = unlist(map(.x = v_stade, .f = rep, 2)),
  santeaidant_etat = rep(c("Bon", "Mauvais"), 3), 
  santeaidant_coutmax = c(213, 1114, 213, 1114, 213, NA)
) |> 
  cbind(d_santeaidant_fixe_2014) |> 
  mutate(
    santeaidant_nconsult_moral = ifelse(santeaidant_etat == "Mauvais", 10, 5),
    santeaidant_nconsult_physique = ifelse(santeaidant_etat == "Mauvais", 6, NA),
    santeaidant_ncomprimes_hebdo = ifelse(santeaidant_etat == "Mauvais", 7, 3.5),
    santeaidant_cout_annuel = 
      ifelse(
        santeaidant_etat == "Mauvais", 
        santeaidant_consultcout * 
          (santeaidant_nconsult_moral + santeaidant_nconsult_physique) +
          santeaidant_ncomprimes_hebdo *
          (santeaidant_comprimescout / santeaidant_bcomprimes) *
          santeaidant_nsem +
          santeaidant_maladieslatentes_cout,
        santeaidant_consultcout * 
          santeaidant_nconsult_moral +
          santeaidant_ncomprimes_hebdo *
          (santeaidant_comprimescout / santeaidant_bcomprimes) *
          santeaidant_nsem
      ),
    santeaidant_cout_annuel_max = case_when(
        santeaidant_etat == "Bon" ~ 213,
        santeaidant_etat == "Mauvais" & stade == "Sévère" ~ NA,
        TRUE ~ 1114
      )
  ) 

d_santeaidant_pec_2014 <- tibble(
  financeur = v_financeur,
  accidents_pec = c(0, 70, 30, 0)
)

d_santeaidant_2014 <- d_santeaidant_cout_2014 |> 
  select(stade, santeaidant_etat, santeaidant_cout_annuel) |> 
  cross_join(
    pivot_wider(
      d_santeaidant_pec_2014, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "santeaidant_"
    )
  )|> 
  mutate(
    across(
      .cols = str_c("santeaidant_", v_financeur), 
      .fns = ~ santeaidant_cout_annuel * (./100), 
      .names = "{.col}_annuel"
    )
  ) |> 
  select(
    -str_c("santeaidant_", v_financeur)
  ) 
# Santé Aidant MAJ ----
# Source: 
# https://drees.solidarites-sante.gouv.fr/publications-communique-de-presse/les-dossiers-de-la-drees/241002_DD_perte_autonomie
# Consultation cout 30€ : https://www.service-public.fr/particuliers/actualites/A17435#:~:text=Médecin%20généraliste%2C%20pédiatre%2C%20psychiatre…%20%3A%20vos%20consultations%20vont%20bientôt%20augmenter,-Publié%20le%2002&text=À%20partir%20du%2022%20décembre,et%20la%20plupart%20des%20spécialistes.
# SEROPRAM: SEROPRAM 20 mg : comprimé sécable (blanc) ; boîte de 28 Ordonnance obligatoire (Liste I) - Remboursable à 65% - Prix : 5.54 €. Source:  https://www.vidal.fr/medicaments/gammes/seropram-9308.html
d_santeaidant_fixe <- tibble(
  santeaidant_consultcout = 30, 
  santeaidant_comprimescout = 5.54,
  santeaidant_bcomprimes = 28,
  santeaidant_nsem = 52,
  santeaidant_maladieslatentes_cout = 550
)

d_santeaidant_cout <- tibble(
  stade = unlist(map(.x = v_stade, .f = rep, 2)),
  santeaidant_etat = rep(c("Bon", "Mauvais"), 3), 
  santeaidant_coutmax = c(213, 1114, 213, 1114, 213, NA)
) |> 
  cbind(d_santeaidant_fixe) |> 
  mutate(
    santeaidant_nconsult_moral = ifelse(santeaidant_etat == "Mauvais", 10, 5),
    santeaidant_nconsult_physique = ifelse(santeaidant_etat == "Mauvais", 6, NA),
    santeaidant_ncomprimes_hebdo = ifelse(santeaidant_etat == "Mauvais", 7, 3.5),
    santeaidant_cout_annuel = 
      ifelse(
        santeaidant_etat == "Mauvais", 
        santeaidant_consultcout * 
          (santeaidant_nconsult_moral + santeaidant_nconsult_physique) +
          santeaidant_ncomprimes_hebdo *
          (santeaidant_comprimescout / santeaidant_bcomprimes) *
          santeaidant_nsem +
          santeaidant_maladieslatentes_cout,
        santeaidant_consultcout * 
          santeaidant_nconsult_moral +
          santeaidant_ncomprimes_hebdo *
          (santeaidant_comprimescout / santeaidant_bcomprimes) *
          santeaidant_nsem
      ),
    santeaidant_cout_annuel_max = case_when(
        santeaidant_etat == "Bon" ~ 213,
        santeaidant_etat == "Mauvais" & stade == "Sévère" ~ NA,
        TRUE ~ 1114
      )
  ) 

d_santeaidant_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(0, 65, 35, 0)
)

# Santé Aidant Formatage ----
d_santeaidant <- d_santeaidant_cout |> 
  select(stade, santeaidant_etat, santeaidant_cout_annuel) |> 
  cross_join(
    pivot_wider(
      d_santeaidant_pec, 
      names_from = financeur, 
      values_from = accidents_pec, 
      names_prefix = "santeaidant_"
    )
  )|> 
  mutate(
    across(
      .cols = str_c("santeaidant_", v_financeur), 
      .fns = ~ santeaidant_cout_annuel * (./100), 
      .names = "{.col}_annuel"
    )
  ) |> 
  select(
    -str_c("santeaidant_", v_financeur)
  ) #|> rename_with(.wcols = contains("psycho_"), .fn = ~ str_c(., "_annuel"))

```

```{r modai_amen}
# Aménagements MAJ ----
# Source: 
# https://aides.francealzheimer.org/accompagnement/moins-60-ans/a-domicile-moins-60-ans/dispositifs-daccompagnement-scenario-6-1/divers-scenario-6-1/lamenagement-du-domicile/
# https://aides.francealzheimer.org/wp-content/uploads/2016/07/Les-aménagements-à-la-maison.pdf
# ANAH: :https://france-renov.gouv.fr/aides/maprimeadapt
# cout : https://www.capretraite.fr/blog/maintien-a-domicile/les-couts-caches-du-maintien-a-domicile-ce-que-vous-devez-savoir/

d_amenagement_fixe <- tibble(amenagement_cout = 1667)

d_amenagement_pec <- tibble(
  financeur = v_financeur,
  accidents_pec = c(100, 0, 0, 0)
)

d_anah <- expand.grid(
  categorie = c("Bleu", "Jaune", "Violet", "Rose"), 
  N_pers_ds_foyer = 1:5,
  idf_YN = c(TRUE, FALSE)
) |> 
  mutate(
    revenus = case_when(
      categorie == "Bleu" ~ "Très modeste", 
      categorie == "Jaune" ~ "Modeste", 
      categorie == "Violet" ~ "Intermédiaire", 
      categorie == "Rose" ~ "Supérieurs", 
    ),
    anah = c(
      23768, 28933, 40404, 40405,
      34884, 42463, 59394, 59395, 
      41893, 51000, 71060, 71061, 
      48914, 59549, 83637, 83638, 
      55961, 68123, 95758, 95759, 
      17173, 22015, 30844, 30845, 
      25115, 32197, 45340, 45341, 
      30206, 38719, 54592, 54593, 
      35285, 45234, 63844, 63845, 
      40388, 51775, 73098, 73099
    )
  ) 

d_anah_sum <- d_anah |> summarise(anah = mean(anah))

d_amenagement_fixe <- tibble(amenagement_cout = 1667)

d_amenagement_pec <- tibble(
  financeur = v_financeur,
  amenagement_pec = c(25, 0, 25, 50)
)

# Aménagements Formatage ----

d_amenagement <- tibble(stade = v_stade, d_amenagement_fixe) |> 
  cross_join(
    pivot_wider(
      d_amenagement_pec, 
      names_from = financeur, 
      values_from = amenagement_pec, 
      names_prefix = "amenagement_"
    )
  )|> 
  mutate(
    across(
      .cols = str_c("amenagement_", v_financeur), 
      .fns = ~ amenagement_cout * (./100), 
      .names = "{.col}_annuel"
    )
  ) |> 
  select(-str_c("amenagement_", v_financeur)) 

```

```{r d_all}
d_all <- d_conssuivi |> 
  left_join(y = d_medsma, by = c("diagnostic", "stade", "instit", "duree_ma")) |> 
  left_join(y = d_psycho, by = c("diagnostic", "stade", "instit", "ald", "duree_ma")) |> 
  left_join(y = d_kine, by = c("diagnostic", "stade", "instit", "ald")) |> 
  left_join(y = d_inf, by = c("diagnostic", "stade", "instit", "ald")) |> 
  left_join(y = d_ortho, by = c("diagnostic", "stade", "instit", "ald")) |> 
  left_join(y = d_esa, by = c("diagnostic", "stade", "instit", "ald")) |> 
  left_join(y = d_accj, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_heber, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_adds, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_tpsaidant, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_apa, by = c("diagnostic", "stade", "instit")) |>
  left_join(y = d_inssoin, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_insheb, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_insdep, by = c("diagnostic", "stade", "instit")) |> 
  left_join(y = d_accgest, by = c("diagnostic", "stade")) |> 
  left_join(y = d_accroute, by = c("diagnostic", "stade")) |> 
  left_join(y = d_hospi, by = c("diagnostic", "stade")) |>
  left_join(y = d_consult_diag, by = c("diagnostic", "stade")) |> 
  left_join(y = d_hdj, by = "stade") |> 
  left_join(y = d_amenagement, by = "stade") |> 
  left_join(y = d_santeaidant, by = "stade")
  
d_all <- d_all |> 
  mutate(
    diagnostic = as.factor(diagnostic),
    diagnostic = relevel(diagnostic, ref = "Précoce"), 
    stade = as.factor(stade),
    stade = relevel(stade, ref = "Léger")
  )

d_all <- d_all |> 
  left_join(
    y = select(d_sit,
               situation, instit, ald, apa,
               sit_prop = prop, 
               sit_prop_adj = pc_adj
    ), 
    by = c("instit", "ald", "apa")
  )

d_all <- d_all |> ungroup()
```

\newpage

## Individual Model

```{r l_ps_all}
l_ps_all <- pmap(
  .l = list(
    aid = d_combinaison$santeaidant_etat,
    inst = d_combinaison$instit,
    ald = d_combinaison$ald,
    apa = d_combinaison$apa,
    sit = d_combinaison$situation,
    pat = d_combinaison$patient),
  .f = function(aid, inst, ald, apa, sit, pat){
    f_ps(
      df = d_all,
      var_aid = aid,
      var_inst = inst,
      var_ald = ald,
      var_apa = apa,
      var_sit = sit,
      var_pat = pat
    ) 
  }
) |> set_names(nm = d_combinaison$combinaison)

```

## Global Model

```{r modb}
# Modèle Global: Patients Types Global ----
t_pg <- map2(
  .x = unlist(map(.x = 1:9, .f = ~ filter(d_combinaison, patient == .x)$combinaison)),
  .y = str_c("d", d_combinaison$patient),
  .f = function(x, y){
    l_ps_all[x][[1]][y][[1]] |>
      left_join(
        y = select(d_combinaison, situation, patient, freq), 
        by = c("situation", "patient")
      ) |> 
      mutate(
        across(
          .cols = c("cout", v_financeur), 
          .fns = ~ . * freq
        )
      ) |> 
      select(-freq)
  }
) |> 
  bind_rows() |> 
  group_by(diagnostic, stade, patient, type) |> 
  summarise_if(
    .predicate = is.numeric, 
    .funs = ~ sum(.x, na.rm = TRUE)
  ) |> 
  left_join(tibble(type = v_tbl_order, ord = 1:length(v_tbl_order)), "type") |> 
  arrange(diagnostic, stade, patient, ord) |> 
  select(-ord) |> ungroup()


# Modèle Annuel ----
t_pg_an <- t_pg |> mutate(across(is.numeric, ~./14))
```

```{r t_pg_glob_ans}
t_pg_an_fn <- t_pg_an |> 
      left_join(y = select(d_profils_freq, patient, freq), by = "patient") |> 
      mutate(
        ma_prop = ma_prop_2014,
        across(.cols = c("cout", v_financeur), .fns = ~ .x * freq * ma_prop)
      ) |> 
      select(-c(freq, ma_prop))

t_pg_an_fn_diag <- t_pg_an_fn |>
      group_by(diagnostic, type) |> 
      summarise_if(
        .predicate = is.numeric, 
        .funs = ~ sum(., na.rm = TRUE)
      )

t_pg_an_fn_diag_tot <- t_pg_an_fn_diag |> 
      group_by(diagnostic) |> 
      summarise_if(.predicate = is.numeric, .funs = sum, is.na = TRUE) |> 
      janitor::adorn_totals(where = "row") |> as_tibble()

t_pg_an_fn_diag_coutgp <- t_pg_an_fn_diag |> 
      mutate(
        couts_gps = case_when(
          type %in% c("consdiag", "suivi", "medsma", "psycho", "hospi", "hdj") ~ "Coûts Médicaux", 
          type %in% c("accj", "heber", "ssiad", "kine", "ortho", "esa", "add") ~ "Coûts Médicaux-Sociaux Domicile", 
          type %in% c("institdep", "institheber",	"institsoin") ~ "Coûts Médicaux-Sociaux Institution", 
          type %in% c("accidents", "accidentsroute") ~ "Accidents", 
          type %in% c("santeaidant", "tpsaidant", "amenagement") ~ "Coûts Liés aux Aidants", 
        )
      ) |> 
      group_by(diagnostic, couts_gps) |> 
      summarise_if(.predicate = is.numeric, .funs = ~ sum(.x, na.rm = TRUE))

t_pg_ans <- list(
  t_pg_an_fn = t_pg_an_fn,
  t_pg_an_fn_diag = t_pg_an_fn_diag,
  t_pg_an_fn_diag_tot = t_pg_an_fn_diag_tot,
  t_pg_an_fn_diag_coutgp = t_pg_an_fn_diag_coutgp
)
```

```{r}
t1 <- t_pg_ans$t_pg_an_fn_diag_coutgp |>
      select(-cout) |> 
      mutate(
        across(.cols = is.numeric, .fns = ~ round(. / 10e8, 3)), 
        cout = rowSums(across(is.numeric))
      )

t2 <- t_pg_ans$t_pg_an_fn_diag_tot |>
  select(-cout) |> 
  mutate(
    across(.cols = is.numeric, .fns = ~ round(. / 10e8, 3)), 
    cout = rowSums(across(is.numeric))
  )
t3 <- t_pg_ans$t_pg_an_fn_diag |>
  select(-cout) |> 
  mutate(
    across(.cols = is.numeric, .fns = ~ round(. / 10e8, 3)), 
    cout = rowSums(across(is.numeric))
  ) |> 
  pivot_wider(
    names_from = diagnostic, 
    values_from = c("cout", v_financeur),
    names_glue = c("{diagnostic}_{.value}")
  ) |> 
  select(type, contains("Précoce"), contains("Moyen"), contains("Tardif")) |>
  janitor::adorn_totals(where = "row") |> as_tibble() %>%
  `colnames<-`(str_remove_all(
    colnames(.), 
    "Précoce_|Moyen_|Tardif_"
  )) |> 
  kable(caption = glue::glue("Coûts Globaux, Détails (en Mds)"), 
        format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx) |> 
  add_header_above(c(" " = 1, "Précoce" = 5, "Moyen" = 5, "Tardif" = 5)) 

p1 <- f_1425_postcouts(annee = t_pg_ans, type = "dets")
p2 <- f_1425_postcouts(annee = t_pg_ans, type = "gps")
p3 <- f_1425_stadescouts(annee = t_pg_ans, type = "gps")
    
l_summary <-list(
      t_postes_depenses = t1,
      t_depenses_globales = t2,
      t_depenses_details = t3, 
      p_stades_financeurs = p1, 
      p_stades_type_depenses = p2, 
      p_stades_prop_depenses = p3
) 

```

### Yearly Cost

```{r l_summary}
l_summary$t_depenses_globales |> 
  kable(caption = "Coûts Globaux (en Mds)",
    format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)
l_summary$t_postes_depenses |> 
  kable(caption = "Répartition des dépenses par Type de Coûts (en Mds)",
    format = "latex", booktabs = TRUE) |> 
  kable_styling(latex_options = ltx)
l_summary$t_depenses_details
```

```{r}
l_summary$p_stades_financeurs
```

```{r}
l_summary$p_stades_type_depenses
```

```{r}
l_summary$p_stades_prop_depenses
```



