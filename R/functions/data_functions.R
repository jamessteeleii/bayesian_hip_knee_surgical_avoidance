read_prep_pilot_data <- function(file) {
  
  data <- read_csv(file) |>
    mutate(event = if_else(surgery == 1, "event", "no_event")) |> 
    group_by(time, joint) |> 
    count(event, .drop = FALSE) |> 
    filter(!is.na(joint)) |>
    ungroup() |>
    pivot_wider(names_from = event,
                values_from = n) |> 
    mutate(event = case_when(
      is.na(event) ~ 0,
      .default = event
    )) |>
    mutate(total         = event + no_event,
           time_period = factor(time)) |>
    group_by(joint) |> 
    mutate(
      study = "Pilot Data",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    ungroup() |>
    add_row(time_period = as.factor(0),
            study = "Pilot Data",
            joint = "Hip",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0
    ) |>
    add_row(time_period = as.factor(0),
            study = "Pilot Data",
            joint = "Knee",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0
    ) |>
    select(time_period, event, no_event, total, study, joint, surv, surv_se, condition, design)
  
  return(data)
  
}

prepare_prior_data <- function() {
  
  # Skou et al. (2020) doi: 10.1136/bmjopen-2019-033495
  # Follow up at 12 and 24 months for GLA:D programme
  # Knee OA, baseline pain for group 49.5, KOOS 48.5
  skou_data <- tibble(
    time_period = as.factor(c(12,24)),
    event = c(13,3),
    no_event = c(37,34),
    total = event + no_event
  ) |>
    mutate(
      study = "Skou et al. (2020)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Skou et al. (2020)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0
    )
  
  # Gwynne-Jones et al., (2018) doi: 10.1016/j.arth.2018.04.011
  # Follow up at 12 months for Joint Clinic non-surgical management
  # Hip OA (Knee OA reported at 5 years in full below), baseline OHS 22.1
  gwynne_jones_data_hip <- tibble(
    time_period = as.factor(12),
    event = 52,
    no_event = 45,
    total = event + no_event
  ) |>
    mutate(
      study = "Gwynne-Jones et al. (2018)",
      joint = "Hip",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Gwynne-Jones et al. (2018)",
            joint = "Hip",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0
    )
  
  # Gwynne-Jones et al., (2020) doi: 10.1016/j.arth.2020.04.087
  # Follow up at mean 5.5 years for Joint Clinic non-surgical management
  # Knee OA, baseline OKS 20.3
  gwynne_jones_knee_data <- tibble(
    time_period = as.factor(66),
    event = 42,
    no_event = 70,
    total = event + no_event
  ) |>
    mutate(
      study = "Gwynne-Jones et al. (2020)",
      joint = "Knee",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Gwynne-Jones et al. (2020)",
            joint = "Knee",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0
    )
  
  gwynne_jones_data <- bind_rows(gwynne_jones_knee_data, gwynne_jones_data_hip)

  # Svege et al., (2015) doi: 10.1136/annrheumdis-2013-203628
  # Follow-up over 6 years for exercise plus education, versus education
  # Hip OA, WOMAC pain 26 (exercise) and 27.3 (education only)
  # Digitised Kaplan-Meier curves and then grouped into discrete time by year
  
  svege_exercise_time <- tibble(
    time = c(4.594833262,
             4.822495919,
             6.784073006,
             8.820928414,
             9.802516575,
             13.57351488,
             14.85644478,
             15.46095596,
             16.74411432,
             23.22913021,
             27.98023167,
             30.92202614,
             32.95888155,
             34.16664736,
             35.37441318,
             37.56216792,
             43.89651294,
             45.25506385,
             47.29237618,
             56.2659174,
             61.62255811,
             64.11599537)
  ) |>
    mutate(time_period = case_when(
      time < 12 ~ 12,
      time < 24 ~ 24,
      time < 36 ~ 36,
      time < 48 ~ 48,
      time < 60 ~ 60,
      time < 72 ~ 72
    )) |>
    group_by(time_period) |>
    count()
  
  svege_exercise_data <- tibble(
    time_period = as.factor(c(12,24,36,48,60,72)),
    event = svege_exercise_time$n,
    total = c(55,50,42,39,27,11),
    no_event = total - event
  ) |>
    mutate(
      study = "Svege et al. (2020) - Exercise + Education",
      joint = "Hip",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Svege et al. (2020) - Exercise + Education",
            joint = "Hip",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0) 
  
  svege_control_time <- tibble(
    time = c(4.444048161,
             4.97328102,
             7.31182086,
             7.614876067,
             9.274882953,
             9.427153059,
             10.48401954,
             11.39021515,
             12.82415862,
             13.73024,
             14.0332952,
             15.54251699,
             16.14725663,
             17.20412311,
             18.11020449,
             20.67515044,
             24.82322573,
             25.57863624,
             30.9328781,
             32.89456942,
             35.76097135,
             36.36559675,
             36.89482961,
             37.80102522,
             41.27056755,
             41.42295189,
             43.30925066,
             46.85418554,
             49.87171526,
             61.40906012,
             63.22487827)
  ) |>
    mutate(time_period = case_when(
      time < 12 ~ 12,
      time < 24 ~ 24,
      time < 36 ~ 36,
      time < 48 ~ 48,
      time < 60 ~ 60,
      time < 72 ~ 72
    )) |>
    group_by(time_period) |>
    count()
  
  svege_control_data <- tibble(
    time_period = as.factor(c(12,24,36,48,60,72)),
    event = svege_control_time$n,
    total = c(54,44,36,30,21,5),
    no_event = total - event
  ) |>
    mutate(
      study = "Svege et al. (2020) - Education Only",
      joint = "Hip",
      condition = "Control",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Svege et al. (2020) - Education Only",
            joint = "Hip",
            condition = "Control",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0) 
  
  svege_data <- bind_rows(svege_control_data, svege_exercise_data)
  
  # Bove et al. (2018) doi: 10.1093/ptj/pzx104
  # Follow-up over 2 years (only 2 year data reported) for exercise only, ex+booster, ex+manual therapy, and all three
  # Original RCT is Fitzgerald et al. (2016) doi: 10.1016/j.joca.2016.03.001
  # Knee OA, baseline pain ex = 5.4/10, ex+b = 6/10, ex+mt = 5.7/10, ex+mt+b = 5.6/10
  
  bove_ex_data <- tibble(
    time_period = as.factor(24),
    event = 5,
    no_event = 76,
    total = event + no_event
  ) |>
    mutate(
      study = "Bove et al. (2018)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Bove et al. (2018)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  bove_exb_data <- tibble(
    time_period = as.factor(24),
    event = 6,
    no_event = 76,
    total = event + no_event
  ) |>
    mutate(
      study = "Bove et al. (2018)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Bove et al. (2018)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  bove_exmt_data <- tibble(
    time_period = as.factor(24),
    event = 9,
    no_event = 75,
    total = event + no_event
  ) |>
    mutate(
      study = "Bove et al. (2018)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Bove et al. (2018)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  bove_exmtb_data <- tibble(
    time_period = as.factor(24),
    event = 5,
    no_event = 74,
    total = event + no_event
  ) |>
    mutate(
      study = "Bove et al. (2018)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Bove et al. (2018)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  bove_data <- bind_rows(bove_ex_data, bove_exb_data, bove_exmt_data, bove_exmtb_data)
  
  
  # Deyle et al. (2005) PMID: 16305269
  # Follow-up over 1 year clinic based physio led exericse + manual therapy vs home based exercise
  # Knee OA, WOMAC
  
  deyle_exmt_data <- tibble(
    time_period = as.factor(12),
    event = 7,
    no_event = 66,
    total = event + no_event
  ) |>
    mutate(
      study = "Deyle et al. (2015)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Deyle et al. (2015)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  deyle_ex_data <- tibble(
    time_period = as.factor(24),
    event = 6,
    no_event = 68,
    total = event + no_event
  ) |>
    mutate(
      study = "Deyle et al. (2015)",
      joint = "Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Deyle et al. (2015)",
            joint = "Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  deyle_data <- bind_rows(deyle_ex_data, deyle_exmt_data)
  
  # Greene and Miles (2022) doi: 10.60118/001c.37664
  # Follow-up over 12/24 months biomechanical footwear intervention
  # Knee OA, WOMAC pain 54.3/100
  
  greene_miles_data <- tibble(
    time_period = as.factor(c(12,24)),
    event = c(23,36),
    no_event = c(365,331),
    total = event + no_event
  ) |>
    mutate(
      study = "Greene and Miles (2022) - Biomechanical Footwear",
      joint = "Knee",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    add_row(time_period = as.factor(0),
            study = "Greene and Miles (2022) - Biomechanical Footwear",
            joint = "Knee",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0)
  
  
  # Pisters et al. (2010) doi: 10.1016/j.joca.2010.05.008
  # Follow-up over 5 years (continuous, though discrete by year extracted) for usual care vs behavioural graded activity
  # Hip/Knee OA, WOMAC pain 9.9/20 (BGA) and 8.4/20 (uc)
  
  pisters_uc_time <- tibble(
    time = c(7.051832201,
             12.07832751,
             13.06121737,
             14.04109664,
             18.99808675,
             21.02473259,
             23.12140721,
             26.12479084,
             34.01212523,
             36.03890197,
             37.01930482,
             45.04604228,
             52.09617285,
             60.06008076,
             61.04414867
    )
  ) |>
    mutate(time_period = case_when(
      time < 20 ~ 20,
      time < 40 ~ 40,
      time < 60 ~ 60,
      time < 62 ~ 62
    )) |>
    group_by(time_period) |>
    count()
  
  pisters_uc_data <- tibble(
    time_period = as.factor(c(20,40,60,62)),
    event = pisters_uc_time$n,
    total = c(40,28,21,13),
    no_event = total - event
  ) |>
    mutate(
      study = "Pisters et al. (2010)",
      joint = "Hip/Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Pisters et al. (2010)",
            joint = "Hip/Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  pisters_bga_time <- tibble(
    time = c(20.03071667,
             32.03561214,
             46.97085189,
             51.02231104,
             58.98176852
    )
  ) |>
    mutate(time_period = case_when(
      time < 20 ~ 20,
      time < 40 ~ 40,
      time < 60 ~ 60,
      time < 66 ~ 66
    )) |>
    group_by(time_period) |>
    count()
  
  pisters_bga_data <- tibble(
    time_period = as.factor(c(40,60)),
    event = pisters_bga_time$n,
    total = c(23,14),
    no_event = total - event
  ) |>
    mutate(
      study = "Pisters et al. (2010)",
      joint = "Hip/Knee",
      condition = "Intervention",
      design = "Randomised Controlled Trial",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Pisters et al. (2010)",
            joint = "Hip/Knee",
            condition = "Intervention",
            design = "Randomised Controlled Trial",
            surv = 1,
            surv_se = 0)
  
  pisters_data <- bind_rows(pisters_bga_data, pisters_uc_data)
  
  
  # Dabare et al. (2017) doi: 10.1111/1756-185X.13083
  # Follow-up over 6 years (continuous, though discrete by year extracted) for usual care (OAKHS at St Vincents)
  # Hip/Knee OA, HOOS/KOOS pain 44.44 (knee) 37.5 (hip)
  
  dabare_knee_time <- tibble(
    time = c(0.348434478,
             0.421890272,
             0.510066935,
             0.681622237,
             0.843195275,
             0.926455078,
             1.009744591,
             1.087998408,
             1.176071088,
             1.25447345,
             1.337748108,
             1.430752502,
             1.582595804,
             1.675644762,
             1.75891942,
             1.83724751,
             2.17053925,
             2.337029148,
             2.430137524,
             2.758512405,
             2.925106284,
             3.086842723,
             3.17992139,
             3.258249479,
             3.429760218,
             3.586505524,
             3.841365193,
             3.998169918,
             4.341265668,
             5.42448982,
             5.836169069
    )
  ) |>
    mutate(time = time*12,
           time_period = case_when(
             time < 12 ~ 12,
             time < 24 ~ 24,
             time < 36 ~ 36,
             time < 48 ~ 48,
             time < 60 ~ 60,
             time < 72 ~ 72
           )) |>
    group_by(time_period) |>
    count()
  
  dabare_knee_data <- tibble(
    time_period = as.factor(c(12,24,36,48,60,72)),
    event = dabare_knee_time$n,
    total = c(167,161,151,146,139,137),
    no_event = total - event
  ) |>
    mutate(
      study = "Dabare et al., (2017)",
      joint = "Knee",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Dabare et al., (2017)",
            joint = "Knee",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0)
  
  dabare_hip_time <- tibble(
    time = c(0.25909916,
             0.347350095,
             0.420523653,
             0.587058114,
             0.680047653,
             0.748408334,
             0.836332469,
             1.012537248,
             1.08539886,
             1.178269563,
             1.256300561,
             1.34426926,
             1.427380517,
             1.500643202,
             1.588522774,
             1.666821154,
             1.9264937,
             2.009590103,
             2.337905567,
             2.509371741,
             2.675891348,
             2.837434677,
             3.008871143,
             3.082282373,
             3.420312718,
             3.493723949,
             4.420083958
    )
  ) |>
    mutate(time = time*12,
           time_period = case_when(
             time < 12 ~ 12,
             time < 24 ~ 24,
             time < 36 ~ 36,
             time < 48 ~ 48,
             time < 60 ~ 60,
             time < 72 ~ 72
           )) |>
    group_by(time_period) |>
    count()
  
  dabare_hip_data <- tibble(
    time_period = as.factor(c(12,24,36,48,60)),
    event = dabare_hip_time$n,
    total = c(80,73,63,58,54),
    no_event = total - event
  ) |>
    mutate(
      study = "Dabare et al., (2017)",
      joint = "Hip",
      condition = "Intervention",
      design = "Single-arm Observational",
      surv = cumprod(1-event/total),
      surv_se = sqrt(((event/total) * (1 - event/total)) / total)
    ) |>
    relocate(total, .after = no_event)  |>
    add_row(time_period = as.factor(0),
            study = "Dabare et al., (2017)",
            joint = "Hip",
            condition = "Intervention",
            design = "Single-arm Observational",
            surv = 1,
            surv_se = 0)
  
  dabare_data <- bind_rows(dabare_hip_data, dabare_knee_data)
  
  # Combine studies data
  
  combined_studies_data <- bind_rows(
    skou_data,
    svege_data,
    bove_data,
    deyle_data,
    greene_miles_data,
    gwynne_jones_data,
    pisters_data,
    dabare_data
  )
  
  return(combined_studies_data)
  
}
