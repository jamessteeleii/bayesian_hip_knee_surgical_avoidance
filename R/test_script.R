# If needed for KM curves
# https://doi.org/10.1186/1471-2288-12-9
# https://link.springer.com/article/10.1186/s12874-021-01308-8


library(tidyverse)


kieser_theme <- theme(
  plot.background = element_rect(fill = "#0084CE"),
  panel.grid = element_line(colour = "#FFFFFF", size = 0.1),
  panel.background = element_rect(fill = "#7FC1E6"),
  axis.title = element_text(colour = "white"),
  axis.text = element_text(colour = "white"),
  strip.background = element_rect(fill = "#3FA2DA", colour = NA),
  strip.text = element_text(colour = "white"),
  plot.title = element_text(colour = "white"),
  plot.subtitle = element_text(colour = "white"),
  plot.caption = element_text(colour = "white")
)

# GMHBA data
data_gmhba_surgery <- read_csv("data/initial_gmhba_data.csv") 
  # group_by(id) |>
  # mutate(
  #   surgery = case_when(
  #     surgery == "y" ~ 1,
  #     lag(surgery) == "y" ~ 1,
  #     lag(surgery, n = 2) == "y" ~ 1,
  #     lag(surgery, n = 3) == "y" ~ 1,
  #     surgery == "n" ~ 0,
  #     lead(surgery) == "n" ~ 0,
  #     lead(surgery, n = 2) == "n" ~ 0,
  #     lead(surgery, n = 3) == "n" ~ 0
  #   )
  # ) |>
  # mutate(
  #   include = case_when(
  #     surgery == 0 ~ "y",
  #     first(surgery) == 1 & time == first(time) ~ "y",
  #     surgery == 1 & lag(surgery) == 0 ~ "y",
  #     .default = "n"
  #   )
  # ) |>
  # filter(include == "y")


gmhba_data_summary <- data_gmhba_surgery |>
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
    dataset = "GMHBA members",
    # joint = "Hip",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  ungroup() |>
  add_row(time_period = as.factor(0),
          dataset = "GMHBA members",
          joint = "Hip",
          surv = 1,
          surv_se = 0
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "GMHBA members",
          joint = "Knee",
          surv = 1,
          surv_se = 0
  ) |>
  select(time_period, event, no_event, total, dataset, joint, surv, surv_se)

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
    dataset = "Skou et al. (2020)",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Skou et al. (2020)",
          joint = "Knee",
          surv = 1,
          surv_se = 0
  )


# Gwynne-Jones et al., (2020) doi: 10.1016/j.arth.2020.04.087
# Follow up at 5 years for Joint Clinic non-surgical management
# Knee OA,baseline OKS 22.1


# Bennell et al., (2021) doi: 10.7326/M21-2388
# Better Knee, Better Me

# Svege et al., (2015) doi: 10.1136/annrheumdis-2013-203628
# Follow-up over 6 years (continuous, though discrete by year extracted) for exercise plus education, versus education
# Hip OA, WOMAC pain 26 (exercise) and 27.3 (education only)

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
    dataset = "Svege et al. (2020) - Exercise + Education",
    joint = "Hip",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Svege et al. (2020) - Exercise + Education",
          joint = "Hip",
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
    dataset = "Svege et al. (2020) - Education Only",
    joint = "Hip",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Svege et al. (2020) - Education Only",
          joint = "Hip",
          surv = 1,
          surv_se = 0) 

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
    dataset = "Bove et al. (2018) - Exercise",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Bove et al. (2018) - Exercise",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

bove_exb_data <- tibble(
  time_period = as.factor(24),
  event = 6,
  no_event = 76,
  total = event + no_event
) |>
  mutate(
    dataset = "Bove et al. (2018) - Exercise + Booster",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Bove et al. (2018) - Exercise + Booster",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

bove_exmt_data <- tibble(
  time_period = as.factor(24),
  event = 9,
  no_event = 75,
  total = event + no_event
) |>
  mutate(
    dataset = "Bove et al. (2018) - Exercise + Manual Therapy",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Bove et al. (2018) - Exercise + Manual Therapy",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

bove_exmtb_data <- tibble(
  time_period = as.factor(24),
  event = 5,
  no_event = 74,
  total = event + no_event
) |>
  mutate(
    dataset = "Bove et al. (2018) - Exercise + Manual Therapy + Booster",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Bove et al. (2018) - Exercise + Manual Therapy + Booster",
          joint = "Knee",
          surv = 1,
          surv_se = 0)


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
    dataset = "Deyle et al. (2015) - Exercise + Manual Therapy",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Deyle et al. (2015) - Exercise + Manual Therapy",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

deyle_ex_data <- tibble(
  time_period = as.factor(24),
  event = 6,
  no_event = 68,
  total = event + no_event
) |>
  mutate(
    dataset = "Deyle et al. (2015) - Exercise",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Deyle et al. (2015) - Exercise",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

# Nelson et al. (1999) doi: 10.1016/s0003-9993(99)90302-7
# Follow-up over average 16 months ILEX/ + General Exercise
# Lumbar/cervical pathology, WOMAC

nelson_data <- tibble(
  time_period = as.factor(12),
  event = 3,
  no_event = 38,
  total = event + no_event
) |>
  mutate(
    dataset = "Nelson et al. (1999) - Exercise",
    joint = "Lumbar/Cervical",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Nelson et al. (1999) - Exercise",
          joint = "Lumbar/Cervical",
          surv = 1,
          surv_se = 0)

# Ansari et al. (2022) doi: 10.1080/21679169.2020.1786162
# Follow-up over average 18 months multimodal intervention
# Lumbar, pain 6.6/10, ODI 54.1

ansari_data <- tibble(
  time_period = as.factor(18),
  event = 9,
  no_event = 89,
  total = event + no_event
) |>
  mutate(
    dataset = "Ansari et al. (2022) - Exercise + Manual Therapy",
    joint = "Lumbar",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Ansari et al. (2022) - Exercise + Manual Therapy",
          joint = "Lumbar",
          surv = 1,
          surv_se = 0)

# Ansari et al. (2020) doi: 10.1016/j.physio.2020.03.258
# Follow-up over 12 months multimodal intervention
# Lumbar

ansari_abstract_data <- tibble(
  time_period = as.factor(12),
  event = 12,
  no_event = 148,
  total = event + no_event
) |>
  mutate(
    dataset = "Ansari et al. (2020) - Exercise + Manual Therapy",
    joint = "Lumbar",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Ansari et al. (2020) - Exercise + Manual Therapy",
          joint = "Lumbar",
          surv = 1,
          surv_se = 0)


# Minetama et al. (2021) doi: 10.1177/0269215520986688
# Follow up at 1 year, supervised physio vs unsupervised exercise
# Lumbar stenosis, back pain physio= 5.3/10, home exercise = 4.9/10, leg pain physio= 6.3/10, home exercise = 6.2/10

minetama_physio_data <- tibble(
  time_period = as.factor(12),
  event = 1,
  no_event = 38,
  total = event + no_event
) |>
  mutate(
    dataset = "Minetama et al. (2021) - Physiotherapy",
    joint = "Lumbar",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Minetama et al. (2021) - Physiotherapy",
          joint = "Lumbar",
          surv = 1,
          surv_se = 0)

minetama_ex_data <- tibble(
  time_period = as.factor(12),
  event = 9,
  no_event = 30,
  total = event + no_event
) |>
  mutate(
    dataset = "Minetama et al. (2021) - Home Exercise",
    joint = "Lumbar",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Minetama et al. (2021) - Home Exercise",
          joint = "Lumbar",
          surv = 1,
          surv_se = 0)

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
    dataset = "Greene and Miles (2022) - Biomechanical Footwear",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Greene and Miles (2022) - Biomechanical Footwear",
          joint = "Knee",
          surv = 1,
          surv_se = 0)

# Drew et al. (2022) doi: 10.1089/pop.2021.0336
# Follow-up over 12/24 months biomechanical footwear intervention
# Knee OA, WOMAC pain 54.7/100

drew_data <- tibble(
  time_period = as.factor(c(12,24)),
  event = c(22,12),
  no_event = c(237,210),
  total = event + no_event
) |>
  mutate(
    dataset = "Drew et al. (2022) - Biomechanical Footwear",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  add_row(time_period = as.factor(0),
          dataset = "Drew et al. (2022) - Biomechanical Footwear",
          joint = "Knee",
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
    dataset = "Pisters et al. (2010) - Usual Care",
    joint = "Hip/Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Pisters et al. (2010) - Usual Care",
          joint = "Hip/Knee",
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
    dataset = "Pisters et al. (2010) - Behavioural Graded Activity",
    joint = "Hip/Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Pisters et al. (2010) - Behavioural Graded Activity",
          joint = "Hip/Knee",
          surv = 1,
          surv_se = 0)



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
    dataset = "Dabare et al., (2017) - OAHKS",
    joint = "Knee",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Dabare et al., (2017) - OAHKS",
          joint = "Knee",
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
    dataset = "Dabare et al., (2017) - OAHKS",
    joint = "Hip",
    surv = cumprod(1-event/total),
    surv_se = sqrt(((event/total) * (1 - event/total)) / total)
  ) |>
  relocate(total, .after = no_event)  |>
  add_row(time_period = as.factor(0),
          dataset = "Dabare et al., (2017) - OAHKS",
          joint = "Hip",
          surv = 1,
          surv_se = 0)


          
# Use pseudo ipd algorithm?
          # # Gustafsson et al. (2022) doi: 10.1302/0301-620X.104B7.BJJ-2021-1766.R1
          # # Follow-up over 9 years (continuous, though discrete by year extracted) for usual care (BOA registry)
          # # Hip/Knee OA, pain 5.3/10 (knee) 5.5/10 (hip)
          # 
          # gustafsson_knee_time <- tibble(
          #   time = c(0.17063922,
          #            0.285752979,
          #            0.373781148,
          #            0.45503792,
          #            0.509209101,
          #            0.563380282,
          #            0.604008667,
          #            0.678494041,
          #            0.732665222,
          #            0.807150596,
          #            0.868093174,
          #            0.942578548,
          #            0.996749729,
          #            1.057692308,
          #            1.111863489,
          #            1.16603467,
          #            1.226977248,
          #            1.260834236,
          #            1.315005417,
          #            1.375947996,
          #            1.430119177,
          #            1.484290358,
          #            1.545232936,
          #            1.579089924,
          #            1.640032503,
          #            1.694203684,
          #            1.755146262,
          #            1.809317443,
          #            1.883802817,
          #            1.917659805,
          #            1.971830986,
          #            2.032773564,
          #            2.086944745,
          #            2.141115926,
          #            2.202058505,
          #            2.256229686,
          #            2.337486457,
          #            2.384886241,
          #            2.445828819,
          #            2.533856988,
          #            2.594799567,
          #            2.655742145,
          #            2.730227519,
          #            2.804712893,
          #            2.879198267,
          #            2.95368364,
          #            2.987540628,
          #            3.048483207,
          #            3.122968581,
          #            3.183911159,
          #            3.23808234,
          #            3.312567714,
          #            3.387053088,
          #            3.441224269,
          #            3.556338028,
          #            3.610509209,
          #            3.705308776,
          #            3.800108342,
          #            3.874593716,
          #            3.989707476,
          #            4.064192849,
          #            4.125135428,
          #            4.172535211,
          #            4.253791983,
          #            4.307963164,
          #            4.389219935,
          #            4.463705309,
          #            4.51787649,
          #            4.612676056,
          #            4.68716143,
          #            4.748104009,
          #            4.80227519,
          #            4.890303359,
          #            4.97156013,
          #            5.046045504,
          #            5.11375948,
          #            5.235644637,
          #            5.343986999,
          #            5.384615385,
          #            5.492957746,
          #            5.533586132,
          #            5.648699892,
          #            5.743499458,
          #            5.817984832,
          #            5.926327194,
          #            5.99404117,
          #            6.095612134,
          #            6.190411701,
          #            6.231040087,
          #            6.319068256,
          #            6.413867822,
          #            6.56283857,
          #            6.691495125,
          #            6.738894908,
          #            6.759209101,
          #            6.793066089,
          #            6.942036836,
          #            7.036836403,
          #            7.070693391,
          #            7.199349946,
          #            7.449891658,
          #            7.483748646,
          #            7.565005417,
          #            7.598862405,
          #            7.72751896,
          #            7.774918743,
          #            7.802004334,
          #            7.862946912,
          #            7.944203684,
          #            7.998374865,
          #            8.059317443,
          #            8.093174431,
          #            8.113488624,
          #            8.133802817,
          #            8.147345612,
          #            8.187973998,
          #            8.208288191,
          #            8.221830986,
          #            8.242145179,
          #            8.282773564,
          #            8.303087757
          #            
          #   )
          # ) |>
          #   mutate(time = time*12,
          #          time_period = case_when(
          #            time < 12 ~ 12,
          #            time < 24 ~ 24,
          #            time < 36 ~ 36,
          #            time < 48 ~ 48,
          #            time < 60 ~ 60,
          #            time < 72 ~ 72,
          #            time < 84 ~ 84,
          #            time < 96 ~ 96,
          #            time < 108 ~108
          #          )) |>
          #   group_by(time_period) |>
          #   count()
          # 
          # gustafsson_knee_data <- tibble(
          #   time_period = as.factor(c(12,24,36,48,60,72,84,96,108)),
          #   event = gustafsson_knee_time$n,
          #   total = c(167,161,151,146,139,137),
          #   no_event = total - event
          # ) |>
          #   mutate(
          #     dataset = "gustafsson et al., (2017) - OAHKS",
          #     joint = "Knee",
          #     surv = cumprod(1-event/total),
          #     surv_se = sqrt(((event/total) * (1 - event/total)) / total)
          #   ) |>
          #   relocate(total, .after = no_event)  |>
          #   add_row(time_period = as.factor(0),
          #           dataset = "gustafsson et al., (2017) - OAHKS",
          #           joint = "Knee",
          #           surv = 1,
          #           surv_se = 0)
          # 
          # gustafsson_hip_time <- tibble(
          #   time = c(0.25909916,
          #            0.347350095,
          #            0.420523653,
          #            0.587058114,
          #            0.680047653,
          #            0.748408334,
          #            0.836332469,
          #            1.012537248,
          #            1.08539886,
          #            1.178269563,
          #            1.256300561,
          #            1.34426926,
          #            1.427380517,
          #            1.500643202,
          #            1.588522774,
          #            1.666821154,
          #            1.9264937,
          #            2.009590103,
          #            2.337905567,
          #            2.509371741,
          #            2.675891348,
          #            2.837434677,
          #            3.008871143,
          #            3.082282373,
          #            3.420312718,
          #            3.493723949,
          #            4.420083958
          #   )
          # ) |>
          #   mutate(time = time*12,
          #          time_period = case_when(
          #            time < 12 ~ 12,
          #            time < 24 ~ 24,
          #            time < 36 ~ 36,
          #            time < 48 ~ 48,
          #            time < 60 ~ 60,
          #            time < 72 ~ 72
          #          )) |>
          #   group_by(time_period) |>
          #   count()
          # 
          # gustafsson_hip_data <- tibble(
          #   time_period = as.factor(c(12,24,36,48,60)),
          #   event = gustafsson_hip_time$n,
          #   total = c(80,73,63,58,54),
          #   no_event = total - event
          # ) |>
          #   mutate(
          #     dataset = "gustafsson et al., (2017) - OAHKS",
          #     joint = "Hip",
          #     surv = cumprod(1-event/total),
          #     surv_se = sqrt(((event/total) * (1 - event/total)) / total)
          #   ) |>
          #   relocate(total, .after = no_event)  |>
          #   add_row(time_period = as.factor(0),
          #           dataset = "gustafsson et al., (2017) - OAHKS",
          #           joint = "Hip",
          #           surv = 1,
          #           surv_se = 0)



# https://pmc.ncbi.nlm.nih.gov/articles/PMC10387773/ 

bind_rows(
  skou_data,
  svege_control_data,
  svege_exercise_data,
  bove_ex_data,
  bove_exb_data,
  bove_exmt_data,
  bove_exmtb_data,
  deyle_ex_data,
  deyle_exmt_data,
  nelson_data,
  ansari_data,
  ansari_abstract_data,
  minetama_ex_data,
  minetama_physio_data,
  greene_miles_data,
  drew_data,
  pisters_bga_data,
  pisters_uc_data,
  dabare_hip_data,
  dabare_knee_data
) |>
  # mutate(
  #   wi = 1/surv_se,
  #   size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE))) |>
  ggplot(aes(x = as.numeric(as.character(time_period)), group=interaction(dataset,joint), y = surv)) +
  geom_line(alpha = 0.5) +
  # geom_point(aes(size=size), alpha(0.5)) +
  geom_pointrange(aes(ymin = surv-surv_se, ymax = surv+surv_se), size = 0.25, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 72),
                     breaks = c(0, 12, 24, 36, 48, 60, 72)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Time (months)",
       y = "Probability of Avoiding Surgery") +
  # facet_grid(joint ~ dataset) +
  theme_bw()





combined_studies_data <- bind_rows(
  skou_data,
  svege_control_data,
  svege_exercise_data,
  bove_ex_data,
  bove_exb_data,
  bove_exmt_data,
  bove_exmtb_data,
  deyle_ex_data,
  deyle_exmt_data,
  nelson_data,
  ansari_data,
  ansari_abstract_data,
  minetama_ex_data,
  minetama_physio_data,
  greene_miles_data,
  drew_data,
  pisters_bga_data,
  pisters_uc_data,
  dabare_hip_data,
  dabare_knee_data
) |>
  mutate(
    vi_weight = 1/sqrt(surv_se),
    time = as.numeric(as.character(time_period)),
    time_period = as.factor(time)
    )


# Model just knee/hip prior studies

library(brms)
library(marginaleffects)
library(tidybayes)

cloglog2prob <- function(x) {
  1 - exp(-exp(x))
}


combined_studies_model<- brm(data = filter(combined_studies_data, time != 0) |>
                                           filter(str_detect(joint, "Knee|Hip")),
                                         family = binomial(link = cloglog),
                                         event | trials(total) + weights(vi_weight) ~ 0 + time_period + (1|dataset),
                                         prior = prior(normal(-2, 1), class = b),
                                         chains = 4, cores = 4, iter = 4000, warmup = 1000,
                                         seed = 1988)

pp_check(combined_studies_model)

plot(combined_studies_model)

get_prior(combined_studies_model)


# GMHBA modelling

broom.mixed::tidy(combined_studies_model)

prior <- 
    c(
      # We set weakly regularising priors for this model as with completely uninformative flat priors on the coefficients chains do not converge
      set_prior("normal(-2.243447, 0.25)", class = "b", coef = "time_period6"), # mean of all estimates as no period for 6 months
      set_prior("normal(-2.08, 0.212)", class = "b", coef = "time_period12"),
      set_prior("normal(-2.22, 0.228)", class = "b", coef = "time_period36"),
      set_prior("normal(-3.42, 0.233)", class = "b", coef = "time_period60"),
      set_prior("student_t(3, 0.713, 0.204)", class = "sd", group = "dataset")
      )

gmhba_data_summary <- gmhba_data_summary |>
  mutate(
    vi_weight = 1/sqrt(surv_se),
    time = as.numeric(as.character(time_period)),
    time_period = as.factor(time),
    dataset = "GMHBA"
  )
  


gmhba_model <- brm(data = filter(gmhba_data_summary, time_period != "0"),
                              family = binomial(link = cloglog),
                              event | trials(total) + weights(vi_weight) ~ 0 + time_period + (1|dataset),
                              prior = prior,
                              chains = 4, cores = 4, iter = 4000, warmup = 1000,
                              seed = 1988)

pp_check(gmhba_model)

plot(gmhba_model)


# Get prior and GMHBA preds
combined_studies_preds <- avg_predictions(combined_studies_model, 
                                          variables = "time_period",
                                          type = "link") |>
  mutate(
    prior_posterior = "Prior"
  )

gmhba_preds <- avg_predictions(gmhba_model, 
                                   variables = "time_period",
                                   type = "link") |>
  mutate(
    prior_posterior = "Posterior"
  )



# Just Posterior Plot

gmhba_preds_plot <- gmhba_preds |>
  mutate(    time = as.numeric(as.character(time_period))) |>
  mutate(prob = cloglog2prob(estimate),
         prob_low = cloglog2prob(conf.low),
         prob_high = cloglog2prob(conf.high))|>
  mutate(
    surv = cumprod(1-prob),
    surv_lower = cumprod(1-prob_low),
    surv_upper = cumprod(1-prob_high),
  )  |>
  add_row(time = 0,
          surv = 1,
          prior_posterior = "Posterior",
          surv_lower = 1,
          surv_upper = 1
  ) |>
  ggplot(aes(x = time)) +
  geom_point(data=filter(gmhba_data_summary, time != 0) |>
               mutate(wi = 1/surv_se,
                      size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE))
               ) |> mutate(prior_posterior = "Posterior"),
             aes(y=surv, size = size), alpha=0.25) +
  geom_ribbon(aes(ymin=surv_lower, ymax=surv_upper), alpha = 0.5, fill = "#0084CE") +
  geom_line(aes(y=surv), alpha = 0.75) +
  scale_x_continuous(limits = c(0, 60),
                     breaks = c(0, 12, 24, 36, 48, 60)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Time (months)",
       y = "Probability of Avoiding Surgery") +
  guides(
    size = "none"
  ) +
  kieser_theme

gmhba_preds_plot

ggsave(gmhba_preds_plot, filename = "gmhba_preds_plot.tiff", device = "tiff", dpi = 300, width = 6, height = 3)


# Plot all

all_preds <- bind_rows(combined_studies_preds, gmhba_preds)

all_data <- bind_rows(combined_studies_data |> mutate(prior_posterior = "Prior") |>
                        filter(str_detect(joint, "Knee|Hip")), 
                      gmhba_data_summary |> mutate(prior_posterior = "Posterior")) |>
  group_by(prior_posterior) |>
  mutate(wi = if_else(surv_se > 0, 1/surv_se, NA),
         size = 0.25 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE))
  )

all_preds_plot <- all_preds |>
  mutate(time = as.numeric(as.character(time_period))) |>
  mutate(prob = cloglog2prob(estimate),
         prob_low = cloglog2prob(conf.low),
         prob_high = cloglog2prob(conf.high))|>
  group_by(prior_posterior) |>
  mutate(
    surv = cumprod(1-prob),
    surv_lower = cumprod(1-prob_low),
    surv_upper = cumprod(1-prob_high),
  )  |>
  ungroup() |>
  add_row(time = 0,
          surv = 1,
          prior_posterior = "Prior",
          surv_lower = 1,
          surv_upper = 1
  ) |>
  add_row(time = 0,
          surv = 1,
          prior_posterior = "Posterior",
          surv_lower = 1,
          surv_upper = 1
  ) |>
  ggplot(aes(x = time)) +
  geom_line(data = all_data,
            aes(y = surv, group=interaction(dataset,joint)),
            alpha = 0.1) +
  geom_point(data= all_data,
             aes(y=surv, size = size), alpha=0.1) +
  geom_ribbon(aes(ymin=surv_lower, ymax=surv_upper), alpha = 0.5, fill = "#0084CE") +
  geom_line(aes(y=surv), alpha = 0.75) +
  geom_text(
    data=gmhba_preds |> filter(time_period != "0") |> 
      mutate(time = as.numeric(as.character(time_period))) |>
      mutate(prob = cloglog2prob(estimate),
             prob_low = cloglog2prob(conf.low),
             prob_high = cloglog2prob(conf.high))|>
      mutate(
        surv = cumprod(1-prob),
        surv_lower = cumprod(1-prob_low),
        surv_upper = cumprod(1-prob_high),
      ),
    aes(y = surv*0.85,
        label = glue::glue("{round(surv,2)*100}% [{round(surv_upper,2)*100}% to {round(surv_lower,2)*100}%]"),
        size = 0.5)
  ) +
  scale_x_continuous(limits = c(0, 72),
                     breaks = c(0, 12, 24, 36, 48, 60, 72)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(x = "Time (months)",
       y = "Probability of Avoiding Surgery",
       caption = "Note, estimates are posteriors means and 95% quantile intervals",
       title = "Two-Stage Bayesian Discrete Time Proportional Hazards Meta-Analysis") +
  guides(
    size = "none"
  ) +
  facet_grid(. ~ factor(prior_posterior, levels = c("Prior", "Posterior"))) +
  kieser_theme 

all_preds_plot

ggsave(all_preds_plot, filename = "all_preds_plot_preds.tiff", device = "tiff", dpi = 300, width = 8, height = 3)




# IML

get_iml <- function(lt) {
  
  # lt is a generic name for a life table of the 
  # kind we made with our `make_lt()` function
  
  # determine the mth row
  lt_m <-
    lt %>% 
    filter(surv > .5) %>% 
    slice(n())
  
  # determine the row for m + 1
  lt_m1 <-
    lt %>% 
    filter(surv < .5) %>% 
    slice(1)
  
  # pull the value for m
  m  <- pull(lt_m, time)
  
  # pull the value for m +1
  m_1  <- pull(lt_m1, time)
  
  # pull the two survival function values
  stm  <- pull(lt_m, surv)
  stm1 <- pull(lt_m1, surv)
  
  # plug the values into Equation 10.6 (page 338)
  iml <- m + ((stm - .5) / (stm - stm1)) * ((m_1) - m)
  
  iml
  
}

all_preds_iml <- all_preds |>
  mutate(prob = cloglog2prob(estimate),
         prob_low = cloglog2prob(conf.low),
         prob_high = cloglog2prob(conf.high))|>
  group_by(prior_posterior) |>
  mutate(
    surv = cumprod(1-prob),
    surv_lower = cumprod(1-prob_low),
    surv_upper = cumprod(1-prob_high),
  )  |>
  ungroup() |>
  add_row(time = 0,
          surv = 1,
          prior_posterior = "Prior",
          surv_lower = 1,
          surv_upper = 1
  ) |>
  add_row(time = 0,
          surv = 1,
          prior_posterior = "Posterior",
          surv_lower = 1,
          surv_upper = 1
  )


all_preds_iml |>
  group_by(prior_posterior) |>
  get_iml()


# determine the mth row
lt_m <-
  all_preds_iml %>% 
  filter(surv > .5) %>% 
  slice(n())

# determine the row for m + 1
lt_m1 <-
  lt %>% 
  filter(surv < .5) %>% 
  slice(1)

# pull the value for m
m  <- pull(lt_m, time)

# pull the value for m +1
m_1  <- pull(lt_m1, time)

# pull the two survival function values
stm  <- pull(lt_m, surv)
stm1 <- pull(lt_m1, surv)

# plug the values into Equation 10.6 (page 338)
iml <- m + ((stm - .5) / (stm - stm1)) * ((m_1) - m)

iml


# 
# combined_preds <- avg_predictions(combined_dataset, 
#                                   variables = "time",
#                                   type = "link", 
#                                   re_formula = NA) |>
#   # posterior_draws() |>
#   mutate(prob = cloglog2prob(estimate),
#          prob_low = cloglog2prob(conf.low),
#          prob_high = cloglog2prob(conf.high)) |>
#   mutate(
#     surv = cumprod(1-prob),
#     surv_lower = cumprod(1-prob_low),
#     surv_upper = cumprod(1-prob_high),
#   )  |>
#   add_row(time = 0,
#           surv = 1,
#           surv_lower = 1,
#           surv_upper = 1
#   ) |>
#   ggplot(aes(x = time)) +
#   geom_line(data = combined_data,
#             aes(y = surv, group=interaction(dataset,joint)), 
#             alpha = 0.25) +
#   # geom_pointrange(data = combined_data,
#   #                 aes(y = surv, ymin = surv-surv_se, ymax = surv+surv_se), 
#   #                 size = 0.25, alpha = 0.25) +
#   geom_point(data=filter(combined_data, time != 0) |>
#                mutate(  wi = 1/surv_se,
#                         size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE))
#                ),
#              aes(y=surv, size = size), alpha=0.25) +
#   geom_ribbon(aes(ymin=surv_lower, ymax=surv_upper), alpha = 0.5, fill = "#0084CE") +
#   geom_line(aes(y=surv)) +
#   scale_x_continuous(limits = c(0, 18),
#                      breaks = c(0, 6, 12, 18)) +
#   scale_y_continuous(limits = c(0, 1),
#                      breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
#   labs(x = "Time (months)",
#        y = "Probability of Avoiding Surgery",
#        title = "Meta-analysis of surgical avoidance from previous studies (k=4) of exercise/physiotherapy interventions",
#        caption = "DOIs for studies included: 10.1016/s0003-9993(99)90302-7, 10.1177/0269215520986688, 10.1016/j.physio.2020.03.258, 10.1080/21679169.2020.1786162") +
#   guides(
#     size = "none"
#   ) +
#   # facet_grid(joint ~ dataset) +
#   # theme_bw(base_size = 6) 
#   theme_minimal(base_size = 6) +
#   kieser_theme
# 
# combined_preds
# 
# ggsave(combined_preds, filename = "quick_meta_spine.tiff", device = "tiff", dpi = 300, width = 5, height = 2.5)