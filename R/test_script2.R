library(brms)

all_data_model <- all_data |>
  filter(time_period != "0") |>
  mutate(
    intervention = ifelse(condition == "Intervention", 1, 0),
    design = factor(design),
    study = factor(study),
    time_period = factor(time_period)
  )

model <- brm(
  event | trials(total) ~ 
    0 + time_period +                    # baseline discrete hazard
    intervention +                       # overall treatment effect
    (1 | study),                         # baseline heterogeneity
  data = all_data_model,
  family = binomial(link = "cloglog"),
  prior = c(
    prior(normal(-2, 1), class = b),
    prior(normal(0, 0.7), class = "b", coef = "intervention"),
    prior(exponential(1), class = "sd")
  ),
  chains = 4, cores = 4,
  iter = 4000, warmup = 1000,
  seed = 1988
)


library(marginaleffects)

pred_draws <- predictions(
  model,
  newdata = datagrid(
    study = "Pilot Data",
    time_period = c("6","12","36","60"),
    intervention = c(0, 1)
  ),
  type = "link",
  re_formula = NULL
) |>
  get_draws()

haz_draws <- pred_draws |>
  select(draw, time_period, intervention, estimate) |>
  pivot_wider(
    names_from = intervention,
    values_from = estimate,
    names_prefix = "haz_"
  ) |>
  arrange(draw, time_period)


surv_draws <- haz_draws |>
  group_by(draw) |>
  mutate(
    surv_0 = cumprod(1 - haz_0),
    surv_1 = cumprod(1 - haz_1),
    delta_surv = surv_1 - surv_0
  ) |>
  ungroup()


