library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)

submissions_root <- '../covid19-forecast-hub/data-processed/'
hub_repo_path <- '../covid19-forecast-hub/'
hub <- "US"
source <- "local_hub_repo"

fdates <- seq.Date(as.Date("2021-05-03"), as.Date("2021-06-28"), by = "7 days")

hosp_mods <- map_dfr(
  paste0(hub_repo_path, "ensemble-metadata/", fdates,"-inc_hosp-model-eligibility.csv"),
  read_csv
  ) %>% pull(model) %>% unique()

fdat <- map_dfr(
  fdates,
  load_latest_forecasts,
  models = hosp_mods,
  forecast_date_window_size = 6,
  locations = fips_codes$location[1:52],
  types = "quantile",
  source = "local_hub_repo",
  targets = paste(1:7, "day ahead inc hosp"),
  hub_repo_path = '../covid19-forecast-hub/'
)

for (model1 in hosp_mods) {
  loc_all <- filter(fdat, model == model1) %>%
    pull(location) %>%
    unique() %>% intersect(covidData::fips_codes$location[1:58])
  truth_data <- load_truth(
    truth_source = "HealthData",
    target_variable = "inc hosp",
    locations = loc_all
  )
  pdf(
    file = paste0("~/Dropbox/Reich_lab/hosp_email_plots/", model1),
    width = 20,
    height = 12
  )
  for (i in seq(1, length(loc_all), by = 9)) {
    p <-
      plot_forecasts(
        fdat %>% filter(
          model == model1,
          location %in% loc_all,
          target_end_date > fdates[1] - 7
        ),
        locations = loc_all[i:min((i + 8), length(loc_all))],
        target_variable = "inc hosp",
        truth_data = truth_data %>% filter(target_end_date > fdates[1] - 7),
        truth_source = "HealthData",
        intervals = c(.5, .8, .95),
        facet = . ~ location,
        facet_scales = "free_y",
        facet_ncol = 3,
        subtitle = "none",
        plot = FALSE
      ) +
      scale_x_date(name = NULL,
                   date_breaks = "1 months",
                   date_labels = "%b") +
      theme(legend.position = "bottom", text = element_text(size = 16))
    print(p)
  }
  dev.off()
}
