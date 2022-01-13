## -----------------------------------------
## 0. Package Loading
## -----------------------------------------

library(tidyverse)
devtools::load_all()

## -----------------------------------------
##  1. Reading Runs in
## -----------------------------------------

stem <- cp_path("archive/india_sub_national/")
reports_all <- function(task = "india_sub_national", wd = cp_path("")) {

  wdold <- getwd()
  setwd(wd)
  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  # get parameters
  pars <- DBI::dbGetQuery(db, 'select * from parameters')
  reports <- DBI::dbGetQuery(db, 'select * from report_version')
  reports <- reports %>% filter(report == task)
  pars <- pars %>% filter(report_version %in% reports$id)
  pars <- pars %>% select(-c("type","id")) %>%
    pivot_wider(names_from = "name", values_from = "value")

  setwd(wdold)
  return(pars)
}
reports_here <- reports_all()

# pick which reports are being summarised
# reports <- reports_here %>% filter(dur_R == 222) %>% group_by(state) %>% filter(report_version == max(report_version))
# lf <- file.path(stem, reports$report_version, "res.rds")
# l <- file.path(stem, reports$report_version)
# tfs <- lapply(tail(l, 36), function(x) { readRDS(file.path(x, "proj.rds"))})
# ind_res <- do.call(rbind, tfs)
# saveRDS(ind_res, paste0("analysis/data/derived/optimistic_projections_", reports$date[1], ".rds"))

# Central
reports <- reports_here %>% filter(dur_R == 115) %>% group_by(state) %>% filter(report_version == max(report_version))
lf <- file.path(stem, reports$report_version, "res.rds")
l <- file.path(stem, reports$report_version)
tfs <- lapply(tail(l, 36), function(x) { readRDS(file.path(x, "proj.rds"))})
ind_res <- do.call(rbind, tfs)
saveRDS(ind_res, paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))

# Worst
# reports <- reports_here %>% filter(dur_R == 70) %>% group_by(state) %>% filter(report_version == max(report_version))
# lf <- file.path(stem, reports$report_version, "res.rds")
# l <- file.path(stem, reports$report_version)
# tfs <- lapply(tail(l, 36), function(x) { readRDS(file.path(x, "proj.rds"))})
# ind_res <- do.call(rbind, tfs)
# saveRDS(ind_res, paste0("analysis/data/derived/worst_projections_", reports$date[1], ".rds"))

## -----------------------------------------
##  3. Downstream Plots
## -----------------------------------------

## ----------------------------------------------------------
## Overall India sero
## ----------------------------------------------------------

res <- readRDS(file.path(tail(l,1), "res.rds"))
sero_det <- res$pmcmc_results$inputs$pars_obs$sero_det
demog <- readRDS(file.path(here::here(),"src/india_sub_national/demog.rds"))
S <- sum(demog$n)

# additional_functions for rolling
roll_func <- function(x, det) {
  l <- length(det)
  ret <- rep(0, length(x))
  for(i in seq_along(ret)) {
    to_sum <- tail(x[seq_len(i)], length(det))
    ret[i] <- sum(rev(to_sum)*head(det, length(to_sum)),na.rm=TRUE)
  }
  return(ret)
}


ind_res <- readRDS(paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))
inf <- ind_res %>%
  filter(r_increase == "No Change") %>%
  group_by(date, replicate) %>%
  summarise(symptoms = sum(symptoms, na.rm = TRUE)) %>%
  group_by(replicate) %>%
  mutate(sero_positive = roll_func(symptoms, sero_det),
         sero_perc = sero_positive/S) %>%
  group_by(date) %>%
  summarise(sero_perc_med = median(sero_perc, na.rm=TRUE),
            sero_perc_min = quantile(sero_perc, 0.025, na.rm=TRUE),
            sero_perc_max = quantile(sero_perc, 0.975, na.rm=TRUE))

nat_sero <- data.frame(
  "date_start" = as.Date(c("2020-12-01","2020-05-11", "2020-08-18", "2021-06-16")),
  "date_end" = as.Date(c("2021-01-31","2020-06-04", "2020-09-20", "2021-07-16")),
  "sero" = c(0.241,0.0073, 0.066, 0.676),
  "sero_min" = c(0.23,0.0034, 0.058, 0.661),
  "sero_max" = c(0.253,0.0113, 0.074, 0.685)
  )

owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
owid <- owid %>% filter(iso_code == "IND") %>% mutate(date = as.Date(date))
inf <- left_join(inf, owid[,c("date", "people_vaccinated_per_hundred")])
inf$vacc_sero_pos <- zoo::na.approx(lag(inf$people_vaccinated_per_hundred/100), na.rm=FALSE)
inf$t <- as.integer(inf$date-min(inf$date,na.rm=TRUE))

inf$sero_perc_med_total <- inf$sero_perc_med + inf$vacc_sero_pos
inf$sero_perc_min_total <- inf$sero_perc_min + inf$vacc_sero_pos
inf$sero_perc_max_total <- inf$sero_perc_max + inf$vacc_sero_pos

sero_overall_gg <- inf[,c(1:4,8:10)] %>% pivot_longer(c(sero_perc_med:sero_perc_max_total)) %>%
  mutate(group = "Seroprevalence without \nantibodies due to vaccination") %>%
  mutate(group = replace(group, grep("_total",name), "Seroprevalence with \nantibodies due to vaccination")) %>%
  mutate(name = gsub("_total","",name)) %>%
  pivot_wider(names_from = name, values_from = value) %>%
ggplot(aes(date, sero_perc_med, ymin = sero_perc_min, ymax = sero_perc_max, color = group, fill = group)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  geom_point(aes(x = date_start + (date_end-date_start)/2, y = sero),
             nat_sero, inherit.aes = FALSE) +
  geom_errorbar(aes(x = date_start + (date_end-date_start)/2,
                    ymin = sero_min, ymax = sero_max),
                nat_sero, inherit.aes = FALSE, width = 0) +
  geom_errorbarh(aes(y = sero, xmin = date_start, xmax = date_end),
                 nat_sero, inherit.aes = FALSE, height = 0) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") +
   ylab("Seroprevalence") + xlab("") +
  scale_fill_viridis_d(name = "") +
  scale_color_viridis_d(name = "")
save_figs("sero_national", sero_overall_gg, 8, 4, root = file.path(here::here(), "analysis/india/plots"))


ggplot(owid, aes(date)) +
  geom_line(aes(y = people_fully_vaccinated_per_hundred, color = "People Fully Vaccinated / 100")) +
  geom_line(aes(y = people_vaccinated_per_hundred, color = "People Vaccinated / 100")) +
  theme_bw() +
  scale_x_date(limits = as.Date(c("2021-01-01","2021-07-21"))) +
  ylab("Value") +
  theme(legend.position = "top")

# sero contributions
sero_cont <- ind_res %>% group_by(date, replicate, state) %>%
  summarise(symptoms = sum(symptoms, na.rm = TRUE)) %>%
  group_by(replicate, state) %>%
  mutate(sero_positive = roll_func(symptoms, sero_det),
         sero_perc = sero_positive/S) %>%
  filter(date < "2020-09-20" & date > "2020-08-18") %>%
  group_by(state) %>% summarise(s = sum(sero_positive)) %>%
  mutate(perc = s/sum(s))

## ----------------------------------------------------------
## Overall India deaths
## ----------------------------------------------------------

deaths <- ind_res  %>%
  filter(r_increase == "No Change") %>%
  group_by(date, replicate) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(replicate) %>%
  group_by(date) %>%
  summarise(y = median(deaths, na.rm=TRUE),
            ymin = quantile(deaths, 0.025, na.rm=TRUE),
            ymax = quantile(deaths, 0.975, na.rm=TRUE))

check <- read.csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/output-data/export_country.csv")
check <- check %>% filter(iso3c == "IND") %>% arrange(date)
owid <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
owid <- owid %>% filter(iso_code == "IND")

gg <- ggplot(deaths, aes(date, y, ymin = ymin, ymax = ymax, color = "Model Predicted")) +
  geom_line() +
  geom_line(aes(as.Date(date), new_deaths, color = "Observed"), data = owid, inherit.aes = FALSE) +
  geom_ribbon(alpha = 0.2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", limits = as.Date(c("2020-02-01", max_date))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + ylab("Daily Deaths") + xlab("") +
  geom_line(aes(as.Date(date), estimated_daily_excess_deaths, color = "Economist Inferred"),
          data = check, inherit.aes = FALSE, linetype = "dashed") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Source:")

gg2 <- ggplot(deaths, aes(date, cumsum(y), ymin = cumsum(ymin), ymax = cumsum(ymax), color = "Model Predicted")) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  geom_line(aes(as.Date(date), total_deaths, color = "Observed"), data = owid, inherit.aes = FALSE) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months", limits = as.Date(c("2020-02-01", max_date))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + ylab("Total Deaths") + xlab("") +
  geom_line(aes(as.Date(date), cumsum(estimated_daily_excess_deaths)*7, color = "Economist Inferred"),
            data = check, inherit.aes = FALSE, linetype = "dashed") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Source:")

overall_deaths_gg <- cowplot::plot_grid(gg, gg2, rel_widths = c(1,1), ncol = 2)
save_figs("deaths_national", overall_deaths_gg, 8, 4)

## --------------------------------
## Checks for reporting
## --------------------------------

demog <- readRDS(file.path(here::here(), "src/india_sub_national/demog.rds"))
pars_init <- readRDS(file.path(here::here(), "src/india_sub_national/pars_init.rds"))
death_reporting <- readRDS(file.path(here::here(), "analysis/data/derived/death_reporting.rds"))
sero_df <- readRDS(file.path(here::here(), "src/india_sub_national/sero.rds"))
reporting_comp <- left_join(as.data.frame(t(as.data.frame(lapply(pars_init$optimistic, "[[", "rf")))) %>%
            mutate(state = rownames(.)) %>%
            rename(rf = V1) %>%
  mutate(state = gsub("\\.", " ", state)),
  death_reporting)
reporting_comp$sero_informed <- FALSE
reporting_comp$sero_informed[reporting_comp$state %in% (sero_df %>% select(state) %>% pull %>% unique())] <- TRUE

reporting_comp$glm_rf <- predict(
  glm(rf ~ death_registration*death_certification, reporting_comp %>% filter(sero_informed), family = "binomial"),
  type = "response", newdata = reporting_comp)

reporting_comp$glm_rf2 <- predict(
  glm(rf ~ death_registration + death_certification, reporting_comp %>% filter(sero_informed), family = "binomial"),
  type = "response", newdata = reporting_comp)

reporting_comp <- left_join(reporting_comp, demog %>% group_by(state) %>% summarise(n = sum(n)))

registration_relationship_gg <- ggplot(reporting_comp[which(reporting_comp$sero_informed),], aes(rf, (death_certification/100) * (death_registration/100))) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() + xlab("Inferred Proportion of COVID-19 deaths detected") +
  ylab("Proportion of All Deaths Registered \nand Medically Certified in 2019")
save_figs("registration_relationship", registration_relationship_gg, 6, 4, root = file.path(here::here(), "analysis/india/plots"))

reporting_fractions_inferred_gg <- ggplot(reporting_comp, aes(x = sero_informed, y = rf, label = state)) + geom_point() +
  ggrepel::geom_text_repel(max.overlaps = 100, point.padding = 0.1,min.segment.length = 0.1) +
  scale_y_log10(labels=scales::percent, breaks = c(0.01,0.03,0.1,0.3,0.6)) +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line()) +
  xlab("Seroprevalence informed") +
  ylab("% of COVID-19 Deaths Reported")
save_figs("reporting_fractions_inferred", reporting_fractions_inferred_gg, 10, 8, root = file.path(here::here(), "analysis/india/plots"))

## --------------------------------
## Checks for excess
## --------------------------------

excess <- read.csv("analysis/data/raw/state_mort_month.csv")
haryana <- read.csv("https://raw.githubusercontent.com/statsofindia/india-mortality/master/Haryana.csv")
excess <- data.table::rbindlist(
  list(
  excess,
  haryana %>%
    mutate(state = "Haryana") %>%
    mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
    select(state, year, month, deaths)
  ), fill = TRUE
) %>% as.data.frame()

excess <- excess %>% group_by(state, month) %>%
  mutate(baseline = mean(deaths[year < 2020])) %>%
  mutate(excess = deaths - baseline) %>%
  filter(year > 2019)

neg_to_zero <- function(x){x[x<0] <- 0; x}
excess <- excess %>% group_by(state) %>%
  mutate(month_since_2020 = month + (year-2020)*12,
         all_excess = excess,
         excess = neg_to_zero(excess)) %>%
  arrange(state, month_since_2020) %>%
  mutate(cumu_deaths = cumsum(excess)) %>%
  mutate(cumu_all_excess = cumsum(all_excess)) %>%
  mutate(date = as.Date(as.Date(paste0(year, "-",month, "-01")) + lubridate::dmonths(1)))

ind_res <- readRDS(paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))
state_excess_gg <- left_join(ind_res %>% filter(state != "Rajasthan") %>%
  filter(state %in% unique(excess$state)) %>%
  filter(r_increase == "No Change") %>%
  group_by(replicate, state) %>%
  na.omit %>%
  mutate(cumu_deaths_raw = cumsum(deaths)) %>%
  group_by(date, state) %>%
  summarise(cumu_deaths = median(cumu_deaths_raw, na.rm = TRUE),
            cumu_deaths_min = quantile(cumu_deaths_raw, 0.025, na.rm = TRUE),
            cumu_deaths_max = quantile(cumu_deaths_raw, 0.975, na.rm = TRUE)) %>%
  filter(date < max(excess$date)),
  excess[, c("state", "cumu_deaths", "date")] %>%
    rename(excess_cumu_deaths = cumu_deaths) %>%
    mutate(excess_cumu_deaths_min = NA,
           excess_cumu_deaths_max = NA)) %>%
  pivot_longer(c(cumu_deaths:excess_cumu_deaths_max)) %>%
  mutate(group = "Model Predicted") %>%
  mutate(group = replace(group, grep("excess", name), "Observed Excess Mortality")) %>%
  mutate(name = gsub("excess_", "", name)) %>%
  group_by(state) %>%
  filter(date <= max(date[group == "Observed Excess Mortality" & !is.na(value)])) %>%
  pivot_wider(values_from = value, names_from = name) %>%
  filter(!is.na(cumu_deaths)) %>%
  ggplot(aes(x = date, y = cumu_deaths, ymin = cumu_deaths_min, ymax = cumu_deaths_max, fill = group, color = group)) +
  geom_line(lwd = 1.5) +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~state, scales = "free_y", ncol = 5) +
  theme_bw()+
  ylab("Cumulative Deaths") +
  scale_color_viridis_d(name = "Source:", end = 0.8) +
  scale_fill_viridis_d(name = "Source:", end = 0.8) +
  xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_figs("state_excess", state_excess_gg+ theme(legend.position = "top"), 12, 8, root = file.path(here::here(), "analysis/plots"))

## --------------------------------
## No Vaccine Counterfactual Impact
## --------------------------------

get_counterfactual <- function(res_path, forecast = 30) {

  res <- readRDS(res_path)
  state <- res$parameters$state
  res <- generate_draws_no_vacc(res, draws = 4, burnin = 1000)
  res <- squire:::projections(res, forecast)
  date_0 <- max(res$pmcmc_results$inputs$data$date)

  inf <- nim_sq_format(res, c("infections"), date_0 = date_0) %>%
    rename(symptoms = y) %>%
    left_join(nim_sq_format(res, "S", date_0 = date_0),
              by = c("replicate", "t", "date")) %>%
    rename(S = y) %>%
    select(replicate, t, date, S, symptoms)

  deaths <- nim_sq_format(res, c("deaths"), date_0 = date_0) %>%
    rename(deaths = y) %>%
    select(replicate, t, date, deaths)

  ret <- left_join(inf, deaths)
  ret$state <- state
  ret$r_increase <- "No increase"
  return(ret)
}

reports_central <- reports_here %>% filter(dur_R == 115) %>% group_by(state) %>% filter(report_version == max(report_version))
lf <- file.path(stem, reports$report_version, "res.rds")
counterfactual_central <- pbapply::pblapply(lf, get_counterfactual)
counterfactual_res <- do.call(rbind, counterfactual_central)
saveRDS(counterfactual_res, paste0("analysis/data/derived/central_counterfactual_projections_", reports$date[1], ".rds"))

ind_res <- readRDS(paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))

vidf <- rbind(
ind_res %>% filter(r_increase == "No Change") %>% mutate(scenario = "Baseline"),
counterfactual_res %>% filter(r_increase == "No increase") %>% mutate(scenario = "No Vaccines", r_increase = "No Change")
)

max_date <- as.Date("2021-12-20")
da_gg_1 <- vidf %>% group_by(date, scenario, replicate) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(date, scenario) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  group_by(scenario) %>%
  mutate(cumu_deaths = cumsum(deaths),
         cumu_deaths_min = cumsum(deaths_min),
         cumu_deaths_max = cumsum(deaths_max)) %>%
  filter(date < max_date & date > "2021-01-01") %>%
  ggplot(aes(date, cumu_deaths, ymin = cumu_deaths_min, ymax = cumu_deaths_max, color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  xlab("") + ylab("Cumulative Deaths") +
  scale_fill_viridis_d("Scenario:", end = 0.8) +
  scale_color_viridis_d("Scenario:", end = 0.8) +
  theme(legend.position = c(0.2,0.92), legend.title = element_blank())

da_gg_2_df <- vidf %>% group_by(date, scenario, replicate) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(date, scenario) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  filter(date < max_date & date > "2021-01-01") %>%
  pivot_wider(names_from = scenario, values_from = deaths_min:deaths) %>%
  mutate(deaths_averted = `deaths_No Vaccines` - deaths_Baseline) %>%
  mutate(deaths_averted_min = `deaths_max_No Vaccines` - deaths_min_Baseline) %>%
  mutate(deaths_averted_max = `deaths_min_No Vaccines` - deaths_max_Baseline)

da_gg_2 <- da_gg_2_df %>%
  ggplot(aes(date, cumsum(deaths_averted), ymin = cumsum(deaths_averted_min), ymax = cumsum(deaths_averted_max))) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  xlab("") + ylab("Cumulative Deaths Averted") +
  scale_fill_viridis_d("Scenario:", end = 0.8) +
  scale_color_viridis_d("Scenario:", end = 0.8)

deaths_averted_gg <- cowplot::plot_grid(da_gg_1, da_gg_2, ncol = 2)
save_figs("deaths_averted", deaths_averted_gg, 10, 4)

colSums(da_gg_2_df[,c("deaths_averted", "deaths_averted_min", "deaths_averted_max")], na.rm = TRUE)


# and by state
da_gg_1 <- vidf %>% group_by(date, scenario, replicate, state) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(date, scenario, state) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  group_by(scenario, state) %>%
  mutate(cumu_deaths = cumsum(deaths),
         cumu_deaths_min = cumsum(deaths_min),
         cumu_deaths_max = cumsum(deaths_max)) %>%
  filter(date < max_date & date > "2021-01-01") %>%
  split(f = .$state) %>%
  lapply(function(x) {
  ggplot(x, aes(date, cumu_deaths, ymin = cumu_deaths_min, ymax = cumu_deaths_max, color = scenario, fill = scenario)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2021-02-01","2021-05-01","2021-08-01","2021-11-01"))) +
  xlab("") + ylab("Cumulative Deaths") +
  scale_fill_viridis_d("Scenario:", end = 0.8) +
  scale_color_viridis_d("Scenario:", end = 0.8) +
  theme(legend.position = c(0.2,0.92), legend.title = element_blank()) +
      ggtitle(x$state[1])
  })

da_gg_2_df <- vidf %>% group_by(date, scenario, replicate, state) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)) %>%
  group_by(date, scenario, state) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  filter(date < max_date & date > "2021-01-01") %>%
  pivot_wider(names_from = scenario, values_from = deaths_min:deaths) %>%
  mutate(deaths_averted = `deaths_No Vaccines` - deaths_Baseline) %>%
  mutate(deaths_averted_min = `deaths_max_No Vaccines` - deaths_min_Baseline) %>%
  mutate(deaths_averted_max = `deaths_min_No Vaccines` - deaths_max_Baseline)

da_gg_2 <- da_gg_2_df %>%
  split(.$state) %>%
  lapply(function(x) {
  ggplot(x, aes(date, cumsum(deaths_averted), ymin = cumsum(deaths_averted_min), ymax = cumsum(deaths_averted_max))) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2021-02-01","2021-05-01","2021-08-01","2021-11-01"))) +
  xlab("") + ylab("Cumulative Deaths Averted") +
  scale_fill_viridis_d("Scenario:", end = 0.8) +
  scale_color_viridis_d("Scenario:", end = 0.8)+
      ggtitle(x$state[1])
  })

posi <- c(); for(i in 1:36){posi <- c(posi, i, i+36)}
state_aversions_gg <- cowplot::plot_grid(plotlist = c(da_gg_1, da_gg_2)[posi], ncol = 2)
save_figs("state_aversions_1", cowplot::plot_grid(plotlist = c(da_gg_1, da_gg_2)[posi[1:18]], ncol = 2), width = 16, height = 22)
save_figs("state_aversions_2", cowplot::plot_grid(plotlist = c(da_gg_1, da_gg_2)[posi[19:36]], ncol = 2), width = 16, height = 22)
save_figs("state_aversions_3", cowplot::plot_grid(plotlist = c(da_gg_1, da_gg_2)[posi[37:54]], ncol = 2), width = 16, height = 22)
save_figs("state_aversions_4", cowplot::plot_grid(plotlist = c(da_gg_1, da_gg_2)[posi[55:72]], ncol = 2), width = 16, height = 22)

pos <- function(x){x[x<0] <- 0; x}
da_gg_2_df %>% na.omit %>%
  group_by(state) %>%
  mutate(across(c("deaths_averted", "deaths_averted_min", "deaths_averted_max"), .fns = sum)) %>%
filter(date == max(.$date)) %>%
  select(state, deaths_averted, deaths_averted_min, deaths_averted_max) %>%
  mutate(deaths_averted = paste0(round(deaths_averted, -1), " [", round(pos(deaths_averted_max), -1), ", ", round(deaths_averted_min, -1),"]")) %>%
  select(state, deaths_averted) %>%
  rbind(data.frame("state" = "","deaths_averted" = "")) %>%
  rbind(colSums(da_gg_2_df[,c("deaths_averted", "deaths_averted_min", "deaths_averted_max")], na.rm = TRUE) %>% t %>% as.data.frame %>%
          mutate(deaths_averted = paste0(round(deaths_averted, -1), " [", round(pos(deaths_averted_max), -1), ", ", round(deaths_averted_min, -1),"]")) %>%
          mutate(state = "National") %>% select(state, deaths_averted)) %>%
  write.csv("analysis/tables/deaths_averted.csv")

## --------------------------------
## Forward Projections - leave this to safir runs
## --------------------------------

ind_res <- readRDS(paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))

future_wave_gg <- ind_res %>% group_by(date, replicate, r_increase) %>%
  summarise(deaths = sum(deaths)) %>%
  group_by(date, r_increase) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  filter(date > "2021-07-15") %>%
  ggplot(aes(date, deaths, ymin = deaths_min, ymax = deaths_max, color = r_increase, fill = r_increase)) +
  geom_line() +
  geom_ribbon(alpha = 0.2)  +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  xlab("") + ylab("Daily Deaths") +
  scale_fill_viridis_d("Increases in R:", end = 0.8) +
  scale_color_viridis_d("Increases in R:", end = 0.8) +
  theme(legend.position = c(0.9,0.82))
# save_figs("future_wave", future_wave_gg, 8, 4, root = file.path(here::here(), "analysis/india/plots"))

future_wave_gg_states <- ind_res %>% group_by(date, state, replicate, r_increase) %>%
  summarise(deaths = sum(deaths)) %>%
  group_by(date, state, r_increase) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  filter(date > "2021-07-15") %>%
  ggplot(aes(date, deaths, ymin = deaths_min, ymax = deaths_max, color = r_increase, fill = r_increase)) +
  geom_line() +
  geom_ribbon(alpha = 0.2)  +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line(), panel.grid.major.y = element_line()) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  xlab("") + ylab("Daily Deaths") +
  scale_fill_viridis_d("Increases in R:", end = 0.8) +
  scale_color_viridis_d("Increases in R:", end = 0.8) +
  theme(legend.position = c(0.9,0.82)) +
  facet_wrap(~state, scales = "free_y")

## --------------------------------
## Extra Numbers to calculate
## --------------------------------

ind_res <- readRDS(paste0("analysis/data/derived/central_projections_", reports$date[1], ".rds"))

# cumulative deaths
ind_res %>% filter(r_increase == "No Change") %>%
  group_by(date, state) %>%
  summarise(deaths_min = quantile(deaths, 0.025, na.rm=TRUE),
            deaths_max = quantile(deaths, 0.975, na.rm=TRUE),
            deaths = median(deaths)) %>%
  group_by(date) %>%
  summarise(deaths_min = sum(deaths_min),
            deaths_max = sum(deaths_max),
            deaths = sum(deaths)) %>%
  na.omit %>%
  mutate(cumu_deaths_min = cumsum(deaths_min),
         cumu_deaths_max = cumsum(deaths_max),
         cumu_deaths = cumsum(deaths)) %>%
  filter(date == max_date)


res <- readRDS(lf[1])
inf <- left_join(ind_res %>%
                   filter(r_increase == "No Change") %>%
                   group_by(date, state, replicate) %>%
                   summarise(symptoms = sum(symptoms, na.rm = TRUE)),
                 demog %>% group_by(state) %>% summarise(n=sum(n))) %>%
  group_by(replicate, state) %>%
  mutate(sero_positive = roll_func(symptoms, res$pmcmc_results$inputs$pars_obs$sero_det),
         sero_perc = sero_positive/n) %>%
  group_by(date, state) %>%
  summarise(sero_perc_med = median(sero_perc, na.rm=TRUE),
            sero_perc_min = quantile(sero_perc, 0.025, na.rm=TRUE),
            sero_perc_max = quantile(sero_perc, 0.975, na.rm=TRUE)) %>%
  filter(date == max(date, na.rm=TRUE))

subnat_vacc <- read.csv("https://data.incovid19.org/csv/latest/cowin_vaccine_data_statewise.csv")
subnat_vacc <- subnat_vacc %>% rename(region = State) %>%
  mutate(date = as.Date(Updated.On, "%d/%m/%Y")) %>%
  mutate(date = replace(date, which(is.na(date)), as.Date(Updated.On[which(is.na(date))]))) %>%
  unique() %>%
  group_by(region, date, Updated.On) %>%
  summarise(total_vaccinations = sum(Total.Doses.Administered),
            people_vaccinated = sum(First.Dose.Administered),
            people_fully_vaccinated = sum(Second.Dose.Administered)) %>%
  rename(state = region)

# total protected in each state
left_join(
  left_join(inf, demog %>% group_by(state) %>% summarise(n=sum(n))),
  group_by(subnat_vacc, state) %>% summarise(people_vaccinated = max(people_vaccinated))
) %>%
  mutate(add_vac = people_vaccinated/n) %>%
  mutate(total_protect = sero_perc_med + (1-sero_perc_med)*add_vac) %>%
  group_by(state) %>%
  filter(date == max(date, na.rm = TRUE))

# Rt maximum per state
res <- lapply(file.path(l, "res.rds"), readRDS)
out <- lapply(res, generate_draws, 3)
rt <- lapply(out, rt_plot_immunity_vaccine)

rtd <- do.call(rbind, lapply(rt, function(x) {

  rtd <- x$rts %>% filter(date > "2021-04-01")
  rtd <- rtd[which.max(rtd$Rt_median),]
  return(rtd)
}))
rtd$state <- reports$state
ggplot(rtd, aes(Rt_median, xmin = Rt_min, xmax = Rt_max, y = fct_reorder(state, Rt_median))) + geom_errorbarh() +
  ylab("") + xlab("Maximum Delta Rt")
