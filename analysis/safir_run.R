# Vaccine pars function from global_covid_vaccine_booster
get_vaccine_pars <- function(
  vaccine = "Pfizer",
  mu_ab_d1 = 0.1133744,
  mu_ab_d2 = 0.4192800,
  vaccine_doses = 3,
  dose_3_fold_increase = 1.716146,
  ab_50 = 0.09652525,
  ab_50_severe = 0.02117124,
  std10 = 0.44,
  k = 3.21117,
  t_d2 = 28,
  t_d3 = 180,
  hl_s = 98.31237,
  hl_l = 377.5782,
  period_s = 99.90433,
  period_l = 390.1238,
  scale_d2 = 0.778,
  scale_d3 = 0.8505415
){
  mu_ab_list <- data.frame(name = vaccine,
                           mu_ab_d1 = mu_ab_d1,
                           mu_ab_d2 = mu_ab_d2) %>%
    mutate(mu_ab_d3 = mu_ab_d2 * dose_3_fold_increase,
           mu_ab_d2 = mu_ab_d2 * scale_d2,
           mu_ab_d3 = mu_ab_d3 * scale_d3)

  ab_parameters <- safir::get_vaccine_ab_titre_parameters(
    vaccine = vaccine, max_dose = vaccine_doses, correlated = TRUE,
    hl_s = hl_s, hl_l = hl_l, period_s = period_s, t_period_l = period_l,
    ab_50 = ab_50, ab_50_severe = ab_50_severe, std10 = std10, k = k,
    mu_ab_list = mu_ab_list
  )
  ab_parameters$max_ab <- 5 # max titre on natural log scale
  return(ab_parameters)
}

# vaccine strategy from global_covid_vaccine_booster
get_vaccine_strategy <-
  function(strategy,
           days_to_vacc_start,
           doses_per_day,
           time_period,
           max_coverage,
           age_groups_covered,
           age_groups_covered_d3 = NA,
           vaccine_doses,
           pop,
           vacc_per_week,
           t_d3,
           t_10y_start) {

  if (strategy == "realistic") {

    # now we want to simulate a country having vaccinated all of the eligible population with the first and second dose before switching to a booster dose
    vaccine_set <- c(rep(0, days_to_vacc_start), rep(doses_per_day, time_period - days_to_vacc_start))

    # if vaccinating children < 10 years, want to wait until 1 Jan 2022
    if (age_groups_covered >= 16){
      # how many days to vaccinate population >10 years with 2 doses?
      population_10y <- sum(pop$n[3:17]) / sum(pop$n)
      days_vacc_10y <- floor(population_10y / (vacc_per_week/7) * max_coverage * 2)
      vaccine_set <- c(rep(0, days_to_vacc_start),
                       rep(doses_per_day, days_vacc_10y),
                       rep(0, t_10y_start - days_to_vacc_start - days_vacc_10y),
                       rep(doses_per_day, time_period - t_10y_start))
    }


    vaccine_coverage_strategy <- list()

    if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered_d3,]}

    next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))

    # don't prioritise anyone for a booster until all population has received primary series
    if (vaccine_doses == 3){
      next_dose_priority[2,] <- 0
    }
  }

  if (strategy == "same_doses"){

    coverage <- sum(pop$n[(17 - age_groups_covered + 1):17])/sum(pop$n)
    days_to_vacc <- floor(coverage / (vacc_per_week/7) * max_coverage * 2)
    days_to_boost <- floor(days_to_vacc)

    if (days_to_vacc < 28) {days_to_vacc = 28}

    if (days_to_vacc + 28 >= (t_d3)) {t_d3 = days_to_vacc + 28}

    vaccine_set <- c(rep(0, days_to_vacc_start),
                     rep(doses_per_day/2, 28),
                     rep(doses_per_day, days_to_vacc - 28),
                     rep(doses_per_day/2, 28),
                     rep(0, t_d3 - days_to_vacc + 28),
                     rep(doses_per_day/2, days_to_boost),
                     rep(0, time_period - days_to_vacc_start - t_d3 - 28 -28 -  days_to_boost))

    vaccine_coverage_strategy <- list()

    if (vaccine_doses == 2) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)
    } else if (vaccine_doses == 3) {
      vaccine_coverage_strategy[[1]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[2]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]
      vaccine_coverage_strategy[[3]] <- nimue::strategy_matrix(strategy = "Elderly",max_coverage = max_coverage)[1:age_groups_covered,]}

    if (vaccine_doses == 2) {
      next_dose_priority <- matrix(data = 1, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
    } else if (vaccine_doses == 3) {
      next_dose_priority <- matrix(data = 0, nrow = vaccine_doses - 1, ncol = ncol(vaccine_coverage_strategy[[1]]))
      next_dose_priority[,(17 - age_groups_covered + 1):17] <- 1
    }
  }

  return(list(vaccine_set = vaccine_set, vaccine_coverage_strategy = vaccine_coverage_strategy, next_dose_priority = next_dose_priority, t_d3 = t_d3))

}


# Run scenario for India, adapted from global_covid_vaccine_booster
run_india_scenario <-
  function(scenario = 1,
           target_pop = 1e6,
           vaccine_doses = 2,
           vaccine = "AZ",
           dt = 0.2,
           repetition = 1,
           seeding_cases = 5,
           vfr = 4,
           vfr_time1 = "2021-11-27",
           vfr_time2 = "2021-12-31",
           variant_fold_reduction = 1,
           dose_3_fold_increase = 1,
           age_groups_covered_d3 = 14,
           vacc_per_week = 0.05,
           name = "scenario1",
           ab_model_infection = TRUE,
           std10 = 0.44,
           std10_infection = 0.44,
           t_d3 = 180,
           mu_ab_d1 = 0.1133744,
           mu_ab_d2 = 0.4192800,
           k =3.21117,
           hl_s = 98.31237,
           hl_l = 377.5782,
           period_s = 99.90433,
           period_l = 390.1238,
           ab_50 = 0.09652525,
           ab_50_severe = 0.02117124,
           scale_d2 = 0.778,
           scale_d3 = 0.8505415,
           mu_ab_infection = 3.7,
           strategy = "realistic",
           max_Rt_omicron = 7.5,
           nimue_fit = res,
           tmax_date = "2022-12-31",
           lambda_external = 0.000001,
           Rt_pre_omicron,
           R0_date_delta_start,
           vacc_per_day
           ){

    # set up transmission
    # -----------------------------------------------------------------------------
    # interpolate Rt (piecewise linear)
    # -----------------------------------------------------------------------------

    # compute level of Rt reached before Omicron switch
    R0_t0 <- Rt_pre_omicron$date[1]
    rt <- c(Rt_pre_omicron$Rt,
            rep(tail(Rt_pre_omicron$Rt,1),as.integer(as.Date(vfr_time2) - max(Rt_pre_omicron$date))),
            rep(max_Rt_omicron, as.integer(as.Date(tmax_date) - as.Date(vfr_time2))))

    rt_out <- list(
      "Rt" = rt,
      "Rt_tt" = seq_along(rt)
      )

    vacc_start <- as.Date(x = nimue_fit$interventions$date_vaccine_change[1], format = "%m/%d/%Y")
    days_to_vacc_start <- as.integer(difftime(vacc_start, Rt_pre_omicron$date[1]))

    # daily per-capita prob of external infection
    time_period <- as.integer(difftime(tmax_date, Rt_pre_omicron$date[1] - 1))
    lambda_external <- rep(lambda_external, time_period)

    # # delta pulse, spread out hazard of 0.1 over 10 days right before Delta wave
    t_spread <- 30
    lambda_tt <- as.integer(difftime(R0_date_delta_start, R0_t0 - 1))
    lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
    lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
    lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.1
    #
    # # second pulse, spread out hazard of 0.01 over 20 days right before 2nd wave
    # #t_spread <- 20
    # t_spread <- 10
    # lambda_tt <- as.integer(difftime(R0_t7, R0_t0 - 1))
    # lambda_tt <- seq(from = lambda_tt - t_spread/2, to = lambda_tt + t_spread/2, by = 1)
    # lambda_external[lambda_tt] <- dnorm(x = lambda_tt, mean = lambda_tt[t_spread/2+1], sd = 3)
    # lambda_external[lambda_tt] <- lambda_external[lambda_tt] / sum(lambda_external[lambda_tt]) * 0.01

    # BASE PARAMETERS

    # Population and mixing
    pop <- squire::get_population(country = "India")
    pop_standardise <- target_pop / sum(pop$n)
    pop$n <- as.integer(pop$n * pop_standardise)
    contact_mat <- squire::get_mixing_matrix(country = "India")

    # Hospital capacity
    hc <- squire::get_healthcare_capacity("India")
    hc$hosp_beds <- round(hc$hosp_beds * sum(pop$n) / 1000)
    hc$ICU_beds <- round(hc$ICU_beds * sum(pop$n) / 1000)

    # Poorer health outcomes for LMICs and LICs
    # not sure why this was here ^
    pnsdt <- squire:::probs$prob_non_severe_death_treatment

    # base parameters
    parameters <- safir::get_parameters(
      population = pop$n,
      contact_matrix_set = contact_mat,
      iso3c = "IND",
      R0 = rt_out$Rt,
      tt_R0 = rt_out$Rt_tt,
      time_period = time_period,
      dt = dt,
      hosp_bed_capacity = hc$hosp_beds,
      ICU_bed_capacity = hc$ICU_beds,
      prob_non_severe_death_treatment = pnsdt,
      #  dur_R = 365,
      seeding_cases = seeding_cases,
      lambda_external = lambda_external
    )

    # --------------------------------------------------------------------------------
    # get vaccine parameters
    # --------------------------------------------------------------------------------

    # doses available each day

    # Approach below attempting to get actual doses for the state fit

    # doses_per_day <- round(nimue_fit$interventions$max_vaccine * (target_pop/sum(nimue_fit$parameters$population)))
    # vaccine_set <- c(rep(0, days_to_vacc_start-1), doses_per_day, rep(tail(doses_per_day, 1), as.Date(tmax_date) - tail(nimue_fit$interventions$date_vaccine_change,1)))
    #
    # vaccine_coverage_strategy <- list(nimue_fit$parameters$vaccine_coverage_mat, nimue_fit$parameters$vaccine_coverage_mat)
    # next_dose_priority <- matrix(1, 1, 17)
    # t_d3 <- 90
    # vaccine_out <- list("vaccine_set" = vaccine_set,
    #                     "vaccine_coverage_strategy" = vaccine_coverage_strategy,
    #                     "next_dose_priority" = next_dose_priority,
    #                     "t_d3" = t_d3)


    doses_per_day <- floor(sum(pop$n) * vacc_per_day)

    vaccine_out <-
      get_vaccine_strategy(
        strategy,
        days_to_vacc_start = days_to_vacc_start,
        doses_per_day = doses_per_day,
        time_period = time_period,
        max_coverage = max(nimue_fit$parameters$vaccine_coverage_mat),
        age_groups_covered = sum(colSums(nimue_fit$parameters$vaccine_coverage_mat)>0),
        age_groups_covered_d3 = age_groups_covered_d3,
        vaccine_doses = vaccine_doses,
        pop = pop,
        vacc_per_week = vacc_per_week,
        t_d3 = t_d3,
        t_10y_start = t_10y_start
      )

    vaccine_set <- vaccine_out$vaccine_set
    vaccine_coverage_strategy <- vaccine_out$vaccine_coverage_strategy
    next_dose_priority <- vaccine_out$next_dose_priority
    t_d3 <- vaccine_out$t_d3




    # profiles and dosing
    vax_pars <- get_vaccine_pars(vaccine = vaccine,
                                 mu_ab_d1 = mu_ab_d1,
                                 mu_ab_d2 = mu_ab_d2,
                                 k = k,
                                 hl_s = hl_s,
                                 hl_l = hl_l,
                                 period_s = period_s,
                                 period_l = period_l,
                                 dose_3_fold_increase = dose_3_fold_increase,
                                 ab_50 = ab_50,
                                 ab_50_severe = ab_50_severe,
                                 scale_d2 = scale_d2,
                                 scale_d3 = scale_d3,
                                 vaccine_doses = vaccine_doses,
                                 std10 = std10,
                                 t_d3 = t_d3)

    # dosing
    if (vaccine_doses == 2) {dose_period <- c(NaN, 28)}
    if (vaccine_doses == 3) {dose_period <- c(NaN, 28, (t_d3 + 28))}

    # combine parameters and verify
    parameters <- safir::make_vaccine_parameters(
      safir_parameters = parameters,
      vaccine_ab_parameters = vax_pars,
      vaccine_set = vaccine_set,
      dose_period = dose_period,
      strategy_matrix = vaccine_coverage_strategy,
      next_dose_priority_matrix = next_dose_priority
    )

    parameters$mu_ab_infection <- mu_ab_infection


    # make VFR reduction vector and attach
    vfr_time1 <- as.Date(x = vfr_time1)
    vfr_time2 <- as.Date(x = vfr_time2)

    stopifnot(vfr_time1 < tmax_date)
    stopifnot(vfr_time2 < tmax_date)

    vfr_time1_day <- as.integer(difftime(vfr_time1, R0_t0 - 1))
    vfr_time2_day <- as.integer(difftime(vfr_time2, R0_t0 - 1))

    vfr_vector <- safir::variant_fold_reduction_vector(parameters = parameters, dt = dt, vfr = vfr, vfr_time_1 = vfr_time1_day, vfr_time_2 = vfr_time2_day)
    parameters <- safir::make_immune_parameters(parameters = parameters, vfr = vfr_vector, mu_ab_infection = mu_ab_infection, std10_infection = std10_infection)

    ######################################################
    # run the simulation
    ######################################################

    # create variables
    timesteps <- parameters$time_period/dt

    # creates the categorical states and ages for the simulated population
    variables <- safir::create_variables(pop = pop, parameters = parameters)
    variables <- safir::create_vaccine_variables(variables = variables, parameters = parameters)
    variables <- safir::create_natural_immunity_variables(variables = variables, parameters = parameters)

    # creates the list of events and attaches listeners which handle state changes and queue future events
    events <- safir::create_events(parameters = parameters)
    events <- safir::create_events_vaccination(events = events, parameters = parameters)
    safir::attach_event_listeners(variables = variables, events = events, parameters = parameters, dt = dt)
    safir::attach_event_listeners_vaccination(variables = variables,events = events,parameters = parameters,dt = dt)
    safir::attach_event_listeners_natural_immunity(variables = variables, events = events, parameters = parameters, dt = dt, additive = TRUE)

    # renderer object is made
    renderer <- individual::Render$new(parameters$time_period)
    vaxx_renderer <- individual::Render$new(parameters$time_period)
    inf_renderer <- individual::Render$new(parameters$time_period)
    incidence_renderer <- individual::Render$new(timesteps)
    safir::attach_tracking_listener_incidence(events=events, renderer = incidence_renderer)

    # processes
    processes <- list(
      safir::natural_immunity_ab_titre_process(parameters = parameters,variables = variables, dt = dt),
      safir::vaccination_process(parameters = parameters,variables = variables,events = events, dt = dt),
      safir::infection_process_vaccine_cpp(parameters = parameters,variables = variables,events = events, dt = dt),
      safir::categorical_count_renderer_process_daily(renderer = renderer, variable = variables$states, categories = variables$states$get_categories(),dt = dt),
      safir::integer_count_render_process_daily(renderer = vaxx_renderer, variable = variables$dose_num, margin = 1:4, dt = dt))

    safir::setup_events(parameters = parameters, events=events, variables = variables, dt=dt)

    safir::simulation_loop_safir(
      variables = variables,
      events = events,
      processes = processes,
      timesteps = timesteps,
      variables_dont_update = c("discrete_age", "phase"),
      progress = TRUE
    )

    df <- renderer$to_dataframe()
    df_vacc <- vaxx_renderer$to_dataframe()
    df_inc <- incidence_renderer$to_dataframe()

    df_inc <- df_inc %>%
      mutate(timestep = floor((timestep-1)*dt)) %>%
      mutate(incidence = coalesce(incidence,0))%>%
      group_by(timestep) %>%
      summarise(incidence = sum(incidence))

    df <- left_join(df, df_vacc, by = c("timestep"))
    df <- left_join(df, df_inc, by = c("timestep"))
    df_rt <- as.data.frame(rt_out) %>%
      rename("timestep" = "Rt_tt")
    df <- left_join(df, df_rt, by = c("timestep"))

    # summarise
    saf_reps_summarise <- df %>%
      mutate(IMild_count = IMild_count + IAsymp_count) %>%
      select(-IAsymp_count) %>%
      pivot_longer(cols = contains(c("count", "Rt","incidence")), names_to = "compartment") %>%

      filter(compartment %in% c("D_count", "X1_count", "X2_count", "X3_count", "R_count", "IMild_count", "ICase_count", "Rt", "incidence")) %>%
      group_by(compartment) %>%
      mutate(value = if_else(compartment == "D_count", value - lag(value), value),
             value = if_else(is.na(value), 0, value)) %>%
      ungroup() %>%
      pivot_wider(id_cols = timestep, names_from = "compartment", values_from = "value")  %>%
      mutate(deaths = sum(D_count[timestep >= days_to_vacc_start]),
             inc = sum(incidence[timestep >= days_to_vacc_start]),
             doses = X1_count + X2_count * 2 + X3_count * 3,
             total_doses = max(doses)) %>%
      ungroup() %>%
      nest(cols = c(timestep, D_count, X1_count, X2_count, X3_count, R_count, IMild_count, ICase_count, doses, Rt, incidence)) %>%
      mutate(scenario = scenario)

    # get prop in Recovered when vaccination starts
    prop_R <- df %>%
      select(timestep, R_count) %>%
      filter(timestep == days_to_vacc_start) %>%
      mutate(prop_R = round(R_count/target_pop * 100,2)) %>%
      select(prop_R) %>%
      mutate(scenario = scenario)

    saf_reps_summarise <- left_join(saf_reps_summarise, prop_R, by = "scenario")

    # Save output
    # output_address <- paste0("raw_outputs/output_", name, "/scenario_", scenario, ".rds")
    # dir.create(dirname(output_address), recursive = TRUE, showWarnings = FALSE)
    # saveRDS(saf_reps_summarise, output_address)
    return(saf_reps_summarise)
  }

# ---------------------------------
## CREATE RUN FOR INDIA
# ---------------------------------

library(tidyverse)
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

# Central fits
reports <- reports_here %>% filter(dur_R == 115)
lf <- file.path(stem, reports$report_version, "res.rds")

# Get the results for Chhattisgarh, typical of India as a whole
res <- readRDS(lf[which(reports$state == "Chhattisgarh")])

# get rt trend for this state
rt_df_res <- rt_df(res)

# get vaccination data for India as a whole
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
subnat_vacc <- subnat_vacc[subnat_vacc$state == "India", ]

# vaccines per day approximately for India
vacc_per_day <- round(lm(total_vaccinations~0+seq_along(date), subnat_vacc)$coefficients[1])
vacc_per_day <- vacc_per_day / sum(squire::get_population("India")$n)


# Now run the scenario
target_pop = 1e6
out <- run_india_scenario(Rt_pre_omicron = rt_df_res, lambda_external = 1e-6, R0_date_delta_start = "2021-02-01", vacc_per_day = vacc_per_day, target_pop = target_pop)
ggplot(out$cols[[1]], aes(timestep+rt_df_res$date[1], cumsum(incidence/target_pop))) +
  geom_line() + xlim(as.Date(c("2020-04-01", "2022-12-01")))
