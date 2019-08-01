##' Fit a dynamic model to the measles outbreak in Katanga
##'
##' Uses the LibBi interface provided by the rbi package
##' @param model a 'bi_model'
##' @param year the year to which to fit
##' @param end how many weeks to retain after the last data point
##' @param start_threshold when this number is exceeded over a two-week period,
##'     start fitting
##' @param vaccine_delay delay in vaccine protection; default: 0 (instantaneous protection)
##' @param nbdata number of data points to require; if fewer data points are
##'     available in a health zone, the health zone will be discarded
##' @param particles number of particles, if given; otherwise will be determined
##'     from the variance of the log-likelihood estimate
##' @param under_5_multiplier multiplier for population size to get under-5
##'     population size; default: 0.169
##' @return libbi object containing the posterior samples
##' @author Sebastian Funk
##' @importFrom dplyr %>% filter between group_by summarise ungroup n left_join
##' @importFrom tidyr replace_na
##' @importFrom lubridate wday
##' @importFrom rbi libbi sample sample_obs fix
##' @importFrom rbi.helpers adapt_proposal adapt_particles
##' @importFrom gpuR detectGPUs listContexts gpuInfo
##' @export
fit_dynamic_model <- function(model, year=2015, end=5, start_threshold=10, vaccine_delay=0, nbdata, particles, under_5_multiplier=.169)
{

    range <- as.Date(paste(year, c("01-01", "12-31"), sep="-"))

    ## filter cases to lie within given range
    cases_age_range <- katanga_measles_cases %>%
        filter(between(week_start, range[1], range[2]))

    ## get cases from relevant age group
    cases_range_health_zone <- cases_age_range %>%
        dplyr::filter(age == "1-4")

    ## create data table of times, with debutsem, week and biweek
    weeks <- expand.grid(week_start=seq(min(cases_range_health_zone$week_start),
                                        max(cases_range_health_zone$week_start),
                                        "1 week"),
                         health_zone=unique(cases_range_health_zone$health_zone),
                         stringsAsFactors = FALSE) %>%
        mutate(week=as.integer(week_start-min(week_start))/7+1,
               time=as.integer((week + 1) %/% 2))

    cases_range_health_zone %<>%
        left_join(weeks, by=c("health_zone", "week_start")) %>%
        group_by(time, health_zone) %>%
        summarise(week_start=min(week_start), cases=sum(cases), n=n()) %>%
        ungroup

    timestep_length <- 14

    ## retain health zones with any week with cases above start threshold
    cases_range_health_zone %<>%
        mutate(more_than_threshold=cases > start_threshold) %>%
        group_by(health_zone) %>%
        dplyr::filter(any(more_than_threshold)) %>%
        ungroup

    ## function to find consecutive TRUEs
    consecutive <- function(data) {
        lengths <- rle(data)$lengths
        return(data*unlist(lapply(lengths, function(x) rep(x, x))))
    }

    ## get the first time of the longest series of consecutive cases above the
    ## stating threshold, in each health zone
    min_times <- cases_range_health_zone %>%
        group_by(health_zone) %>%
        mutate(consec_above_threshold=consecutive(more_than_threshold)) %>%
        dplyr::filter(consec_above_threshold==max(consec_above_threshold)) %>%
        summarise(min_time=min(time),
                  consec_above_threshold=unique(consec_above_threshold))

    ## only retain times at or later than the minimum time found in the last
    ## command and with at least 2 data points above the threshold in a row
    cases_range_health_zone %<>%
        left_join(min_times, by="health_zone") %>%
        filter(time >= min_time, consec_above_threshold > 1) %>%
        select(time, value=cases, health_zone=health_zone)

    if (length(nb_data) > 0) {
        cases_range_health_zone %>%
            group_by(health_zone) %>%
            summarise(n=n()) %>%
            dplyr::filter(n >= nb_data) %>%
            .$health_zone -> threshold_health_zone
        cases_range_health_zone %<>%
            dplyr::filter(health_zone %in% threshold_health_zone)
    }

    ## update timings
    weeks %<>%
        left_join(min_times, by="health_zone") %>%
        dplyr::filter(time >= min_time)

    threshold_health_zone <- unique(weeks$health_zone)

    ## get mass vaccination campaigns in year(s) of given range, clean dates, set
    ## debutsem to Monday of week
    mvc_range <- katanga_2015_mvc %>%
        filter(year %in% year(range)) %>%
        mutate(week_start=date-(wday(date)-2) %% 7)

    ## filter MVCs to those in a health zone above the threshold, filter to
    ## campaigns less than a year long
    mvc_range %<>%
        filter(health_zone %in% threshold_health_zone) %>%
        filter(max_date-min_date<364) %>%
        filter(min_age < 5) %>%
        replace_na(list(vaccinated=0)) %>%
        mutate(vaccinated=case_when(
                   .$max_age == 10 ~ round(vaccinated * under_5_multiplier / .33),
                   .$max_age == 15 ~ round(vaccinated * under_5_multiplier / .46),
                   TRUE ~ vaccinated))

    ## fill campaign dates for those without daily data
    if (any(is.na(mvc_range$week_start))) {
        vacc <-
            apply(mvc_range %>% dplyr::filter(is.na(week_start)), 1, function(x) {
            population <- as.numeric(x[["population"]])
            targetpop <- as.numeric(x[["targetpop"]])
            year <- as.integer(x[["year"]])
            survey_coverage <- as.numeric(x[["survey_coverage"]])
            min_date <- as.Date(x[["min_date"]])
            max_date <- as.Date(x[["max_date"]])
            min_age <- as.numeric(x[["min_age"]])
            max_age <- as.numeric(x[["max_age"]])
            min_sem <- min_date - (wday(min_date) - 2) %% 7
            max_sem <- max_date - (wday(max_date) - 2) %% 7
            weeks <- seq(min_sem, max_sem, "1 week")
            days_within_weeks <- vapply(weeks, function(y) {
                min_time_diff <- min(as.integer(y - min_date), 0)
                max_time_diff <- min(as.integer(max_date - (y + 6)), 0)
                return(as.integer(7 + min_time_diff + max_time_diff))
            }, as.integer(0))
            ret <-
                data.frame(province=x[["province"]],
                           health_zone=x[["health_zone"]],
                           health_area=x[["health_area"]],
                           village=x[["village"]],
                           population=population,
                           targetpop=targetpop,
                           year=year,
                           doses=x[["doses"]],
                           vaccinated=round(as.integer(x[["vaccinated"]]) *
                                            days_within_weeks /
                                            sum(days_within_weeks)),
                           survey_coverage=survey_coverage,
                           min_date=min_date,
                           max_date=max_date,
                           min_age=min_age,
                           max_age=max_age,
                           week_start=weeks,
                           stringsAsFactors=FALSE)
            return(ret)
        })
        mvc_range %<>% bind_rows(vacc)
    }

    ## clean MVC data
    mvc_range %<>%
        filter(!is.na(week_start)) %>% ## these have just been fixed
        left_join(weeks, by=c("health_zone", "week_start")) %>%
        filter(!is.na(time)) %>%
        select(-week_start, -time) %>%
        left_join(weeks %>% select(week_start, health_zone, time, week),
                  by=c("health_zone", "week")) %>%
        mutate(time=time+1) %>%
        group_by(date, min_date, max_date, time, health_zone) %>%
        summarise(vaccinated=sum(vaccinated),
                  week_start=min(week_start),
                  min_age=min(min_age),
                  max_age=max(max_age)) %>%
        ungroup

    ## expand ranges to daily vaccinees
    if (any(is.na(mvc_range$date))) {
        daily_vacc <-
            apply(mvc_range %>% dplyr::filter(is.na(date)), 1, function(x) {
            week_start <- as.Date(x[["week_start"]])
            min_date <- max(as.Date(x[["min_date"]]), week_start)
            max_date <- min(as.Date(x[["max_date"]]), week_start+timestep_length-1)
            min_age <- as.numeric(x[["min_age"]])
            max_age <- as.numeric(x[["max_age"]])
            vaccinated <- as.integer(x[["vaccinated"]])
            dates <- seq(min_date, max_date, by="1 day")
            ret <-
                data.frame(date=dates,
                           week_start=week_start,
                           vaccinated=round(vaccinated / length(dates)),
                           min_date=min_date,
                           max_date=max_date,
                           min_age=min_age,
                           max_age=max_age,
                           time=as.integer(x[["time"]]),
                           health_zone=x[["health_zone"]],
                           stringsAsFactors = FALSE)
            return(ret)
            })
        mvc_range %<>% bind_rows(daily_vacc) %>%
            dplyr::filter(!is.na(date))
    }

    ## estimated number of vaccinees before/after exposure (assuming uniform
    ## exposure within each time interval)
    mvc_range %<>%
        mutate(days_after=as.integer(week_start+timestep_length-1-date),
               timing_factor=days_after/timestep_length,
               pre_exposure=round(timing_factor*vaccinated),
               post_exposure=vaccinated-pre_exposure) %>%
        group_by(health_zone, time) %>%
        summarise(pre_exposure=sum(pre_exposure),
                  post_exposure=sum(post_exposure),
                  min_date=min(min_date),
                  max_date=max(max_date),
                  min_age=min(min_age),
                  max_age=max(max_age),
                  week_start=min(week_start)) %>%
        ungroup

    ## require three data points after and three before campaign
    mvc_range %>%
        dplyr::filter(max_date < as.Date("2016-01-01")-3*timestep_length) %>%
        group_by(health_zone) %>%
        summarise(min_mvc=min(time)) %>%
        left_join(min_times, by="health_zone") %>%
        dplyr::filter(min_mvc-min_time >= 3) %>%
        .$health_zone -> threshold_health_zone

    cases_range_sum_health_zone <- cases_range_health_zone %>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        group_by(health_zone) %>%
        summarise(cases=sum(value)) %>%
        ungroup %>%
        arrange(-cases)

    threshold_health_zone <- cases_range_sum_health_zone$health_zone

    message("Fitting ", paste(threshold_health_zone, collapse=", "))

    cases_range_health_zone %<>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(health_zone=factor(health_zone),
               time=time+1) %>% ## observations are at the end of a (bi-)week
        arrange(time, health_zone)

    weeks %<>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(health_zone=factor(health_zone))

    mvc_range %<>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(health_zone=factor(health_zone))

    input_pre_mvc <- mvc_range %>%
        dplyr::select(time, value=pre_exposure, health_zone) %>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        arrange(health_zone) %>% 
        mutate(health_zone=
                   factor(health_zone, levels(cases_range_health_zone$health_zone))) %>%
        group_by(health_zone) %>%
        do(rbind(., tail(., n=1) %>% mutate(time=time+1, value=0))) %>%
        arrange(time, health_zone)

    input_post_mvc <- mvc_range %>%
        dplyr::select(time, value=post_exposure, health_zone) %>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(health_zone=factor(health_zone,
                                  levels(cases_range_health_zone$health_zone))) %>%
        group_by(health_zone) %>%
        do(rbind(., tail(., n=1) %>% mutate(time=time+1, value=0))) %>%
        arrange(time, health_zone)

    N <- populations %>% 
        dplyr::filter(health_zone %in% threshold_health_zone) %>% 
        mutate(value=round(l5pop)) %>%
        dplyr::select(health_zone, value) %>% 
        mutate(health_zone=
                   factor(health_zone,
                          levels=levels(cases_range_health_zone$health_zone)))

    min_times %<>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(value=min_time) %>%
        dplyr::select(health_zone, value)

    mvc_times_health_zone <-
        expand.grid(time=unique(input_pre_mvc$time),
                    health_zone=unique(cases_range_health_zone$health_zone)) %>%
        arrange(time, health_zone)

    input_pre_mvc %<>%
        right_join(mvc_times_health_zone, by=c("time", "health_zone")) %>%
        replace_na(list(value=0)) %>%
        mutate(time=time+vaccine_delay)

    input_post_mvc %<>%
        right_join(mvc_times_health_zone, by=c("time", "health_zone")) %>%
        replace_na(list(value=0)) %>%
        mutate(time=time+vaccine_delay)

    total_coverage <- katanga_mcv_admin %>%
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        dplyr::filter(year<2015) %>%
        group_by(health_zone) %>%
        summarise(target=sum(target), doses=sum(doses)) %>%
        mutate(coverage=doses/target)
    
    est_vacc <- dhs_coverage_estimates %>% 
        dplyr::filter(health_zone %in% threshold_health_zone) %>%
        mutate(zs_zone=factor(health_zone)) %>%
        select(health_zone, value=coverage_estimate)

    min_dates <- weeks %>%
        group_by(health_zone) %>%
        summarise(min_date=min(week_start)-min(time)*7*(1+!ode))

    model %<>%
        fix(n_health_zone=length(threshold_health_zone))

    input <- list(pre_mvc=input_pre_mvc, post_mvc=input_post_mvc,
                  N=N, est_vacc=est_vacc,
                  start_data=min_times)

    message(base::date(), " starting.")

    libbi_options <- list(model, nsamples=8192,
                          assert=opts[["debug"]], single=TRUE,
                          input=input,
                          obs=list(Incidence=cases_range_health_zone),
                          end_time=max(cases_range_health_zone$time),
                          noutputs=max(cases_range_health_zone$time),
                          verbose=opts[["verbose"]])

    ngpu <- detectGPUs()
    if (ngpu > 0) {
        context <- listContexts() %>%
            dplyr::filter(device_type=="gpu") %>% .$context %>% min
        gpu <- gpuInfo(context_idx=context)
        if (grepl("NVIDIA", gpu$deviceVendor)) {
            libbi_options <-
                c(libbi_options,
                  list(cuda=TRUE, gpu_cache=TRUE, nthreads=12,
                       cuda_fast_math=TRUE,
                       openmp=FALSE))
        }
    }

    bi <- do.call(libbi, libbi_options)

    if (missing(particles))
    {
        if (deterministic) {
            min_particles <- 1
        } else {
            min_particles <- max(ceiling(log2(nrow(cases_range_health_zone))), 256)
        }
        bi$options$nparticles <- min_particles
    } else {
        bi$options$nparticles <- particles
    }

    message(base::date(), " test run")
    bi %<>% sample(proposal="prior")

    if (missing(particles))
    {
        bi %<>% adapt_proposal(min=0.1, max=0.4, adapt="size")
        bi %<>% adapt_particles(max=2**20)
    }

    bi %<>% adapt_proposal(min=0.1, max=0.3, adapt="size")

    if (missing(particles)) {
        nparticles <- bi$options$nparticles
        bi %<>% adapt_particles(max=2**20)
        adapt_proposal(min=0.1, max=0.3, adapt="size")
    }


    bi <- sample(bi, nsamples=262144, thin=64)
    bi_obs <- sample_obs(bi)

    bi_obs$supplement <-
        list(min_date=min(weeks$debutsem)- min(weeks$time)*14,
             mvc=mvc_range, timestep=paste("2 week"),
             vaccine_delay=vaccine_delay)

    return(bi_obs)
}

