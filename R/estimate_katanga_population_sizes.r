##' Estimate population size per health zone in Katanga
##'
##' @return data frame with estimated population size ('population') and population size less than 5 years old ('l5pop') per year and per health zone
##' @importFrom dplyr group_by summarise ungroup mutate if_else right_join if_else filter select arrange case_when
##' @author Sebastian Funk
##' @param under_5_multiplier factor population size is multiplied by to estimate population size under 5
estimate_population_sizes <- function(under_5_multiplier=0.169)
{
  populations <- katanga_2015_mvc %>%
    select(-year) %>%
    group_by(health_zone, phase, health_area, village) %>%
    summarise(population=unique(round(population)),
              targetpop=unique(round(targetpop)),
              vaccinated=sum(vaccinated),
              survey_coverage=unique(survey_coverage),
              min_age=min(min_age),
              max_age=max(max_age)) %>%
    ungroup() %>%
    mutate(pop_factor=case_when(
             .$max_age == 5 ~ 1 / under_5_multiplier,
             .$max_age == 10 ~ 1 / .33,
             TRUE ~ 1 / .46)) %>%
    mutate(population=if_else(!is.na(survey_coverage),
                              round(vaccinated / survey_coverage * pop_factor),
                              population),
           population=if_else(is.na(population),
                              round(targetpop * pop_factor),
                              population)) %>%
    group_by(health_zone) %>%
    summarise(population=sum(unique(population)), vaccinated=sum(vaccinated)) %>%
    ungroup() %>%
    right_join(katanga_mcv_admin %>%
               filter(!is.na(population)) %>%
               group_by(health_zone) %>%
               filter(year==min(max(year), 2015)) %>%
               select(province, health_zone, adm_population=population)) %>%
    mutate(population=if_else(is.na(population), adm_population, population)) %>%
    mutate(l5pop=population * under_5_multiplier) %>% ## estimated population under 5
    select(province, health_zone, population, l5pop) %>%
    arrange(health_zone)

  return(populations)
}
