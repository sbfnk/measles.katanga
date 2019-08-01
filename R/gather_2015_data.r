##' Gather 2015 data for cases, coverage and campaigns
##'
##' @return data frame
##' @author Sebastian Funk
##' @importFrom dplyr filter group_by summarise ungroup select mutate summarise ungroup right_join left_join
##' @importFrom tidyr replace_na
##' @importFrom lubridate year
##' @keywords internal
gather_2015_data <- function()
{
  cases_2015 <- katanga_measles_cases %>%
    filter(year(week_start)==2015) %>%
    group_by(health_zone) %>%
    summarise(cases=sum(cases)) %>%
    ungroup() %>%
    select(health_zone, cases)

  coverage_2015 <- katanga_mcv_admin %>%
    filter(between(year, 2010, 2015)) %>%
    mutate(coverage=doses/target) %>%
    group_by(health_zone) %>%
    summarise(mean_coverage=mean(coverage, na.rm=TRUE)) %>%
    ungroup() %>%
    select(health_zone, mean_coverage)

  campaigns_2015 <- katanga_2015_mvc %>%
    group_by(health_zone) %>%
    summarise(campaign="yes") %>%
    ungroup() %>%
    right_join(populations %>% select(health_zone), by="health_zone") %>%
    replace_na(list(campaign="no")) %>%
    mutate(campaign=factor(campaign, levels=c("no", "yes")))

  df <- cases_2015 %>%
    right_join(coverage_2015, by="health_zone") %>%
    right_join(dhs_coverage_estimates, by="health_zone") %>%
    filter(!is.na(mean_coverage)) %>%
    filter(!is.na(coverage_estimate)) %>%
    right_join(campaigns_2015, by="health_zone") %>%
    left_join(populations, by="health_zone") %>%
    replace_na(list(cases=0)) %>%
    mutate(incidence=cases/population)

  return(df)
}
