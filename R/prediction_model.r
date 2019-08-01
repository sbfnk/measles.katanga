##' Prediction model for outbreaks in 2015
##'
##' @return glm object
##' @author Sebastian Funk
##' @param campaigns_2015 whether to include campaigns in 2015 as a variable
##' @importFrom dplyr select
##' @importFrom MASS glm.nb
##' @export
prediction_model <- function(provinces=FALSE, campaigns_2015=FALSE)
{
  df_201113 <- katanga_measles_cases %>%
    filter(between(year(week_start), 2011, 2013)) %>%
    group_by(health_zone) %>%
    summarise(cases_201113=sum(cases))

  df <- gather_2015_data() %>%
    left_join(df_201113, by="health_zone") %>%
    filter(!is.na(mean_coverage))

  df <- df %>%
    mutate(population=(population-mean(population))/sd(population),
           cases_201113=(cases_201113-mean(cases_201113))/sd(cases_201113),
           mean_coverage=(mean_coverage-mean(mean_coverage))/sd(mean_coverage),
           coverage_estimate=(coverage_estimate-mean(coverage_estimate, na.rm=TRUE))/sd(coverage_estimate, na.rm=TRUE))

  variables <- c("population", "cases_201113", "mean_coverage", "coverage_estimate")

  if (campaigns_2015) variables <- c(variables, "campaign")
  if (provinces) variables <- c(variables, "province")

  formula <- paste0("cases ~ ", paste(variables, collapse="+"))

  g <- glm.nb(as.formula(formula), data=df)

  return(g)
}
