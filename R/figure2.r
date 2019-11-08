##' Plot figure 2
##'
##' @return plot
##' @importFrom dplyr group_by summarise ungroup filter mutate %>%
##' @importFrom ggplot2 ggplot aes facet_wrap geom_bar scale_y_continuous theme element_text scale_fill_brewer scale_color_brewer scale_x_datetime
##' @importFrom scales comma
##' @importFrom cowplot theme_cowplot
##' @importFrom lubridate floor_date
##' @author Sebastian Funk
##' @export
figure2 <- function()
{
    cases_age <- katanga_measles_cases %>%
        mutate(month_start=floor_date(week_start, "months")) %>%
        group_by(month_start, age) %>%
        summarise(cases=sum(cases, na.rm=TRUE),
                  deaths=sum(deaths, na.rm=TRUE))

    cases_age_sum <- cases_age %>%
        filter(month_start >= "2010-01-01" & month_start <  "2016-01-01") %>%
        mutate(age=factor(age, levels=c("1-4", "5+"))) %>%
        group_by(month_start, age) %>%
        summarise(cases=sum(cases, na.rm=TRUE)) %>%
        ungroup %>%
        arrange(desc(age))

    p <- ggplot(cases_age_sum, aes(x=month_start, y=cases, color=age, fill=age)) +
        geom_bar(stat="identity", position="stack") +
        scale_y_continuous("Monthly incidence", labels=comma)+
        theme_cowplot() +
        theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="top") +
        scale_fill_brewer("Age", palette="Dark2") +
        scale_color_brewer("Age", palette="Dark2") +
        xlab("")

    return(p)
}
