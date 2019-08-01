##' Plot figure 2
##'
##' @return plot
##' @importFrom dplyr group_by summarise ungroup filter mutate %>%
##' @importFrom ggplot2 ggplot aes facet_wrap geom_bar scale_y_continuous theme element_text scale_fill_brewer scale_color_brewer scale_x_datetime
##' @importFrom scales comma
##' @importFrom cowplot theme_cowplot
##' @author Sebastian Funk
##' @export
figure2 <- function()
{
    cases_age <- katanga_measles_cases %>%
        group_by(week_start, province, age) %>%
        summarise(cases=sum(cases, na.rm=TRUE),
                  deaths=sum(deaths, na.rm=TRUE))

    cases_age_sum <- cases_age %>%
        filter(week_start >= "2010-01-01" & week_start <  "2016-01-01") %>%
        mutate(age=factor(age, levels=c("1-4", "5+"))) %>%
        group_by(week_start, province, age) %>%
        summarise(cases=sum(cases, na.rm=TRUE)) %>%
        ungroup

    p <- ggplot(cases_age_sum, aes(x=week_start, y=cases, color=age, fill=age)) +
        facet_wrap(~province) +
        geom_bar(stat="identity", position="stack") +
        scale_y_continuous("Weekly incidence", labels=comma)+
        theme_cowplot() +
        theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="top") +
        scale_fill_brewer("Age", palette="Dark2") +
        scale_color_brewer("Age", palette="Dark2") +
        xlab("")

    return(p)
}
