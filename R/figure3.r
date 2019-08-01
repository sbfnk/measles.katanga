##' Plot figure 3
##'
##' @return plot
##' @importFrom dplyr %>% mutate group_by summarise ungroup left_join select if_else
##' @importFrom tidyr complete replace_na spread gather spread
##' @importFrom lubridate year
##' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_x_continuous scale_y_continuous expand_limits geom_hline theme
##' @importFrom ggrepel geom_label_repel 
##' @importFrom cowplot theme_cowplot
##' @author Sebastian Funk
##' @export
figure3 <- function()
{
    cases_annual <- katanga_measles_cases %>%
        mutate(year=year(week_start)) %>%
        group_by(health_zone, year) %>%
        summarise(cases=sum(cases)) %>%
        ungroup() %>%
        complete(health_zone, year) %>%
        replace_na(list(cases=0)) %>%
        spread(year, cases) %>%
        mutate(`201113`=`2011`+`2012`+`2013`) %>%
        gather(year, cases, 2:ncol(.)) %>%
        left_join(populations, by="health_zone") %>%
        mutate(incidence=cases/population) %>%
        select(-cases, -population, -l5pop) %>%
        spread(year, incidence) %>%
        mutate(size=if_else(`201113`>0.015, "previous_big", "smallish"),
           size=if_else(`2015`>0.005, "later_big", size),
           size=factor(size, levels=c("later_big", "previous_big", "smallish")),
           label_health_zone=if_else(size == "smallish", "", health_zone))

    p <- ggplot(cases_annual, aes(x=`201113`*1000, y=`2015`*1000,
                                  label=label_health_zone))+
        geom_label_repel(label.padding = 0.15, label.size=NA,
                         segment.color="darkgrey") +
        geom_point(aes(color=size)) +
        scale_color_manual(values=c("black", "red", "darkgrey")) +
        scale_x_continuous("Incidence in 2011-13 (per 1000)") +
        scale_y_continuous("Incidence in 2015 (per 1000)") +
        expand_limits(y=-1) +
        geom_hline(yintercept=0, linetype="dashed")+
        theme_cowplot() +
        theme(legend.position="none")

    return(p)
}
