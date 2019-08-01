##' Plot figure 4
##'
##' @return plot
##' @importFrom dplyr filter group_by summarise ungroup left_join mutate select
##' @importFrom lubridate year
##' @importFrom ggplot2 ggplot aes geom_jitter geom_smooth xlab ylab
##' @importFrom cowplot theme_cowplot plot_grid
##' @author Sebastian Funk
##' @export
figure4 <- function()
{
    df <- gather_2015_data() %>%
        mutate(incidence=cases/population)

    p1 <- ggplot(df, aes(x=mean_coverage, incidence * 1000)) +
        geom_jitter() +
        geom_smooth(method="lm") +
        xlab("EPI vaccination coverage 2010-15") +
        ylab("Incidence in 2015 (per 1000)") +
        theme_cowplot()

    p2 <- ggplot(df, aes(x=coverage_estimate, incidence * 1000)) +
        geom_jitter() +
        geom_smooth(method="lm") +
        xlab("Estimated vaccination coverage") +
        ylab("Incidence in 2015 (per 1000)") + 
        theme_cowplot()

    pg <- plot_grid(p1, p2, labels=c("A", "B"), ncol=2)

    return(pg)
}
