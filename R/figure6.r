##' Plot figure 6
##'
##' @return plot
##' @param posterior a \code{libbi} object containing posterior samples
##' @param posterior_no_mvc a \code{libbi} object containing posterior predictions without a mass-vaccination campaign
##' @importFrom rbi bi_read flatten
##' @importFrom rbi.helpers numeric_to_time
##' @importFrom dplyr %>% select rename mutate bind_rows left_join inner_join group_by summarise arrange
##' @importFrom tibble enframe
##' @importFrom tidyr unnest spread
##' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_point geom_rect facet_wrap xlab ylab scale_color_manual scale_fill_manual theme
##' @importFrom cowplot theme_cowplot
##' @importFrom magrittr add %<>%
##' @export
##' @author Sebastian Funk
figure6 <- function(posterior, posterior_no_mvc)
{

    min_date <- posterior$supplement$min_date

    ## identify contiguous periods of mass-vaccination campaigns
    mvc <- posterior$supplement$mvc %>%
        select(-time) %>%
        rename(time=debutsem) %>%
        mutate(type="MVC", value=0)

    mvc_contiguous <- list()

    for (mvc_health_zone in unique(mvc$health_zone))
    {
        mvc_dates <- apply(mvc %>% dplyr::filter(health_zone == mvc_health_zone), 1, function(x)
        {
            seq(as.Date(x[["min_date"]]),as.Date(x[["max_date"]]), by="day") %>%
                as.character()
        }) %>%
            unlist() %>%
            as.Date() %>%
            unique() %>%
            sort()

        mvc_date_diffs <- mvc_dates %>%
            diff() %>%
            as.integer()
        mvc_date_rle <- mvc_date_diffs %>%
            rle()

        mvc_date_cl <- mvc_date_rle$lengths %>%
            cumsum() %>%
            add(1) %>%
            c(1, .)

        contiguous <- which(mvc_date_rle$values==1)

        contiguous_mvc <- lapply(contiguous, function(x) {
            c(min_date=mvc_dates[mvc_date_cl[x]],
              max_date=mvc_dates[mvc_date_cl[x+1]])
        })

        min_dates <- vapply(contiguous_mvc, function(x) {
            as.character(x[["min_date"]])
        }, "") %>%
            as.Date()
        max_dates <- vapply(contiguous_mvc, function(x) {
            as.character(x[["max_date"]])
        }, "") %>%
            as.Date()
        
        mvc_contiguous[[mvc_health_zone]] <-
            data.frame(min_date=min_dates, max_date=max_dates, health_zone=mvc_health_zone,
                       stringsAsFactors = FALSE)
    }

    mvc_contiguous %<>%
        bind_rows()

    obs <- bi_read(posterior, file="obs") %>%
        numeric_to_time(origin=min_date, unit="2 weeks") %>%
        flatten()

    res <- bi_read(posterior, type=c("state", "obs", "noise")) %>%
        numeric_to_time(origin=min_date, unit="2 weeks") %>%
        flatten()

    cases_summary <- res %>%
        group_by(health_zone, time, var) %>%
        summarise(value =
                      list(enframe(my_quantile(value,
                                               probs=c(0.05, 0.25, 0.5, 0.75, 0.95))))) %>%
        unnest %>%
        spread(name, value) %>%
        arrange(health_zone, var, time) %>%
        ungroup

    incidence <- cases_summary %>%
        inner_join(obs %>% rename(data=value), by=c("health_zone", "time", "var")) %>%
        mutate(type="With MVC",
               time=time-7) %>%
        filter(time < "2016-01-01")

    inp_no_mvc <- bi_read(posterior_no_mvc, file="input")
    inp_no_mvc$start_data %<>% rename(time=value)
    inp_no_mvc %<>%
        numeric_to_time(origin=min_date, unit="2 weeks")

    res_no_mvc <- bi_read(posterior_no_mvc, type=c("state", "obs", "noise")) %>%
        numeric_to_time(origin=min_date, unit="2 weeks") %>%
        flatten()

    cases_no_mvc_summary <- res_no_mvc %>%
        group_by(health_zone, time, var) %>%
        summarise(value =
                      list(enframe(my_quantile(value,
                                               probs=c(0.05, 0.25, 0.5,
                                                       0.75, 0.95))))) %>%
        unnest %>%
        spread(name, value) %>%
        arrange(health_zone, var, time) %>%
        ungroup

    incidence_no_mvc <- cases_no_mvc_summary %>%
        left_join(inp_no_mvc$start_data %>% rename(start_time=time),
                  by="health_zone") %>%
        filter(time >= start_time) %>%
        select(-start_time) %>%
        inner_join(obs %>% select(health_zone, var) %>% unique,
                   by=c("health_zone", "var")) %>%
        left_join(obs %>% rename(data=value), by=c("health_zone", "time", "var")) %>%
        mutate(type="Without MVC",
               time=time-7) %>%
        filter(time < "2016-01-01")

    incidence_all <- rbind(incidence, incidence_no_mvc)

    p <- ggplot(incidence_all) +
        geom_line(aes(x=time, y=`50%`, group=type, color=type)) +
        geom_ribbon(aes(x=time, ymin=`25%`, ymax=`75%`, group=type, fill=type),
                    alpha=0.5) +
        geom_ribbon(aes(x=time, ymin=`5%`, ymax=`95%`, group=type, fill=type),
                    alpha=0.25) +
        geom_point(aes(x=time, y=data)) +
        geom_rect(data=mvc_contiguous,
                  mapping=aes(xmin=min_date, xmax=max_date),
                  ymin=0, ymax=max(incidence_all$`95%`),
                  alpha=0.3, fill="blue") +
        facet_wrap(~health_zone, scales="free_y") +
        xlab("") +
        ylab("Observed Incidence") +
        scale_color_manual("", values=c("black", "red")) +
        scale_fill_manual("", values=c("black", "red")) +
        theme_cowplot() +
        theme(legend.position="top",
              axis.text.x=element_text(angle=45, hjust=1))

    return(p)
}
