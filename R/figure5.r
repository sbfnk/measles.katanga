##' Plot figure 4
##'
##' @return plot
##' @importFrom dplyr %>% filter mutate
##' @importFrom lubridate year
##' @importFrom ggplot2 ggplot aes geom_abline geom_point scale_color_manual scale_x_continuous scale_y_continuous expand_limits geom_hline theme
##' @importFrom ggrepel geom_label_repel
##' @importFrom cowplot theme_cowplot
##' @author Sebastian Funk
##' @export
##' @param m a prediction model of class \code{glm}, such as returned by \code{\link{prediction_model}}
figure5 <- function(m)
{
    df <- gather_2015_data() %>%
        mutate(incidence=cases/population) %>%
        filter(!is.na(mean_coverage))

    df$hat <- predict(m, type="response")

    df <- df %>%
        mutate(size=if_else(hat > 2000, "real_big", "small"),
               size=if_else(cases > 2000, "model_big", size),
               size=factor(size, levels=c("model_big", "real_big", "small")),
               label_zs=if_else(size == "small", "", health_zone))

    p <- ggplot(df, aes(x=hat, cases, label=label_zs))+
        geom_abline(slope=1, intercept=0) +
        geom_label_repel(label.padding = 0.25, label.size=NA, segment.color="darkgrey") +
        geom_point(aes(color=size)) +
        scale_color_manual(values=c("black", "red", "darkgrey")) +
        scale_x_continuous("Predicted number of cases in 2015") +
        scale_y_continuous("Number of cases in 2015") +
        expand_limits(y=-1000) +
        geom_hline(yintercept=0, linetype="dashed")+
        theme_cowplot() +
        theme(legend.position="none")

    return(p)
}
