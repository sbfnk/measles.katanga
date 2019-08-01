##' Remove mass vaccination campaigns from a LibBi model fit
##'
##' @param posterior a \code{libbi} object containing posterior samples
##' @return a \code{libbi} object with updated posterior predictive samples, with all mass vaccination campaigns removed
##' @author Sebastian Funk
##' @importFrom rbi bi_read attach_data predict sample_obs
##' @importFrom dplyr %>% group_by mutate filter summarise ungroup select arrange inner_join
##' @export
remove_mvc <- function(posterior, ...)
{
    vaccine_delay <- posterior$supplement$vaccine_delay
    input <- bi_read(posterior, file="input")
    end_time <- posterior$options$`end-time`

    input$pre_mvc %>%
        group_by(health_zone) %>%
        mutate(len=sum(value>0)) %>%
        .$len %>%
        max -> max_length

    mvc_start <-
        input$pre_mvc %>%
        group_by(health_zone) %>%
        dplyr::filter(value > 0) %>%
        dplyr::summarise(time=min(time)) %>%
        ungroup %>%
        rbind(input$pre_mvc %>%
              group_by(health_zone) %>%
              summarise(value=sum(value)) %>%
              filter(value == 0) %>%
              mutate(time=end_time-1) %>%
              select(health_zone, time)) %>%
        arrange(health_zone)

    input_no_vacc <- input

    input_no_vacc$pre_mvc <- NULL
    input_no_vacc$post_mvc <- NULL

    bi_output <- bi_read(posterior)

    new_output <- bi_output
    new_output$initI <- new_output$I %>%
        inner_join(mvc_start, by=c("health_zone", "time")) %>%
        select(-time) %>%
        arrange(np, health_zone)
    new_output$S %<>%
        inner_join(mvc_start, by=c("health_zone", "time")) %>%
        select(-time) %>%
        arrange(np, health_zone)

    posterior <- attach_data(posterior, "output", new_output)

    input_no_vacc$start_data <- mvc_start %>%
        mutate(value=time) %>%
        select(-time)

    ##:ess-bp-start::browser@nil:##
browser(expr=is.null(.ESSBP.[["@22@"]]));##:ess-bp-end:##
    pred_no_mvc <-
        predict(posterior, input=input_no_vacc, ...) %>%
        sample_obs()

    return(pred_no_mvc)
}
