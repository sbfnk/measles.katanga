##' Plot figure 1
##'
##' Shapefiles can be obtained from
##' https://data.humdata.org/dataset/dr-congo-health-0. Download
##' RDC_Zone_de_sante.zip, unpack it and pass the directory in which it was
##' unpacked to this function.
##'
##' @param shapefile_dir directory containing the shape files
##' @return plot
##' @importFrom sf st_read st_union st_centroid
##' @importFrom lwgeom st_make_valid
##' @importFrom dplyr rename mutate filter if_else left_join group_by summarise select %>%
##' @importFrom tidyr replace_na
##' @importFrom purrr map map_dbl
##' @importFrom ggplot2 ggplot geom_sf geom_text xlab ylab coord_sf theme scale_fill_gradient aes element_blank
##' @author Sebastian Funk
##' @export
figure1 <- function(shapefile_dir) {

    health_zone_province <- katanga_measles_cases %>%
        dplyr::select(name=health_zone, province) %>%
        unique

    ## name assimilation
    katanga <- st_read(dsn=shapefile_dir, layer="zoneste") %>%
        rename(name=NOM_ZS) %>%
        mutate(name=as.character(name)) %>%
        mutate(name=if_else(name == "Mbulala", "Mbulula", name),
               name=if_else(name == "Kabondo Dianda", "Kabondo-Dianda", name),
               name=if_else(name == "Malemba", "Malemba-Nkulu", name),
               name=if_else(name == "Kilela Balanda", "Kilela-Balanda", name),
               name=if_else(name == "Kiyambi", "Kiambi", name),
               name=if_else(name == "Mufunga Sampwe", "Mufunga-Sampwe", name)) %>%
        filter(name %in% health_zone_province$name) %>%
        st_make_valid %>%
        left_join(health_zone_province)

    katanga_province <- katanga %>%
        group_by(province) %>%
        summarise(geometry=st_union(geometry)) %>%
        mutate( ## https:/.data=/github.com/tidyverse/ggplot2/issues/2111
            CENTROID = map(geometry, st_centroid),
            COORDS = map(CENTROID, st_coordinates),
            COORDS_X = map_dbl(COORDS, 1),
            COORDS_Y = map_dbl(COORDS, 2))

    health_zone_2015 <- katanga_measles_cases %>%
        filter(week_start >= "2015-01-01" & week_start <  "2016-01-01") %>%
        group_by(health_zone) %>%
        summarise(cases=sum(cases, na.rm=TRUE)) %>%
        select(name=health_zone, cases)

    map_2015 <- katanga %>%
        left_join(health_zone_2015, by="name") %>%
        replace_na(list(cases=0)) %>%
        mutate(cases=if_else(Nom_ZS_PUC %in% c("Kikula", "Lukafu", "Manika", "Kanzenze"), NA_real_, cases))

    p <- ggplot(map_2015) +
        geom_sf(aes(fill=cases), color="grey80") +
        geom_text(data=katanga_province, size=5, aes(COORDS_X-50000, COORDS_Y+10000,
                                                     label=province)) +
        xlab("") + ylab("") +
        coord_sf(datum=NA) +
        theme(axis.line = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) +
        scale_fill_gradient("Reported cases", low="white", high="black", na.value="white")
    return(p)
}
