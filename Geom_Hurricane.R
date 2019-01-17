#'<ggplot geom> Wind radii chart
#'
#'This is a customized geom function that plots a layer of wind radii chart onto a ggplot object. It can be used with other ggplot2 and
#'ggmap functions to create a wind radii chart.
#'Required aesthetics includes "x", "y", "r_ne", "r_se", "r_nw", "r_sw", which refers to geo-coordinates, direction and wind speed info.
#'Aesthetics such as "fill" and "color" can be used to modify elements of the plot.
#'
#'@param x Longitude value for hurricane eye location.
#'@param y Latitude value for hurricane eye location.
#'@param r_ne Furtherst distance in north-eastern direction.
#'@param r_nw Furtherst distance in north-western direction.
#'@param r_se Furtherst distance in south-eastern direction.
#'@param r_sw Furtherst distance in south-western direction.
#'
#'@param fill <Optional> Typically uses wind speed for internal colouring of polygon
#'@param color <Optional> Typically uses wind speed for polygon border colouring
#'
#'@examples
#'get_map("Galveston", zoom = 6, maptype = "toner-background") %>%
#'ggmap(extent = "device") +
#'  ggtitle("Ike 2008-09-13 12:00:00, @Galveston") +
#'    geom_hurricane(data = selected_tracks, inherit.aes = FALSE,
#'                     mapping = (aes(x = longitude, y = latitude,
#'                                    r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'                                    fill = wind, color = wind))
#') + 
#'scale_color_manual(name = "Wind speed (kts)", 
#'values = c("red4", "orange4", "yellow4")) + 
#'scale_fill_manual(name = "Wind speed (kts)", 
#'values = c("red", "orange", "yellow"))
#'
#'@export

geom_hurricane <- function(mapping = NULL, data = NULL, position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = StatHurricane, geom = GeomHurricane, data = data, mapping = mapping,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

GeomHurricane <- ggplot2:ggproto("GeomHurricane", ggplot2::GeomPolygon,
                                 default.aes(ggplot2::aes(colour = NA,
                                                          fill = NA,
                                                          size = 0.5,
                                                          linetype = 1,
                                                          alpha = 0.65)))


stat_hurricane <- function(mapping = NULL, data = NULL, geom = "polygon",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatHurricane,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

StatHurricane <- ggplot2::ggproto("StatHurricane", ggplot2::Stat,
                                  compute_group = function(data, scales) {
                                    eye <- c( as.numeric(data[1, "x"]), as.numeric(data[1, "y"]) )
                                    scale <- data[1,]$scale_radii
                                    if( is.null(scale) ) {scale = 1.0}
                                    
                                    #data uses nautical miles as distance unit, thus, need to convert to meter for plotting
                                    xne <- geosphere::destPoint(eye, b = seq(  0,  90, length.out = 21), d = data[1,]$r_ne*1852.0*scale)
                                    xse <- geosphere::destPoint(eye, b = seq( 90, 180, length.out = 21), d = data[1,]$r_se*1852.0*scale)
                                    xsw <- geosphere::destPoint(eye, b = seq(180, 270, length.out = 21), d = data[1,]$r_sw*1852.0*scale)
                                    xnw <- geosphere::destPoint(eye, b = seq(270, 360, length.out = 21), d = data[1,]$r_nw*1852.0*scale)
                                    df_34 <- as.data.frame(rbind(xne, xse, xsw, xnw))
                                    colnames(df_34) <- c("x", "y")
                                    df_34["wind"] <- NULL
                                    df_34$wind <- 34
                                    xne <- geosphere::destPoint(eye, b = seq(  0,  90, length.out = 21), d = data[2,]$r_ne*1852.0*scale)
                                    xse <- geosphere::destPoint(eye, b = seq( 90, 180, length.out = 21), d = data[2,]$r_se*1852.0*scale)
                                    xsw <- geosphere::destPoint(eye, b = seq(180, 270, length.out = 21), d = data[2,]$r_sw*1852.0*scale)
                                    xnw <- geosphere::destPoint(eye, b = seq(270, 360, length.out = 21), d = data[2,]$r_nw*1852.0*scale)
                                    df_50 <- as.data.frame(rbind(xne, xse, xsw, xnw))
                                    colnames(df_50) <- c("x", "y")
                                    df_50["wind"] <- NULL
                                    df_50$wind <- 50
                                    xne <- geosphere::destPoint(eye, b = seq(  0,  90, length.out = 21), d = data[3,]$r_ne*1852.0*scale)
                                    xse <- geosphere::destPoint(eye, b = seq( 90, 180, length.out = 21), d = data[3,]$r_se*1852.0*scale)
                                    xsw <- geosphere::destPoint(eye, b = seq(180, 270, length.out = 21), d = data[3,]$r_sw*1852.0*scale)
                                    xnw <- geosphere::destPoint(eye, b = seq(270, 360, length.out = 21), d = data[3,]$r_nw*1852.0*scale)
                                    df_64 <- as.data.frame(rbind(xne, xse, xsw, xnw))
                                    colnames(df_64) <- c("x", "y")
                                    df_64["wind"] <- NULL
                                    df_64$wind <- 64
                                    df <- rbind(df_34, df_50, df_64)
                                    df$wind <- as.factor(df$wind)

                                    #Output df includes three columns: x, y, wind. Fits well into GeomPolygon aesthetics
                                    df},
                                  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
                                  default_aes = ggplot2::aes(scale_radii = 1.0))