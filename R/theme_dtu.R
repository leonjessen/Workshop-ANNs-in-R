dtu_colours = c("corp_red" = "#990000",
                "white" = "#FFFFFF",
                "black" = "#000000",
                "blue" = "#2F3EEA",
                "bright_green" = "#1FD082",
                "navy_blue" = "#030F4F",
                "yellow" = "#F6D04D",
                "orange" = "#FC7634",
                "pink" = "#F7BBB1",
                "grey" = "#DADADA",
                "red" = "#E83F48",
                "green" = "#008835",
                "purple" = "#79238E")
theme_dtu <- function(){
  corporate_font <- "Arial"
  theme_minimal(base_size = 15, base_family = corporate_font) %+replace%
    theme(panel.grid = element_blank(),
          panel.grid.major.y = element_line(colour = dtu_colours["grey"]),
          axis.line.x = element_line(colour = dtu_colours["black"]))
}

