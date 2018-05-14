library(ggplot2)
library(gridExtra)
library(grid)

gpp_graph <- function(CPS_data, GPP_data, y_axis)
{
a <- ggplot() + 
  geom_line(data=CPS_data, aes(x = year, y = value, colour="red"), size=2) +
  geom_line(data=GPP_data, aes(x = year, y = value, colour="green"), linetype="dotted", size=2)  +
  labs(x = "year", y = y_axis) +
  #scale_colour_manual(name="Scenario", values=c("Current policies" = "red", "Good practice policies"="green")) +
  scale_fill_discrete(name="Scenario",
                      breaks=c("Current policies", "Good practice policies"),
                      labels=c("Current policies", "Good practice policies")) +
  theme(axis.title = element_text(face="bold", size=10)) +
  theme(axis.text = element_text(face="bold", size=10)) +
  theme(legend.text=element_text(size=10)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black')) #+
  #scale_y_continuous(limits = c(0, NA))
  
  output <- a

}

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}