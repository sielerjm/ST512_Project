# Plot Settings

#   COLOR PALETTE
#     RcolorBrewer: https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
pal.bupu <- brewer.pal(9, "BuPu")  # specifies the length of the color palette and the name from RcolorBrewer
pal.dark2 <- brewer.pal(8, "Dark2")
pal.brbg <- brewer.pal(11, "BrBG")
pal.RdYlGn <- brewer.pal(11, "RdYlGn")
pal.PuOr <- brewer.pal(9, "PuOr") 
pal.YlGn <- brewer.pal(9, "YlGn")
pal.PuRd <- brewer.pal(9, "PuRd")
pal.RdPu <- brewer.pal(9, "RdPu")
pal.grey <- brewer.pal(9, "Greys")
pal.paired <- brewer.pal(12, "Paired")
pal.spectrum <- brewer.pal(11, "Spectral")
pal.accent <- brewer.pal(11, "Accent")
pal.set1 <- brewer.pal(9, "Set1")


#   Color codes for variables  
#     Might need changing depending how the figures look

col.diet <- pal.dark2[1:3]
col.timepoint <- c(pal.dark2[4], pal.dark2[8])  # creates a range of colors between those indexes
col.treatment <- c(pal.dark2[8], pal.dark2[6], pal.dark2[7])  # creates a list of colors
col.sex <- pal.dark2[1:2]
col.severity <- c(pal.YlGn[4], pal.YlGn[6], pal.YlGn[7], pal.YlGn[9])
col.exposed <- c(pal.paired[6], pal.paired[2])
col.infected <- c(pal.dark2[5:6])
col.spectrum <- c(pal.spectrum[1:9])
col.diverge <- c(pal.PuOr[1:9])


################################################################################
################################################################################


#   Cowplot settings 
#     Standardizing ggplot settings
#     (adapted and modified from Keaton Stagaman)
my_theme <- theme_update(
  legend.position = "none",
  legend.box = "vertical",
  legend.box.just = "center",
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 11),
  legend.key = element_rect(fill = "white"),
  legend.key.size = unit(1, "line"), # legend symbol size
  
  strip.text = element_text(size = 10),
  
  plot.caption = element_text(hjust = 0, size = 10),
  
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  panel.background = element_rect(fill = pal.grey[1]),
  panel.grid.major = element_line(color = pal.grey[3]),
  panel.grid.minor = element_line(color = pal.grey[3]),
  
)


################################################################################
################################################################################

# brewer_opts <- {
#   scale_fill_brewer(palette = "Dark2") +
#   scale_color_brewer(palette = "Dark2")
# }
