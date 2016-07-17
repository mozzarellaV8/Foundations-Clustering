# Voronoi / Delaunay Visualization Tests


# Voronoi Cells -------------------------------------------

library(deldir)
vtess <- deldir(winescale$Alcohol, winescale$Proline)
vtess_tile <- tile.list(vtess)

library(RColorBrewer)
winepal <- brewer.pal(3, "Reds")

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), family = "HersheySans")
plot.tile.list(vtess_tile, type = "triang", asp = 1, axes = F, 
               fillcol = winepal)

# ggplot w/ delaunay triangulation
wcplot03 <- ggplot(wineclust, aes(x = Alcohol, y = Proline, color = cluster)) +
  geom_point(size = 2.75) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans") +
  geom_segment(data = vtess$delsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "red4", alpha = 0.35) +
  labs(title = "Delaunay Triangulation over Alcohol and Proline points")

wcplot03

wcplot04 <- ggplot(wineclust, aes(x = Alcohol, y = Proline, color = cluster)) +
  geom_point(size = 2.75) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center')) +
  geom_point(data = centers, aes(x = Alcohol, y = Proline, color = 'Center'),
             size = 24, alpha = 0.25) +
  theme_minimal(base_size = 12, base_family = "HersheySans") +
  geom_segment(data = vtess$dirsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "red4", alpha = 0.35) +
  labs(title = "Voronoi tessellation over Alcohol and Proline points")

wcplot04