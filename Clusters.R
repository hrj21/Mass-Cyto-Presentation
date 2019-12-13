a_1 <- rnorm(100, 10, 2)
a_2 <- rnorm(100, 2, 5)
a_3a <- rnorm(50, 0, 1)
a_3b <- rnorm(50, 6, 1)

b_1 <- rnorm(100, 30, 2)
b_2 <- rnorm(100, 6, 5)
b_3a <- rnorm(50, 0, 1)
b_3b <- rnorm(50, 6, 1)

c_1 <- rnorm(100, 10, 2)
c_2 <- rnorm(100, 80, 5)
c_3a <- rnorm(50, 0, 1)
c_3b <- rnorm(50, 6, 1)

d <- data.frame(id = rep(1:3, 100),
                cluster = rep(c("a", "b", "c", "d", "e", "f"), each = 50),
                x = c(a_1, b_1, c_1),
                y = c(a_2, b_2, c_2),
                z = c(c(a_3a, a_3b), c(b_3a, b_3b), c(c_3a, c_3b)))

d

saveRDS(d, "cluster_data.rds")

pairs(d[, 3:5], col = d$cluster)

library(tidyverse)
library(GGally)


ggplot(d, aes(x, y)) +
  geom_point(size = 5, shape = 21, fill = "black") +
  theme_bw()

ggsave("img/cluster_example1.png", dpi = 300)

ggplot(d, aes(x, z)) +
  geom_point(size = 5) +
  theme_bw()

ggplot(d, aes(y, z)) +
  geom_point(size = 5) +
  theme_bw()

gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1
  
  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1
  
  g
}


g <- ggpairs(d[, 3:5], 
             lower  = list(continuous = wrap("points", size = 3)),
             upper  = list(continuous = "blank"),
             diag  = list(continuous = "blankDiag")
)

g_col <- ggpairs(d[, 3:5], mapping = aes(col = d$cluster),
             lower  = list(continuous = wrap("points", size = 3)),
             upper  = list(continuous = "blank"),
             diag  = list(continuous = "blankDiag")
)

gpairs_lower(g) + theme_bw()

ggsave("img/cluster_example2.png", dpi = 300)

gpairs_lower(g_col) + theme_bw()

ggsave("img/cluster_example3.png", dpi = 300)

library(plot3D)
library(plot3Drgl)

scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst, colkey = FALSE) 
}

Cairo_png("img/cluster_example3.png", res = 200, type = "cairo-png")
scatter3D_fancy(x = d$x, y = d$y, z = d$z, pch = 19, phi = 10, theta = 20, 
                bty = "g", ticktype = "detailed")
dev.off()

plotrgl()
