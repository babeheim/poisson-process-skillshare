
cell <- function(i, j, delta, ...) {
  if (length(i) == 1) {
    polygon(c(i - delta, i + delta, i + delta, i - delta), 
      c(j - delta, j - delta, j + delta, j + delta),
      ...
    )
  } else {
    # for (k in 1:length(i)) cell(i[k], j[k], delta = delta, ...)
    lapply(1:length(i), function(z) cell(i[z], j[z], delta = delta, ...))
  }
}

col_alpha <- function (acol, alpha = 0.2) {
    acol <- col2rgb(acol)
    acol.red <- acol["red",]/255
    acol.green <- acol["green",]/255
    acol.blue <- acol["blue",]/255
    acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
    return(as.character(acol))
}

plot_scatter <- function(intensity, n_side = 10, save_png = FALSE, ...) {

  if (length(intensity) == 1) {

    n_cells <- n_side ^ 2

    n_drops <- rpois(1, lambda = intensity * n_cells)

    cell_id <- 1:n_cells
    x <- sample(1:n_side, n_drops, replace = TRUE)
    y <- sample(1:n_side, n_drops, replace = TRUE)

    par(mfrow = c(1, 2))
    plot(1, 1,
      xlim = c(1, n_side), ylim = c(1, n_side), pch = 15, cex = 1, col = NULL,
      main = paste0(n_drops, " raindrops / ", n_cells, " cells"), xaxt = "n", yaxt = "n", xlab = "", ylab = ""
    )

    cell(x, y, delta = 0.5, col = col_alpha("dodgerblue", 0.3), border = NA)

    counts <- x + n_side * (y - 1)

    counts <- sapply(cell_id, function(z) sum(counts == z))
    mean(counts)
    n_drops / n_cells

    props <- prop.table(table(counts))
    plot(1, 1, col = NULL, xaxt = "n", ylim = c(0, 0.4), frame.plot = FALSE, xlim = c(-0.5, 10 + 0.5), las = 1, ylab = "frequency", xlab = "number of raindrops per cell", main = paste0("intensity = ", sprintf("%.2f", intensity), "\n (raindrops per cell)"))
    del <- 0.3
    for (i in 1:length(props)) polygon(c((i - 1) - del, (i - 1) + del, (i - 1) + del, (i - 1) - del), c(0, 0, props[i], props[i]), col = col_alpha("dodgerblue", min(1, (i * 0.2))), border = NA)
    axis(1, at = 1:length(props) - 1, labels = 1:length(props) - 1)
    points(1:length(props) - 1, dpois(1:length(props) - 1, lambda = intensity), type = "b")
    points(intensity, 0.3, pch = 6)

  } else {

    if (save_png == TRUE) {
      for (i in 1:length(intensity)) {
        filename <- paste0("image_", sprintf("%04d", i), ".png")
        png(filename, height = 14.5, width = 26.5, res = 100, units = "cm")
        plot_scatter(intensity[i], n_side = n_side, ...)
        dev.off()
      }
      graphics.off()
    } else {
      for (j in 1:length(intensity)) {
        plot_scatter(intensity[j], n_side = n_side, ...)
      }
    }

  }

}

lambdas <- rep(3, 20)

plot_scatter(lambdas, n_side = 100, save_png = FALSE)
# system("convert image_*.png plot_100.gif") # animate with imagemagik
