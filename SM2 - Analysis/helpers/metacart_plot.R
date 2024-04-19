# This code is adapted slightly from the metacart package so that the plot function returns a ggplot object - otherwise, post-processing is very difficult
# Copyright: Elise Dusseldorp, 2020 - GPL-2

metacart_plot <- function (x, sesoi = .1, ...)
{
  if (length(x$n) < 2) {
    warning("no tree was detected")
  }
  else {
    transparent_theme <- ggplot2::theme(axis.line = element_blank(),
                                        axis.text.x = element_blank(), axis.text.y = element_blank(),
                                        axis.ticks = element_blank(), axis.title.x = element_blank(),
                                        axis.title.y = element_blank(), panel.background = element_blank(),
                                        panel.border = element_blank(), panel.grid.major = element_blank(),
                                        panel.grid.minor = element_blank(), plot.background = element_blank())
    transparent_theme2 <- ggplot2::theme(axis.text.x = element_blank(),
                                         axis.ticks = element_blank(), axis.title.x = element_blank(),
                                         axis.title.y = element_blank(), panel.background = element_blank(),
                                         panel.border = element_blank(), panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(), plot.background = element_blank())
    y <- NULL
    term.node <- NULL
    yi <- NULL
    leaf.no <- NULL
    tree <- x$tree
    tree <- tree[!is.na(tree$pleaf), ]
    count <- 1
    nodes <- data.frame(leaf = 1, pleaf = 0, x = 0, y = 0,
                        w = 1)
    for (pleaf in tree$pleaf) {
      pleaf_row <- nodes[nodes$leaf == pleaf, ]
      count <- count + 1
      nodes <- updateNodes(nodes, data.frame(leaf = count,
                                             pleaf = pleaf, x = pleaf_row$x - pleaf_row$w/2,
                                             y = pleaf_row$y - 1, w = pleaf_row$w/2))
      count <- count + 1
      nodes <- updateNodes(nodes, data.frame(leaf = count,
                                             pleaf = pleaf, x = pleaf_row$x + pleaf_row$w/2,
                                             y = pleaf_row$y - 1, w = pleaf_row$w/2))
    }
    nodes$split = NA
    nodes$split[tree$pleaf] = as.character(tree$split)
    nodes$x.new <- rep(NA, nrow(nodes))
    inx.term <- !(nodes$leaf %in% nodes$pleaf)
    nodes.term <- nodes[inx.term, ]
    nodes$x.new[inx.term] <- rank(nodes$x[inx.term])
    nodes$leaf.no[inx.term] <- x$n[as.character(nodes$leaf[inx.term])]
    for (i in min(nodes$y):-1) {
      inx.pleaf <- which(nodes$y == i)
      coords <- sapply(split(nodes$x.new[inx.pleaf], nodes$pleaf[inx.pleaf]),
                       mean)
      leaf.no <- sapply(split(nodes$leaf.no[inx.pleaf],
                              nodes$pleaf[inx.pleaf]), sum)
      inx.replace <- names(coords[!is.na(coords)])
      nodes$x.new[as.numeric(inx.replace)] <- coords[inx.replace]
      nodes$leaf.no[as.numeric(inx.replace)] <- leaf.no[inx.replace]
    }
    nodes$x <- nodes$x.new
    config.leaf_width_scale <- 0.9
    x.scale <- config.leaf_width_scale/2 * min(sapply(split(nodes[-1,
    ]$x, f = nodes[-1, ]$y), function(x) min(diff(sort(x)))))
    y.scale <- x.scale * diff(range(nodes$y))/diff(range(nodes$x))
    vis <- ggplot()
    for (i in 1:nrow(nodes)) {
      node <- nodes[i, ]
      if (node$pleaf == 0) {
        next
      }
      parent = nodes[nodes$leaf == node$pleaf, ]
      data_line = data.frame(x = c(node$x, parent$x), y = c(node$y,
                                                            parent$y))
      vis <- vis + geom_line(data = data_line, aes(x, y),
                             color = "black")
    }
    config.branch_text_left_dx = -0.2
    config.branch_text_right_dx = 0.2
    config.branch_text_left = "Yes"
    config.branch_text_right = "No"
    config.branch_text_size = 3
    config.leaf_oval_ratio = 1.3
    config.leaf_text_size = 5
    config.split_text_dy = -0.33
    config.split_text_size = 3
    config.split_label = T
    for (i in 1:nrow(nodes)) {
      node <- nodes[i, ]
      parent = nodes[nodes$leaf == node$pleaf, ]
      vis <- oval_draw(vis, node$x, node$y, config.leaf_oval_ratio,
                       x.scale, y.scale) + geom_text(data = data.frame(x = node$x,
                                                                       y = node$y), aes(x, y), label = paste("K =",
                                                                                                             node$leaf.no), size = config.leaf_text_size)
      h = 1
      if (!is.na(node$split)) {
        dy <- h * config.split_text_dy
        data_text = data.frame(x = node$x, y = node$y +
                                 dy)
        show_text = ifelse(config.split_label, geom_label,
                           geom_text)
        vis <- vis + show_text(data = data_text, aes(x,
                                                     y), label = encodeHtml(node$split), size = config.split_text_size)
      }
      dx = h * ifelse(node$leaf%%2 == 0, config.branch_text_left_dx,
                      config.branch_text_right_dx)
      data_text = data.frame(x = (node$x + parent$x)/2 +
                               dx, y = (node$y + parent$y)/2)
      vis <- vis + geom_text(data = data_text, aes(x, y),
                             label = ifelse(node$leaf%%2 == 0, config.branch_text_left,
                                            config.branch_text_right), size = config.branch_text_size)
    }
    vis <- vis + transparent_theme
    term <- nodes[is.na(nodes$split), ]
    term <- term[ordered(term$x.new), ]
    yi <- model.response(x$data)
    p <- ggplot()
    p <- p + geom_hline(yintercept = c(min(yi), max(yi)),
                        linetype = "solid")
    p <- p + geom_hline(yintercept = 0, linetype = "dashed")
    p <- p + scale_x_discrete(limits = as.factor(term$leaf))
    CI.ratio = 2
    for (i in unique(x$data$term.node)) {
      i <- as.character(i)
      y.coord2 = x$g[i]
      x.coord2 = nodes[i, ]$x
      p <- CI_draw(p, x = x.coord2, y = y.coord2, b = 1.96 *
                     x$se[i], a = 1.96 * x$se[i]/CI.ratio)
    }
    p <- p + geom_hline(yintercept = sesoi, linetype = "dotted")
    p <- p + geom_hline(yintercept = -sesoi, linetype = "dotted")
    p <- p + transparent_theme2
    patchwork::wrap_plots(vis, p, ncol = 1, heights = c(3,1))
  }
}

updateNodes <- function (nodes, newNode, name = "leaf")
{
  rows <- nodes[name] == newNode[1, name]
  nodes <- rbind(nodes[!rows, ], newNode)
  nodes
}

oval_draw <- function (plotobj, x, y, c, x.scale = 1, y.scale = 1, ...)
{
  t <- seq(-1 * pi, 1 * pi, length = 100)
  df <- data.frame(x = x.scale * sin(t) + x, y = y.scale *
                     cos(t)/c + y)
  plotobj <- plotobj + geom_polygon(data = df, aes(x, y), fill = "lightgrey",
                                    color = "black")
}

encodeHtml <- function (input)
{
  dict <- data.frame(c("&", "&"), c("<=", "≤"), c(">=", "≥"),
                     c("<", "<"), c(">", ">"))
  tmp <- input
  for (i in 1:ncol(dict)) {
    tmp <- gsub(dict[1, i], dict[2, i], tmp)
    Encoding(tmp) <- "UTF-8"
  }
  tmp
}

CI_draw <- function (plotobj, x, y, a = 1, b = 1)
{
  x0 <- seq(0, a, length = 25)
  x1 <- seq(-a, 0, length = 25)
  y1 <- -b/a * x0 + b
  y2 <- b/a * x0 - b
  y3 <- b/a * x1 + b
  y4 <- -b/a * x1 - b
  x.coord <- c(x0, x1, x0, x1) + x
  y.coord <- c(y1, y3, y2, y4) + y
  df <- data.frame(x.coord = x.coord, y.coord = y.coord)
  plotobj <- plotobj + geom_polygon(data = df, aes(x.coord,
                                                   y.coord), fill = "black", color = "black")
}
