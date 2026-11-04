# Step 6: save the plots for words, emojis and the bipartite word-emoji network.
# Run after step 3. If steps 4 and 5 were run, nodes are coloured by emotion
# (EmoTag for emojis, NRC for words); otherwise they are grey.
# Emojis need the "ragg" package to draw correctly:  install.packages("ragg")
source("R/config.R")
library(igraph)

nodes <- read.csv(file.path(res_dir, "node_table.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
edges <- read.csv(file.path(proc_dir, "edges_word_emoji.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)

# ---- colours -------------------------------------------------------------
nodes$emotion <- NA_character_
f_emo <- file.path(res_dir, "emoji_nodes_with_emotion.csv")
f_wrd <- file.path(res_dir, "word_nodes_with_emotion.csv")
if (file.exists(f_emo)) {
  e <- read.csv(f_emo, encoding = "UTF-8", stringsAsFactors = FALSE)
  nodes$emotion[match(e$name, nodes$name)] <- e$top_emotion
}
if (file.exists(f_wrd)) {
  w <- read.csv(f_wrd, encoding = "UTF-8", stringsAsFactors = FALSE)
  i <- match(w$name, nodes$name)
  nodes$emotion[i] <- w$primary_emotion
}
node_color <- function(emo) {
  col <- rep(no_emotion_color, length(emo))
  ok <- !is.na(emo) & emo %in% names(emotion_colors)
  col[ok] <- emotion_colors[emo[ok]]
  col[!is.na(emo) & emo == "multiple"] <- multi_emotion_color
  col
}
nodes$color <- node_color(nodes$emotion)

# Legend with only the emotions that appear in the plot (info comes from present_info()).
# show_several: also list "several emotions" (only for plots that colour such words grey)
# outside: put the legend under the plot, in the bottom margin
legend_items <- function(info, show_several = FALSE, outside = FALSE, ncol = 4) {
  labs <- info$emotions
  cols <- unname(emotion_colors[info$emotions])
  if (show_several && info$several) {
    labs <- c(labs, "several emotions"); cols <- c(cols, multi_emotion_color)
  }
  if (info$none) { labs <- c(labs, "no emotion known"); cols <- c(cols, no_emotion_color) }
  if (outside) {
    u <- par("usr")
    legend(x = mean(u[1:2]), y = u[3] - 0.02 * (u[4] - u[3]), xjust = 0.5, yjust = 1,
           legend = labs, fill = cols, cex = 1.05, bty = "n", ncol = ncol, xpd = TRUE)
  } else {
    legend("bottomright", legend = labs, fill = cols, cex = 0.9, bty = "n", ncol = 2)
  }
}

# Emotion sets: for every node, its emotions as one text ("joy;trust"), for words (NRC) and emojis (EmoTag / manual)
emo_string <- character(0)
for (f in c(f_wrd, f_emo)) if (file.exists(f)) {
  x <- read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
  v <- if ("emotions" %in% names(x)) x$emotions else x$top_emotion
  emo_string <- c(emo_string, setNames(v, x$name))
}
# Pie data: one equal slice per emotion of the node. A node with no emotion gets one solid slice.
pie_values <- function(vnames) {
  lapply(vnames, function(n) {
    z <- if (n %in% names(emo_string)) emo_string[[n]] else NA
    if (is.na(z) || z == "") return(c(none = 1))
    parts <- strsplit(z, ";")[[1]]
    setNames(rep(1, length(parts)), parts)
  })
}
pie_colors <- function(vals, solid) {
  Map(function(v, sc) if (names(v)[1] == "none") sc else unname(emotion_colors[names(v)]), vals, solid)
}

# Which emotions (and "none" / "several") appear among these nodes
present_info <- function(vnames) {
  nm <- unlist(lapply(pie_values(vnames), names))
  em <- nodes$emotion[match(vnames, nodes$name)]
  list(emotions = intersect(names(emotion_colors), nm),
       none     = any(nm == "none"),
       several  = any(em %in% "multiple"))
}

open_png <- function(file, w, h, res = 150) {
  path <- file.path(fig_dir, file)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(path, width = w, height = h, units = "in", res = res)
  } else {
    warning("Package 'ragg' not installed: emojis may show as empty boxes. install.packages('ragg')")
    png(path, width = w, height = h, units = "in", res = res)
  }
}

# A node is drawn as a pie only if it has 2 or more emotions; otherwise a plain coloured shape
shape_for <- function(pv, is_emoji, emoji_shape) {
  ifelse(lengths(pv) > 1, "pie", ifelse(is_emoji, emoji_shape, "circle"))
}

# ---- 1 + 2. bar charts: each bar is split equally between the node's emotions ----
# (the lexicons only say yes/no per emotion, so equal slices show WHICH emotions a node has, not how much)
# Extra room on the right (xlim) so the numbers and the legend do not cover the bars.
stacked_bar <- function(d, main, n = 25) {
  d <- head(d[order(-d$degree), ], n)
  d <- d[nrow(d):1, ]
  pv <- pie_values(d$name)
  m <- matrix(0, nrow = length(emotion_colors) + 1, ncol = nrow(d),
              dimnames = list(c(names(emotion_colors), "none"), NULL))
  for (k in seq_len(nrow(d))) m[names(pv[[k]]), k] <- d$degree[k] / length(pv[[k]])
  par(mar = c(4.5, 6, 3, 1))
  bp <- barplot(m, horiz = TRUE, names.arg = d$name, las = 1,
                col = c(emotion_colors, no_emotion_color), border = "white", lwd = 0.6,
                main = main, xlab = "Degree (number of different linked nodes)",
                cex.names = 1.1, xlim = c(0, max(d$degree) * 1.35))
  text(d$degree, bp, labels = d$degree, pos = 4, cex = 0.8, xpd = TRUE)
  legend_items(present_info(d$name))
}
open_png("words_top.png", 8, 8)
stacked_bar(nodes[nodes$type == "word", ],
            "Top 25 word nodes by degree\n(bar split equally between the word's NRC emotions)")
dev.off()

open_png("emojis_top.png", 8, 8)
stacked_bar(nodes[nodes$type == "emoji", ],
            "Top 25 emoji nodes by degree\n(bar split equally if the emoji has several emotions)")
dev.off()

# ---- 3. bipartite network: top words (top row) and top emojis (bottom row) ----
nw <- 25; ne <- 15
tw <- head(nodes$name[nodes$type == "word"][order(-nodes$degree[nodes$type == "word"])], nw)
te <- head(nodes$name[nodes$type == "emoji"][order(-nodes$degree[nodes$type == "emoji"])], ne)
sub_edges <- edges[edges$word %in% tw & edges$emoji %in% te, ]
g <- graph_from_data_frame(sub_edges, directed = FALSE,
                           vertices = data.frame(name = c(tw, te)))
is_emoji <- V(g)$name %in% te
V(g)$type <- is_emoji
lay <- layout_as_bipartite(g)             # words on the top row, emojis on the bottom row
# spread each row evenly across the width (keeps the order that reduces crossing lines)
xw <- 2.2                                 # half width of the picture (the height is 1)
for (r in unique(lay[, 2])) {
  i <- lay[, 2] == r
  lay[i, 1] <- (rank(lay[i, 1], ties.method = "first") - 1) / max(1, sum(i) - 1)
}
lay2 <- cbind(xw * (2 * lay[, 1] - 1), 2 * lay[, 2] - 1)   # words at y = +1, emojis at y = -1
idx <- match(V(g)$name, nodes$name)
V(g)$color <- nodes$color[idx]
pv <- pie_values(V(g)$name)
V(g)$shape <- shape_for(pv, is_emoji, "circle")   # every node here is a circle or a pie
V(g)$size  <- ifelse(is_emoji, 8, 9)
E(g)$width <- 0.5 + 4 * E(g)$weight / max(E(g)$weight)

open_png("network_bipartite_top.png", 20, 11)
par(mar = c(5, 1, 9, 1))
plot(g, layout = lay2, rescale = FALSE, xlim = c(-xw - 0.1, xw + 0.1), ylim = c(-1.55, 1.1),
     vertex.label = NA, vertex.pie = pv, vertex.pie.color = pie_colors(pv, V(g)$color),
     vertex.frame.color = "grey30", edge.color = adjustcolor("grey30", 0.35))
mtext(paste0("Bipartite word-emoji network: top ", nw, " words (top row) and top ", ne,
             " emojis (bottom row)"), side = 3, line = 7, font = 2, cex = 1.3)
mtext("line width = number of posts; each circle is split equally between the node's emotions",
      side = 3, line = 5.6, cex = 1.1)
# Labels drawn by hand so they never overlap: words are tilted above their circle,
# emojis sit well below their circle so they do not cover it
wd <- !is_emoji
text(lay2[wd, 1], lay2[wd, 2] + 0.08, V(g)$name[wd], srt = 55, adj = c(0, 0.5), cex = 1.3,
     family = "sans", xpd = TRUE)
text(lay2[is_emoji, 1], lay2[is_emoji, 2] - 0.2, V(g)$name[is_emoji], cex = 2.2, xpd = TRUE)
legend_items(present_info(V(g)$name), outside = TRUE)
dev.off()

# ---- 4. the whole network, no emoji symbols: colour = emotion only --------
G <- graph_from_data_frame(edges, directed = FALSE)
G_emoji <- V(G)$name %in% edges$emoji
j <- match(V(G)$name, nodes$name)
V(G)$color <- nodes$color[j]
V(G)$size  <- ifelse(G_emoji, 1.5 + 0.9 * log1p(nodes$degree[j]), 1.5)
V(G)$shape <- ifelse(G_emoji, "square", "circle")
set.seed(421)
lay_all <- layout_with_fr(G)
open_png("network_full_emotions.png", 12, 13, res = 200)
par(mar = c(7, 0.5, 2.5, 0.5))
plot(G, layout = lay_all, vertex.label = NA, vertex.frame.color = NA,
     edge.width = 0.2, edge.color = adjustcolor("grey40", 0.15),
     main = paste0("Whole word-emoji network, colour = emotion (", sum(!G_emoji),
                   " words = circles, ", sum(G_emoji), " emojis = squares)"))
legend_items(present_info(V(G)$name), show_several = TRUE, outside = TRUE, ncol = 3)
dev.off()

# ---- 5. slim network: top 25 emojis and top 25 words, with labels ----------
# Words are circles (pies if several emotions). Emojis are ALWAYS squares: a square split into
# vertical stripes, one per emotion, with the emoji symbol on top. (igraph's pie shape is round,
# so the emoji squares are drawn by hand.)
tw25 <- head(nodes$name[nodes$type == "word"][order(-nodes$degree[nodes$type == "word"])], 25)
te25 <- head(nodes$name[nodes$type == "emoji"][order(-nodes$degree[nodes$type == "emoji"])], 25)
e25 <- edges[edges$word %in% tw25 & edges$emoji %in% te25, ]
H <- graph_from_data_frame(e25, directed = FALSE)
H_emoji <- V(H)$name %in% te25
k <- match(V(H)$name, nodes$name)
V(H)$color <- nodes$color[k]
pvH <- pie_values(V(H)$name)
V(H)$shape <- ifelse(H_emoji, "none", ifelse(lengths(pvH) > 1, "pie", "circle"))
V(H)$size  <- 7
E(H)$width <- 0.5 + 5 * E(H)$weight / max(E(H)$weight)
set.seed(421)
layH <- norm_coords(layout_with_fr(H, weights = E(H)$weight), -1.3, 1.3, -1.3, 1.3)

# Push nodes apart until no two overlap (also leaves room for the word labels).
# Minimum centre-to-centre distances, in plot units: emoji-emoji, emoji-word, word-word.
spread <- function(lay, is_emoji, d_ee = 0.22, d_ew = 0.2, d_ww = 0.18, iters = 500) {
  n <- nrow(lay)
  set.seed(1)
  for (it in seq_len(iters)) {
    moved <- FALSE
    for (i in 1:(n - 1)) for (j in (i + 1):n) {
      d <- lay[j, ] - lay[i, ]
      dist <- sqrt(sum(d^2))
      need <- if (is_emoji[i] && is_emoji[j]) d_ee else if (is_emoji[i] || is_emoji[j]) d_ew else d_ww
      if (dist < need) {
        if (dist < 1e-9) { d <- runif(2, -1, 1); dist <- sqrt(sum(d^2)) }
        push <- (need - dist) / 2 * d / dist
        lay[i, ] <- lay[i, ] - push
        lay[j, ] <- lay[j, ] + push
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  lay
}
layH <- spread(layH, H_emoji)
lim <- range(layH) + c(-0.25, 0.25)            # same limits on both axes keeps squares square

open_png("network_top25.png", 16, 16, res = 200)
par(mar = c(5, 0.5, 3, 0.5))
plot(H, layout = layH, rescale = FALSE, xlim = lim, ylim = lim,
     vertex.frame.color = "grey30",
     vertex.pie = pvH, vertex.pie.color = pie_colors(pvH, V(H)$color),
     vertex.label = ifelse(H_emoji, NA, V(H)$name), vertex.label.color = "black",
     vertex.label.family = "sans", vertex.label.cex = 1.1, vertex.label.dist = 1.4,
     vertex.label.degree = -pi / 2,
     edge.color = adjustcolor("grey30", 0.35),
     main = "Slim network: top 25 emojis (squares) and top 25 words (circles)\nline width = number of posts; each node is split equally between its emotions")
h <- 0.065                                            # half side of an emoji square
for (v in which(H_emoji)) {
  cx <- layH[v, 1]; cy <- layH[v, 2]
  nm <- names(pvH[[v]])
  cols <- if (nm[1] == "none") V(H)$color[v] else unname(emotion_colors[nm])
  edges_x <- seq(cx - h, cx + h, length.out = length(cols) + 1)
  rect(edges_x[-length(edges_x)], cy - h, edges_x[-1], cy + h, col = cols, border = NA)
  rect(cx - h, cy - h, cx + h, cy + h, border = "grey30")
  text(cx, cy, V(H)$name[v], cex = 2)                 # emoji symbol centred on its square
}
legend_items(present_info(V(H)$name), outside = TRUE)
dev.off()

cat("Saved to", fig_dir, ":\n", paste(" ", c("words_top.png", "emojis_top.png",
    "network_bipartite_top.png", "network_full_emotions.png", "network_top25.png")), sep = "\n")
