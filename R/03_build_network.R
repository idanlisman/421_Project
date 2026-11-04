# Step 3: build the word-emoji network (bipartite: edges only between a word and an emoji)
# from the TRAIN posts, and compute the first basic numbers.
source("R/config.R")
library(igraph)
library(dplyr)

posts <- readRDS(file.path(proc_dir, "posts_clean.rds"))
train <- posts[posts$split == "train", ]

# Every (word, emoji) pair that appears in the same post
make_pairs <- function(w, e) {
  if (length(w) == 0 || length(e) == 0) return(NULL)
  expand.grid(word = w, emoji = e, stringsAsFactors = FALSE)
}
pairs <- do.call(rbind, Map(make_pairs, train$words, train$emojis))
stopifnot(!is.null(pairs))

# Edge weight = number of posts in which the word and the emoji appear together
edges <- pairs %>% count(word, emoji, name = "weight")
write.csv(edges, file.path(proc_dir, "edges_word_emoji.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

g <- graph_from_data_frame(edges, directed = FALSE)
V(g)$type <- V(g)$name %in% edges$emoji     # TRUE = emoji node, FALSE = word node
stopifnot(is_bipartite(g))

# Node table
nodes <- data.frame(
  name     = V(g)$name,
  type     = ifelse(V(g)$type, "emoji", "word"),
  degree   = degree(g),            # number of links
  strength = strength(g),          # sum of the weights of the links
  stringsAsFactors = FALSE
)
nodes <- nodes[order(-nodes$degree), ]
write.csv(nodes, file.path(res_dir, "node_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Basic numbers
n_words  <- sum(!V(g)$type)
n_emojis <- sum(V(g)$type)
comp <- components(g)
summary_lines <- c(
  paste("Word nodes:", n_words),
  paste("Emoji nodes:", n_emojis),
  paste("Edges:", ecount(g)),
  paste("Bipartite density (edges / possible word-emoji pairs):",
        round(ecount(g) / (n_words * n_emojis), 4)),
  paste("Average degree of word nodes:",  round(mean(degree(g)[!V(g)$type]), 2)),
  paste("Average degree of emoji nodes:", round(mean(degree(g)[V(g)$type]), 2)),
  paste("Connected components:", comp$no),
  paste("Size of the largest component:", max(comp$csize)),
  "",
  "Top 10 words by degree:",
  capture.output(print(head(nodes[nodes$type == "word", c("name", "degree", "strength")], 10), row.names = FALSE)),
  "",
  "Top 10 emojis by degree:",
  capture.output(print(head(nodes[nodes$type == "emoji", c("name", "degree", "strength")], 10), row.names = FALSE))
)
writeLines(summary_lines, file.path(res_dir, "network_summary.txt"), useBytes = TRUE)
cat(summary_lines, sep = "\n")

# Degree distribution figure
png(file.path(fig_dir, "degree_distribution.png"), width = 1200, height = 500, res = 120)
par(mfrow = c(1, 2))
hist(nodes$degree[nodes$type == "word"],  breaks = 30, main = "Word nodes",
     xlab = "Degree (number of emojis linked)", col = "grey80")
hist(nodes$degree[nodes$type == "emoji"], breaks = 30, main = "Emoji nodes",
     xlab = "Degree (number of words linked)", col = "grey80")
dev.off()
