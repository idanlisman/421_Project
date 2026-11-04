# Step 5: give each WORD node an emotion from the NRC Emotion Lexicon.
# NRC says, for each word, which of 10 categories it is linked to (1) or not (0).
# We keep the same 8 emotions as EmoTag (anger, anticipation, disgust, fear, joy,
# sadness, surprise, trust) and ignore the two sentiment categories (positive, negative).
# Unlike EmoTag, NRC has no scores: a word can be linked to several emotions at once.
source("R/config.R")
library(dplyr)

wide <- load_nrc()
write.csv(wide, file.path(res_dir, "word_emotions.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Attach to the word nodes of our network
nodes <- read.csv(file.path(res_dir, "node_table.csv"), encoding = "UTF-8", stringsAsFactors = FALSE)
wnodes <- nodes[nodes$type == "word", ]
m <- match(wnodes$name, wide$name)
wnodes <- cbind(wnodes, wide[m, c(emotag_emotions, "n_emotions", "emotions", "primary_emotion")])
wnodes$n_emotions[is.na(m)] <- 0
write.csv(wnodes, file.path(res_dir, "word_nodes_with_emotion.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Coverage summary
covered <- !is.na(m)
lines <- c(
  paste("Words in NRC (8 emotions, at least one emotion):", nrow(wide)),
  paste("Word nodes in our network:", nrow(wnodes)),
  paste("  with at least one NRC emotion:", sum(covered)),
  paste("  not in NRC (no emotion):", sum(!covered)),
  paste("Share of word links (strength) covered by NRC:", round(sum(wnodes$strength[covered]) / sum(wnodes$strength), 3)),
  "",
  "Word nodes by number of emotions:",
  capture.output(print(table(wnodes$n_emotions))),
  "",
  "Word nodes by primary emotion (multiple = linked to 2 or more emotions):",
  capture.output(print(table(wnodes$primary_emotion, useNA = "ifany"))),
  "",
  "Words linked to each emotion (a word with 2 emotions counts in both):",
  capture.output(print(colSums(wnodes[, emotag_emotions], na.rm = TRUE))),
  "",
  "Most-linked words NOT in NRC (no emotion):",
  if (any(!covered)) capture.output(print(head(wnodes[!covered, c("name", "degree", "strength")], 15), row.names = FALSE)) else "(none: the network only has NRC words)"
)
writeLines(lines, file.path(res_dir, "word_emotion_coverage.txt"), useBytes = TRUE)
cat(lines, sep = "\n")
