# Step 4: give every emoji an emotion, using the EmoTag1200 scores.
# EmoTag has 150 emojis. Each has a score from 0 to 1 for 8 emotions.
# The emoji's emotion = the emotion with the highest score; its second emotion is added too
# when that score is close to the top one (settings in config.R).
# Run this after step 3 (it needs results/node_table.csv).
source("R/config.R")
library(stringi)

tag <- read.csv(emotag_file, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
stopifnot(all(emotag_emotions %in% names(tag)))

# Write the emoji the same way as in our posts (remove the invisible selector U+FE0F)
tag$emoji <- stri_replace_all_fixed(tag$emoji, "️", "")

# Highest and second-highest emotion for each emoji
scores <- as.matrix(tag[, emotag_emotions])
ord <- t(apply(scores, 1, function(s) order(s, decreasing = TRUE)))
idx <- seq_len(nrow(scores))
tag$top_emotion    <- emotag_emotions[ord[, 1]]
tag$top_score      <- scores[cbind(idx, ord[, 1])]
tag$second_emotion <- emotag_emotions[ord[, 2]]
tag$second_score   <- scores[cbind(idx, ord[, 2])]
tag$top_emotion[tag$top_score == 0] <- NA        # no emotion at all
tag$tie <- tag$top_score == tag$second_score     # TRUE = top emotion is tied with another

# Keep the second emotion too only if it is close to the top one (settings in config.R)
two <- !is.na(tag$top_emotion) &
  tag$second_score >= emotag_second_min &
  tag$second_score >= emotag_second_ratio * tag$top_score
tag$n_emotions <- ifelse(is.na(tag$top_emotion), 0L, 1L + two)
tag$emotions   <- ifelse(two, paste(tag$top_emotion, tag$second_emotion, sep = ";"), tag$top_emotion)

cols <- c("emoji", "name", "top_emotion", "top_score",
          "second_emotion", "second_score", "tie", "n_emotions", "emotions", emotag_emotions)
write.csv(tag[, cols], file.path(res_dir, "emoji_emotions.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Attach the emotion to the emoji nodes of our network
nodes <- read.csv(file.path(res_dir, "node_table.csv"),
                  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
emo <- nodes[nodes$type == "emoji", ]
m <- match(emo$name, tag$emoji)                  # NA = emoji is not in EmoTag
emo$best_emotion <- tag$top_emotion[m]                       # the single highest-scoring emotion
emo$top_emotion  <- ifelse(tag$n_emotions[m] > 1, "multiple", tag$top_emotion[m])
emo$top_score   <- tag$top_score[m]
emo$tie         <- tag$tie[m]
emo$source      <- ifelse(is.na(m), NA, "EmoTag")
emo$emotions    <- tag$emotions[m]          # all emotions of the emoji, separated by ";"

# Emotions you filled in by hand for emojis EmoTag does not have (data/emotag/manual_emoji_emotions.csv).
# In the "emotion" column write one or several of the 8 emotions, separated by ";" (for example fear;disgust).
# Blank rows are ignored. An EmoTag emotion is never overwritten.
# An emoji with several emotions gets top_emotion = "multiple" (like the words); best_emotion keeps the top EmoTag one.
manual_file <- file.path("data", "emotag", "manual_emoji_emotions.csv")
n_manual <- 0
if (file.exists(manual_file)) {
  man <- read.csv(manual_file, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  man$emoji <- stri_replace_all_fixed(man$emoji, "\uFE0F", "")
  sets <- lapply(man$emotion, function(x) {
    z <- trimws(unlist(strsplit(tolower(x), "[;,/|]")))
    unique(z[z != ""])
  })
  filled <- lengths(sets) > 0
  ok <- filled & sapply(sets, function(z) all(z %in% emotag_emotions))
  if (any(filled & !ok)) warning("Rows with an unknown emotion were ignored: ",
                                 paste(man$emoji[filled & !ok], collapse = " "))
  mm  <- match(emo$name, man$emoji)
  use <- is.na(emo$top_emotion) & !is.na(mm) & ok[mm]
  emo$emotions[use]    <- sapply(sets[mm[use]], paste, collapse = ";")
  emo$top_emotion[use] <- ifelse(lengths(sets[mm[use]]) > 1, "multiple", sapply(sets[mm[use]], `[`, 1))
  emo$source[use]      <- "manual"
  n_manual <- sum(use)
}
write.csv(emo, file.path(res_dir, "emoji_nodes_with_emotion.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# How much of our network does EmoTag cover?
covered <- !is.na(emo$top_emotion)          # has an emotion (EmoTag or manual)
lines <- c(
  paste("Emojis in EmoTag:", nrow(tag)),
  paste("Emoji nodes in our network:", nrow(emo)),
  paste("  with an emotion:", sum(covered), "(EmoTag:", sum(emo$source %in% "EmoTag" & covered),
        "| manual:", n_manual, ")"),
  paste("  without an emotion:", sum(!covered)),
  paste("Share of emoji links (strength) with an emotion:",
        round(sum(emo$strength[covered]) / sum(emo$strength), 3)),
  "",
  "Emoji nodes per emotion:",
  capture.output(print(table(emo$top_emotion, useNA = "ifany"))),
  "",
  "Most-linked emojis still without an emotion:",
  capture.output(print(head(emo[!covered, c("name", "degree", "strength")], 10), row.names = FALSE))
)
writeLines(lines, file.path(res_dir, "emoji_emotion_coverage.txt"), useBytes = TRUE)
cat(lines, sep = "\n")
