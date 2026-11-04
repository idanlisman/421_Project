# Project settings. Change values here, not inside the other scripts.
# Run all scripts from the project root folder (the folder that contains R/ and data/).

raw_dir  <- file.path("data", "posts")
proc_dir <- file.path("data", "processed")
fig_dir  <- "figures"
res_dir  <- "results"

# The 11 emotion columns in the SemEval-2018 E-c files (1 = present, 0 = not present)
emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "love",
                  "optimism", "pessimism", "sadness", "surprise", "trust")

# Which words and emojis become nodes (counted on the TRAIN posts only)
min_word_posts  <- 5   # keep a word only if it appears in at least this many train posts
min_word_len    <- 3   # ignore words with fewer letters than this
min_emoji_posts <- 1   # keep an emoji only if it appears in at least this many train posts

for (d in c(proc_dir, fig_dir, res_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# EmoTag1200 emoji-emotion scores (https://github.com/abushoeb/EmoTag, MIT license)
emotag_file     <- file.path("data", "emotag", "EmoTag1200-scores.csv")
emotag_emotions <- c("anger", "anticipation", "disgust", "fear",
                     "joy", "sadness", "surprise", "trust")

# NRC Emotion Lexicon (Mohammad & Turney): put the downloaded folder inside data/nrc/
# The script looks for the file whose name contains "Wordlevel" (word <tab> emotion <tab> 0 or 1).
nrc_dir <- file.path("data", "nrc")

# One colour per emotion, used by the plots (same 8 emotions as EmoTag; grey = no emotion known)
emotion_colors <- c(anger = "#D7191C", anticipation = "#F4A300", disgust = "#7B5E2B",
                    fear = "#6A3D9A", joy = "#FFD92F", sadness = "#1F78B4",
                    surprise = "#33A02C", trust = "#17BECF")
no_emotion_color <- "grey75"
multi_emotion_color <- "grey45"

# Build the network only from words that have an NRC emotion (TRUE) or from all frequent words (FALSE)
only_emotion_words  <- TRUE
# Same idea for emojis, using EmoTag (FALSE = keep every frequent emoji)
only_emotion_emojis <- FALSE
source("R/nrc.R")

# EmoTag: when does an emoji keep its SECOND emotion too?
# The second emotion is kept only if its score is at least emotag_second_min AND at least
# emotag_second_ratio times the top score (so the top emotion is not clearly higher).
# Otherwise the emoji keeps only its top emotion.
emotag_second_min   <- 0.2
emotag_second_ratio <- 0.8
