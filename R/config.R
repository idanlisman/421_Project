# Project settings. Change values here, not inside the other scripts.
# Run all scripts from the project root folder (the folder that contains R/ and data/).

# Which version of the analysis to run. run.R sets these for the main run and the robustness
# runs; when you run a single script on its own, the defaults ("main" run) are used.
run_name           <- Sys.getenv("RUN_NAME", "main")
only_emotion_words <- as.logical(Sys.getenv("ONLY_EMOTION_WORDS", "TRUE"))  # TRUE = only NRC words
min_emoji_posts    <- as.integer(Sys.getenv("MIN_EMOJI_POSTS", "3"))        # emoji must be in >= this many train posts

raw_dir  <- file.path("data", "posts")
proc_dir <- file.path("data", "processed", run_name)   # every run writes to its own folders
fig_dir  <- file.path("figures", run_name)
res_dir  <- file.path("results", run_name)

# The 11 emotion columns in the SemEval-2018 E-c files (1 = present, 0 = not present)
emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "love",
                  "optimism", "pessimism", "sadness", "surprise", "trust")

# Which words and emojis become nodes (counted on the TRAIN posts only)
min_word_posts  <- 5   # keep a word only if it appears in at least this many train posts
min_word_len    <- 3   # ignore words with fewer letters than this
# min_emoji_posts is set at the top of this file

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

# only_emotion_words (NRC words only, or all frequent words) is set at the top of this file
# Same idea for emojis, using EmoTag (FALSE = keep every frequent emoji)
only_emotion_emojis <- FALSE
source("R/nrc.R")
source("R/emoji.R")

# EmoTag: when does an emoji keep its SECOND emotion too?
# The second emotion is kept only if its score is at least emotag_second_min AND at least
# emotag_second_ratio times the top score (so the top emotion is not clearly higher).
# Otherwise the emoji keeps only its top emotion.
emotag_second_min   <- 0.2
emotag_second_ratio <- 0.8
