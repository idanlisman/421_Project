#install.packages(c("stringi", "dplyr", "igraph", "stopwords", "ragg"))
# Runs the whole pipeline three times. Each run writes to its own folders:
#   results/<run>/, figures/<run>/, data/processed/<run>/
#   main         : NRC words only, emojis in >= 3 train posts   (the version the report uses)
#   all_words    : robustness - every frequent word, not only NRC words
#   emoji_min5   : robustness - emojis must be in >= 5 train posts

run_pipeline <- function(name, only_emotion_words, min_emoji_posts) {
  cat("\n================ RUN:", name, "================\n")
  Sys.setenv(RUN_NAME = name,
             ONLY_EMOTION_WORDS = only_emotion_words,
             MIN_EMOJI_POSTS = min_emoji_posts)
  on.exit(Sys.unsetenv(c("RUN_NAME", "ONLY_EMOTION_WORDS", "MIN_EMOJI_POSTS")))
  for (s in c("R/01_load_data.R", "R/02_preprocess.R", "R/03_build_network.R",
              "R/04_emoji_emotions.R", "R/05_word_emotions.R", "R/06_plots.R")) {
    source(s)
  }
}

run_pipeline("main",       only_emotion_words = TRUE,  min_emoji_posts = 3)
run_pipeline("all_words",  only_emotion_words = FALSE, min_emoji_posts = 3)
run_pipeline("emoji_min5", only_emotion_words = TRUE,  min_emoji_posts = 5)
