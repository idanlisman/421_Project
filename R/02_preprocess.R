# Step 2: clean the text, pull out words and emojis, and decide which ones become nodes.
# Words and emojis are kept only if they are frequent enough in the TRAIN posts.
source("R/config.R")
library(stringi)

posts <- readRDS(file.path(proc_dir, "posts_raw.rds"))
stop_words <- stopwords::stopwords("en")   # install.packages("stopwords") if missing

# Words: remove links and @mentions, lowercase, keep letters/apostrophes only
clean <- stri_replace_all_fixed(posts$Tweet, "\\n", " ")         # the files contain a literal "\n"
clean <- stri_replace_all_fixed(clean, c("&amp;", "&gt;", "&lt;"), c("&", ">", "<"),
                                vectorize_all = FALSE)                # HTML codes
clean <- stri_replace_all_fixed(clean, "\u2019", "'")                  # curly apostrophe -> '
clean <- stri_replace_all_regex(clean, "http\\S+|@\\w+", " ")
clean <- stri_trans_tolower(clean)
raw_words <- stri_extract_all_regex(clean, "[a-z']+")
posts$words <- lapply(raw_words, function(w) {
  w <- w[!is.na(w)]
  w <- w[nchar(w) >= min_word_len & !(w %in% stop_words)]
  unique(w)
})

# Keep only words that have at least one NRC emotion (switch in config.R)
if (only_emotion_words) {
  nrc_words <- load_nrc()$name
  posts$words <- lapply(posts$words, function(w) w[w %in% nrc_words])
}

# Emojis: whole emojis from the original text (see R/emoji.R)
posts$emojis <- extract_emojis(posts$Tweet)
# Optionally keep only emojis that EmoTag knows (switch in config.R)
if (only_emotion_emojis) {
  tag <- read.csv(emotag_file, encoding = "UTF-8", stringsAsFactors = FALSE)
  tag_emojis <- stri_replace_all_fixed(tag$emoji, "\uFE0F", "")
  posts$emojis <- lapply(posts$emojis, function(e) e[e %in% tag_emojis])
}

# Which words/emojis to keep, counted on train posts only
is_train <- posts$split == "train"
word_posts  <- table(unlist(posts$words[is_train]))
emoji_posts <- table(unlist(posts$emojis[is_train]))
keep_words  <- names(word_posts)[word_posts >= min_word_posts]
keep_emojis <- names(emoji_posts)[emoji_posts >= min_emoji_posts]

posts$words  <- lapply(posts$words,  function(w) w[w %in% keep_words])
posts$emojis <- lapply(posts$emojis, function(e) e[e %in% keep_emojis])

cat("Words kept:", length(keep_words), "| Emojis kept:", length(keep_emojis), "\n")
cat("Train posts with at least one kept emoji:",
    sum(sapply(posts$emojis[is_train], length) > 0), "of", sum(is_train), "\n")

write.csv(data.frame(word = names(word_posts), n_posts = as.integer(word_posts)),
          file.path(res_dir, "vocabulary_words.csv"), row.names = FALSE)
write.csv(data.frame(emoji = names(emoji_posts), n_posts = as.integer(emoji_posts)),
          file.path(res_dir, "vocabulary_emojis.csv"), row.names = FALSE, fileEncoding = "UTF-8")

saveRDS(posts, file.path(proc_dir, "posts_clean.rds"))
