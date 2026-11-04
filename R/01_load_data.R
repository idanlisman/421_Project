# Step 1: read the three SemEval-2018 E-c files (train, dev, test-gold) into one table.
source("R/config.R")
library(stringi)

files <- list.files(raw_dir, pattern = "E-c-En.*\\.txt$", full.names = TRUE)
print(files)
stopifnot(length(files) == 3)

read_one <- function(f) {
  d <- read.delim(f, quote = "", comment.char = "",
                  fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  nm <- basename(f)
  d$split <- ifelse(grepl("train", nm), "train",
              ifelse(grepl("dev", nm), "dev", "test"))
  d
}
posts <- do.call(rbind, lapply(files, read_one))
stopifnot(all(c("ID", "Tweet", emotion_cols) %in% names(posts)))

# Quick look at the data
posts$has_emoji <- stri_detect_regex(posts$Tweet, "\\p{So}")

summary_lines <- c(
  "Posts per split:",
  capture.output(print(table(posts$split))),
  "",
  "Share of posts with at least one emoji, by split:",
  capture.output(print(round(tapply(posts$has_emoji, posts$split, mean), 3))),
  "",
  "Number of posts with each emotion label (all splits):",
  capture.output(print(colSums(posts[, emotion_cols])))
)
writeLines(summary_lines, file.path(res_dir, "data_summary.txt"))
cat(summary_lines, sep = "\n")

saveRDS(posts, file.path(proc_dir, "posts_raw.rds"))
