# Helper: read the NRC Emotion Lexicon and return one row per word with 0/1 columns
# for the 8 emotions we use (the same 8 as EmoTag). Positive/negative are ignored.
load_nrc <- function() {
  nrc_path <- list.files(nrc_dir, pattern = "Wordlevel", recursive = TRUE, full.names = TRUE)
  if (length(nrc_path) == 0) {
    stop("NRC lexicon not found. Download it from\n",
         "https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm\n",
         "unzip it and put the folder inside ", nrc_dir, "/")
  }
  nrc <- read.delim(nrc_path[1], header = FALSE, sep = "\t", quote = "", comment.char = "",
                    col.names = c("word", "emotion", "flag"), stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")
  nrc <- nrc[nrc$flag == 1 & nrc$emotion %in% emotag_emotions, ]
  nrc$word <- tolower(nrc$word)
  tab <- unclass(table(nrc$word, factor(nrc$emotion, levels = emotag_emotions)))
  wide <- data.frame(name = rownames(tab), as.data.frame.matrix(tab), stringsAsFactors = FALSE)
  rownames(wide) <- NULL
  wide$n_emotions <- rowSums(wide[, emotag_emotions])
  wide$emotions   <- apply(wide[, emotag_emotions], 1, function(r) paste(emotag_emotions[r > 0], collapse = ";"))
  # primary_emotion: the emotion if the word has exactly one, "multiple" if it has several
  wide$primary_emotion <- ifelse(wide$n_emotions == 1, wide$emotions,
                          ifelse(wide$n_emotions > 1, "multiple", NA))
  wide
}
