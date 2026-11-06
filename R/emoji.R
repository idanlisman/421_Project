# Helper: extract whole emojis from tweets.
# A tweet is split into "grapheme clusters" (what a reader sees as one character), so emojis built
# from several code points stay together: 🤦‍♀️, 👨‍👩‍👧, flags like 🇨🇦. A cluster counts as an emoji
# if it contains a Unicode Extended_Pictographic or Regional_Indicator character. Skin-tone
# modifiers and variation selectors are removed, so 👍🏽 and 👍 count as the same emoji.
# This replaces the old "\p{So}" rule, which also matched ©, ®, ™, °, box-drawing characters
# from text faces like (╯°□°)╯︵ ┻━┻, and split multi-part emojis into pieces.
library(stringi)

EMOJI_PATTERN <- "\\p{Extended_Pictographic}|\\p{Regional_Indicator}"
EMOJI_STRIP   <- "[\\x{1F3FB}-\\x{1F3FF}\\x{FE0F}\\x{FE0E}]"   # skin tones, variation selectors
# Pictographic in Unicode but not used as emojis: (c) (R) TM !! ?! arrows info M wavy-dash etc.
NOT_EMOJI <- c("©", "®", "™", "‼", "⁉", "➡",
               "⬅", "⬆", "⬇", "↔", "↕", "ℹ",
               "Ⓜ", "〰", "〽")

# Returns a list with one character vector per tweet: the distinct emojis in that tweet
extract_emojis <- function(txt) {
  clusters <- stri_split_boundaries(txt, type = "character")
  lapply(clusters, function(g) {
    e <- g[stri_detect_regex(g, EMOJI_PATTERN)]
    e <- stri_replace_all_regex(e, EMOJI_STRIP, "")
    unique(e[nchar(e) > 0 & !(e %in% NOT_EMOJI)])
  })
}
