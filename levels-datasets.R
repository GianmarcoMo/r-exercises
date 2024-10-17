x <- c("a", "b", "a", "a", "c", "b")

y <- factor(x); y; nlevels(y)

# Ordinal factors
gradiments = c("not sufficient", "sufficient", "good", "very good")
level_gradiments <- factor(levels = gradiments, ordered = TRUE)
level_gradiments
min(level_gradiments)
