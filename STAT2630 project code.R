library(ggplot2)
library(dplyr)
library(plotly)
library(wordcloud2)
library(RColorBrewer)
library(scales) 
library(showtext)

showtext_auto()
data <- read.csv("douban_all_comments.csv", stringsAsFactors = FALSE)

data$rating <- as.numeric(data$rating)
data$like_count <- as.numeric(data$like_count)

# ====================== Rating Distribution ======================
ggplot(data, aes(x = factor(rating))) +
  geom_bar(fill = "#dfaad0", color = "white") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 5) +
  labs(title = "Rating Distribution",
       x = "Rating",
       y = "Number of Reviews") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.4, color = "#9B59B6", face = "bold"))

# ====================== Sentiment Distribution (Pie Chart) ======================
binary_data <- data %>%
  filter(sentiment_label %in% c("Positive", "Negative")) %>%
  count(sentiment_label) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2),
    slice_label = paste0(n, "\n(", percentage, "%)")   # <-- ONLY numbers + %
  )

# Plot
plot_ly(
  binary_data,
  values = ~n,
  type = 'pie',
  labels = ~sentiment_label,      # used ONLY for legend
  text = ~slice_label,            # shows ONLY numbers + % on slices
  textinfo = 'text',              # display custom label only
  hoverinfo = 'text',
  marker = list(colors = c("#ec8fca", "#a666b0"))   # Purple = Positive, Red = Negative
) %>%
  layout(
    title = list(
      text = "Sentiment Distribution",
      y = 0.92,           
      x = 0.5,
      xanchor = "center",
      yanchor = "top",
      font = list(
        size = 25,                
        family = "Arial Black",
        color = "#e0737b"
      )
    ),  
    showlegend = TRUE,                     # legend kept for clarity
    legend = list(orientation = "v", y = 0.8)
  )


# ====================== 5. Average Like Count by Sentiment ======================
avg_likes <- data %>%
  group_by(sentiment_label) %>%
  summarise(avg_like = mean(like_count, na.rm = TRUE))

ggplot(avg_likes, aes(x = sentiment_label, y = avg_like, fill = sentiment_label)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(avg_like, 0)), vjust = -0.5, size = 5) +
  labs(title = "Average Like Count by Sentiment",
       x = "", y = "Average Like Count") +
  scale_fill_manual(values = c("Positive" = "#539dad", 
                               "Neutral" = "#0a4780", 
                               "Negative" = "#d04c54")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 0.5, color = "#9e4461", size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.3, color = "#7fb2be", face = "bold"))

# ====================== 6. Movie Sentiment Comparison (Stacked Bar) ======================


# ====================== 2. Hard-coded Movie Names & Data ======================
movie_data <- data.frame(
  movie_name = c("哪吒之魔童降世", "流浪地球2", "满江红"),
  Positive   = c(80, 126, 87),
  Neutral    = c(50, 57, 56),
  Negative   = c(70, 17, 57)
)

# Reshape for ggplot
plot_data <- movie_data %>%
  tidyr::pivot_longer(cols = c(Positive, Neutral, Negative),
                      names_to = "sentiment_label",
                      values_to = "count")

# ====================== 3. Plot (with exact Chinese movie names) ======================
ggplot(plot_data, aes(x = movie_name, y = count, fill = sentiment_label)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5),
            size = 4.5, color = "black") +
  labs(title = "Movie Sentiment Comparison",
       x = "Movie",
       y = "Number of Reviews") +
  scale_fill_manual(values = c("Positive" = "#a2d1fb", 
                               "Neutral" = "#a0a7d6", 
                               "Negative" = "#f2b5dc")) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Microsoft YaHei"),   # Chinese font
    axis.text.x = element_text(hjust = 0.5, color = "#8f449a", size = 12),
    plot.title = element_text(hjust = 0.5, color = "#1e5082", face = "bold"),
    legend.title = element_blank()
  )
# ====================== 7. Word Cloud (Chinese) ======================
all_text <- paste(data$comment_text, collapse = " ")

# ====================== 4. Simple Text Cleaning (Base R) ======================
# Remove punctuation and split into words
words <- strsplit(all_text, "[[:punct:][:space:]]+")[[1]]

# Remove very short words and empty strings
words_clean <- words[nchar(words) > 1]

# ====================== 5. Get Top 70 Most Frequent Words ======================
word_freq <- table(words_clean)
word_freq_df <- as.data.frame(word_freq) %>%
  arrange(desc(Freq)) %>%
  head(70) %>%
  rename(word = words_clean, freq = Freq)

# Show the top 70 words (optional)
print("Top 70 words:")
print(word_freq_df)

# ====================== 6. Generate Word Cloud ======================
wordcloud2(word_freq_df,
           size = 0.9, 
           color = "random-dark",
           backgroundColor = "white",
           minRotation = -pi/6,
           maxRotation = pi/6,
           rotateRatio = 0.7,
           shape = "circle")