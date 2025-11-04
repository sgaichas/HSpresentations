# Claude Sonnet 4.5 generated initial code from prompt:
# write R code to generate a word cloud from a pdf, please use library::function syntax

# Required libraries (no need to load them with library())
# pdftools - For reading PDF files
# tm - For text mining and preprocessing
# wordcloud - For creating word clouds
# RColorBrewer - For color palettes

# I added lines to remove references from the pdf if they exist

# Function to generate word cloud from PDF
generate_wordcloud_from_pdf <- function(pdf_path, 
                                        max_words = 100,
                                        min_freq = 2,
                                        colors = brewer.pal(8, "Dark2"),
                                        background = "white") {
  
  # Read PDF file
  cat("Reading PDF file...\n")
  #text <- pdftools::pdf_text(pdf_path)
  
  # remove references
  # extract_pdf_text() <- function(path, numbering = TRUE, references = TRUE){
     text <- pdftools::pdf_text(pdf_path) |>
       paste0(collapse = " ") |>
       paste0(collapse = " ") |>
       stringr::str_squish()
  #   if(numbering) {
  #     target <- "\\[.*?\\]"
  #     text <- gsub(target, "", text)
  #   }
  #   if(references) {
       target <- "\\bReferences|references|REFERENCES\\b"
       text <- stringr::str_split(text, target) |>
         unlist() |>
         #head( -1) |>
         paste(collapse = " ")
  #   }
  #   return(text)
  # }
  
  # remove bullet characters, had to paste the large one in from an initial run
  text <- gsub("(\\*|\\-|●|•|–)", "", text)
  
  # remove proper names
  text <- gsub("Gaichas|Fay|salary", "", text)
  
  # Combine all pages into one text string
  text <- paste(text, collapse = " ")
  
  # Create a text corpus
  cat("Processing text...\n")
  corpus <- tm::Corpus(tm::VectorSource(text))
  
  # Text preprocessing
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))  # Convert to lowercase
  corpus <- tm::tm_map(corpus, tm::removePunctuation)              # Remove punctuation
  corpus <- tm::tm_map(corpus, tm::removeNumbers)                  # Remove numbers
  corpus <- tm::tm_map(corpus, tm::removeWords, tm::stopwords("english"))  # Remove common words
  corpus <- tm::tm_map(corpus, tm::stripWhitespace)                # Remove extra whitespace
  
  # Create term-document matrix
  tdm <- tm::TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  
  # Create data frame with words and frequencies
  df <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Generate word cloud
  cat("Generating word cloud...\n")
  wordcloud::wordcloud(words = df$word, 
                       freq = df$freq,
                       min.freq = min_freq,
                       max.words = max_words,
                       random.order = FALSE,
                       rot.per = 0.35,
                       colors = colors,
                       background.color = background)
  
  cat("Word cloud generated successfully!\n")
  
  # Return word frequency data frame
  return(df)
}

# Example usage:
# Replace with your PDF file path
#pdf_file <- "your_document.pdf"

pdf_file <- "~/Documents/Work/Proposals/Operationalizing  Ecosystem and Habitat Indicators-MAFMC May 2025.pdf"

# Generate word cloud (make sure the file exists first)
if (file.exists(pdf_file)) {

  png("docs/images/MAFMCwordcloud.png", width = 800, height = 600)
  
  word_data <- generate_wordcloud_from_pdf(
    pdf_path = pdf_file,
    max_words = 100,
    min_freq = 3,
    colors = RColorBrewer::brewer.pal(8, "Set2"),
    background = "white"
  )
  
  dev.off()
  
  # View top 20 most frequent words
  #head(word_data, 20)
  
  # # Optional: Save word cloud to file
  # 
  # wordcloud::wordcloud(words = word_data$word, 
  #                      freq = word_data$freq,
  #                      max.words = 100,
  #                      colors = RColorBrewer::brewer.pal(8, "Set2"))
  
  
} else {
  cat("Error: PDF file not found. Please update the pdf_file path.\n")
}

# Alternative: More customized word cloud
# wordcloud::wordcloud(words = word_data$word, 
#           freq = word_data$freq,
#           scale = c(4, 0.5),
#           min.freq = 2,
#           max.words = 200,
#           random.order = FALSE,
#           rot.per = 0.3,
#           colors = RColorBrewer::brewer.pal(9, "Blues")[4:9])