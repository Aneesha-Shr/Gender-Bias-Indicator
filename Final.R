#Final Project CS 210
  
  
#Load Gender Words
load_gender_words <- function(filepath) {
  library(jsonlite)
  
  tryCatch({
    gender_pairs <- fromJSON(filepath)
    
    if (is.matrix(gender_pairs)) {
      gender_pairs <- split(gender_pairs, row(gender_pairs))
    }
    
    masculine <- tolower(sapply(gender_pairs, function(pair) pair[[1]]))
    feminine  <- tolower(sapply(gender_pairs, function(pair) pair[[2]]))
    
    return(list(masculine = masculine, feminine = feminine))
  }, error = function(e) {
    stop("Failed to load gender word pairs: ", e$message)
  })
}


#Process Input Text
process_text <- function(file_path) {
  text <- tolower(readChar(file_path, file.info(file_path)$size))
  text <- gsub("[^a-z\\s]", " ", text)  # remove punctuation, numbers, symbols
  words <- unlist(strsplit(text, "\\s+"))  # split on whitespace
  words <- words[words != ""]  # remove empty strings
  return(words)
}


#Bias Analysis
analyze_bias <- function(words, gender_lists) {
  masculine_words <- gender_lists$masculine
  feminine_words  <- gender_lists$feminine
  
  masculine_count <- sum(words %in% masculine_words)
  feminine_count  <- sum(words %in% feminine_words)
  
  df <- data.frame(
    Gender = c("Masculine", "Feminine"),
    Frequency = c(masculine_count, feminine_count)
  )
  return(df)
}


#Plot Data - Bar Chart
plot_bar <- function(df) {
  library(ggplot2)
  ggplot(df, aes(x = Gender, y = Frequency, fill = Gender)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Gender Bias in Text",
         x = "Gender",
         y = "Frequency")
}


#Plot Data - Pie Chart
plot_pie <- function(df) {
  library(ggplot2)
  library(dplyr)
  
  # Calculate percentage and label position
  df <- df %>%
    mutate(
      Percent = Frequency / sum(Frequency) * 100,
      Label = paste0(round(Percent, 1), "%"),
      ypos = cumsum(Frequency) - 0.5 * Frequency
    )
  
  ggplot(df, aes(x = "", y = Frequency, fill = Gender)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    labs(title = "Gender Bias in Text") +
    scale_fill_manual(values = c("Masculine" = "#1f77b4", "Feminine" = "#ff69b4")) +
    geom_text(aes(y = ypos, label = Label), color = "white", size = 5)
}

#Web Scrape
library(rvest)

# Function to scrape text and save to a temporary file
scrape_text_to_file <- function(url) {
  # Read the HTML content from the website
  webpage <- read_html(url)
  
  # Extract the text from the webpage
  text <- webpage %>% html_nodes("p") %>% html_text()
  
  # Combine the text into a single string
  full_text <- paste(text, collapse = "\n")
  
  # Create a temporary file
  temp_file <- tempfile(fileext = ".txt")
  
  # Write the text to the temporary file
  writeLines(full_text, temp_file)
  
  # Return the path to the temporary file
  return(temp_file)
}



#Invalid URL Error Handling

is_valid_url <- function(url) {
  grepl("^https?://[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}", url)
}

#Handle File Naming
get_url_slug <- function(url) {
  slug <- gsub("^https?://", "", url)             # Remove http:// or https://
  slug <- gsub("[^a-zA-Z0-9]", "_", slug)          # Replace special characters with _
  slug <- substr(slug, 1, 30)                      # Limit slug length to keep filenames short
  return(slug)
}


#Run the dang thing


#Output Folder
output_dir <- file.path("~", "Desktop", "gender-charts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

print("Welcome to the Gender Bias Indicator! Use this tool to determine the frequency of gender biased words appearing in news articles. Enter the URL of any article to begin. Input 'escape' to end the session")
gender_words <- load_gender_words("/Users/aneeshashrestha/Documents/GitHub/Gender-Bias-Indicator/equalize_pairs.json")

url <- ""

while(url != "escape"){
  url <- readline(prompt = "Please enter URL of article: ")
  
  if(url == "escape"){
    break
  }
  
  if(!is_valid_url(url)) {
    cat("Invalid URL format. Please enter a valid URL (starting with http:// or https://) or type 'escape'.\n")
    next
  }
  
  temp_file_path <- tryCatch({
    scrape_text_to_file(url)
  }, error = function(e) {
    message("Error scraping the URL: ", e$message)
    return(NULL)
  })
  
  if(is.null(temp_file_path)) {
    next
  }
  
  word_list <- process_text(temp_file_path)
  data <- analyze_bias(word_list, gender_words)
  
  chart <- readline(prompt = "Would you like a pie or bar chart? Please enter 'pie' or 'bar': ")
  
  if(chart == "bar"){
    p <- plot_bar(data)
    try(print(p), silent = TRUE)
    slug <- get_url_slug(url)
    file_name <- paste0("bar_chart_", slug, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    file_path <- file.path(output_dir, file_name)
    ggsave(file_path, plot = p, width = 6, height = 4)
    message("Bar chart saved to: ", file_path)
    # rest of your saving code...
  } else if (chart == "pie"){
    p <- plot_pie(data)
    try(print(p), silent = TRUE)
    slug <- get_url_slug(url)
    file_name <- paste0("pie_chart_", slug, "_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    file_path <- file.path(output_dir, file_name)
    ggsave(file_path, plot = p, width = 6, height = 4)
    message("Pie chart saved to: ", file_path)
    # rest of your saving code...
  } else {
    cat("Invalid chart type.\n")
  }
}
