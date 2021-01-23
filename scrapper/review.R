# Package rvest mempunyai fungsi yang serupa dengan library beautiful soup pada Python, yaitu untuk web scraping.
library(rvest)

get_amusement_parks_reviews <- function(url, size = -1, incProgress = NULL) {
    reviews <- character()
    reviewers <- character()
  
    # Proses scraping kita mulai dengan membaca file HTML dari halaman website dengan menggunakan fungsi read_html.
    reviewPage <- read_html(url)
    review <- reviewPage %>%
      html_nodes('._3hDPbqWO > ._2uD5bLZZ .cPQsENeY') %>%
      html_text()
    reviewer <- reviewPage %>%
      html_nodes('._1r_My98y') %>%
      html_text()
    
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    if(!is.null(incProgress)) {
      incProgress(10/size) 
    }
    
    nextPage <- reviewPage %>%
      html_nodes('.next') %>%
      html_attr('href')
    
    if(is_empty(nextPage)) {
      nextPage = NA
    }
    
    while (!is.na(nextPage) & (length(reviews) < size | size == -1)) {
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(url, nextPage, sep = "")
      reviewPage <- read_html(reviewUrl)
      
      review <- reviewPage %>%
        html_nodes('._3hDPbqWO > ._2uD5bLZZ .cPQsENeY') %>%
        html_text()
      
      reviewer <- reviewPage %>%
        html_nodes('._1r_My98y') %>%
        html_text()

      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
    
      nextPage <- reviewPage %>%
        html_nodes('.next') %>%
        html_attr('href')
      
      if(is_empty(nextPage)) {
        nextPage = NA
      }
      
      # Fungsi incProgress menambah bilah status dengan jumlah tertentu, sedangkan fungsi setProgress menyetelnya ke nilai tertentu, dan juga dapat menyetel teks yang ditampilkan.
      if(!is.null(incProgress)) {
        incProgress(10/size) 
      }
    }
    
    totalReviews <- length(reviews)
    if(totalReviews < size || size == -1) {
      size = totalReviews
    }
    
    print(paste(length(reviews), "data", "collected"))
    
    return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE)[1 : size,])
}
