# define functions
myIBCF <- function(w, S30) {
  bool_known = !is.na(w)
  
  w_known = w[bool_known]
  S30_known = S30[,bool_known]
  
  rating = rep(NA, length(w))
  names(rating) = colnames(S30)
  # rating_unknown = rating[!bool_known]
  
  if (sum(bool_known)!=1) {
    # numerator = rowSums(S30_known[!bool_known, ] %*% diag(w_known), na.rm=TRUE) # wrong !!!
    numerator = rowSums(t(t(S30_known[!bool_known, ]) * w_known), na.rm=TRUE)
    denominator = rowSums(S30_known[!bool_known, ], na.rm=TRUE)
    denominator[denominator==0] = NA    
    rating[!bool_known] = numerator / denominator
  } else {
    rating[ !is.na(S30_known[!bool_known]) ] = w_known
  }
  ## set unpredictable ratings to be 0
  ## note: known ratings are set as NA
  rating[is.na(rating) & !bool_known] = 0  
  
  ## Sort rating with randomly breaking ties
  rand_numbers = runif(length(rating))
  indices = order(rating, rand_numbers, na.last=TRUE, decreasing=TRUE)
  sorted_rating = rating[indices]

  if ( sum(is.na(sorted_rating[1:10])) > 0 ) {
    cat("Sorry! No sufficient movies are available for recommendation. Please stay tuned for new movies released!\n")
  } 
  
  return(sorted_rating) 
}

search_movie_idx <- function(ID_top10, movies) {
  ID_top10 = as.integer(gsub("m", "", ID_top10))
  ind = which(movies$MovieID %in% ID_top10)  # without order
  movies_top10 = movies[ind,]
  ordered_ind = order(match(movies_top10$MovieID, ID_top10)) # based on the order of ID_top10
  return(ind[ordered_ind])  
}

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# read in data
# top10_genres <- fread('data/top10_genres.csv')
top10_genres = read.csv('data/top10_genres.csv', header = TRUE)
S30 <- read.csv('data/S30.csv', header = TRUE, row.names = 1)

myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


shinyServer(function(input, output, session) {
  
  #######################
  # By Genres 
  #######################
  
  # genreInput <- eventReactive(input$update, {
  #   top10_ID = top10_genres[,input$genre]
  #   movies_idx = search_movie_idx(top10_ID, movies)
  #   movies[movies_idx,]
  # }, 
  # ignoreNULL = FALSE)  
  
  # output$view <- renderTable({
  #   head(genreInput(), n = 10)
  # })
  
  # Calculate recommendations when the sbumbutton is clicked
  df1 <- eventReactive(input$btn1, {
    withBusyIndicatorServer("btn1", { # showing the busy indicator
      # # hide the rating container
      # useShinyjs()
      # jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      # runjs(jsCode)
      
      top10_ID = top10_genres[,input$genre]
      
      user_results = (10:1)/10 #(1:10)/10
      user_predicted_idx = search_movie_idx(top10_ID, movies) #1:10
      
      recom_results <- data.table(Rank = 1:10,
                                  MovieIdx = user_predicted_idx,
                                  Title = movies$Title[user_predicted_idx],
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results1 <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df1()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center",
                a(img(src = movies$image_url[recom_result$MovieIdx[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%",
                strong(movies$Title[recom_result$MovieIdx[(i - 1) * num_movies + j]])
            )
            
        )
      }))) # columns
    }) # rows
    
  }) # renderUI function  
  
  

  #######################
  # By Rating 
  #######################  

  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)
        user_ratings = as.matrix(user_ratings)
        
        w = rep(NA, ncol(S30))
        names(w) = colnames(S30)
        if (dim(user_ratings)[1] > 0) {
          w[paste0('m', user_ratings[,'MovieID'])] = user_ratings[,'Rating']
        }
        
        sorted_rating = myIBCF(w, S30)[1:10]
        # top10_ID = as.integer(gsub("m", "", names(sorted_rating)))
        top10_ID = names(sorted_rating)
        
        user_results = sorted_rating #(1:10)/10
        user_predicted_idx = search_movie_idx(top10_ID, movies) #1:10
        
        recom_results <- data.table(Rank = 1:10,
                                    MovieIdx = user_predicted_idx,
                                    Title = movies$Title[user_predicted_idx],
                                    Predicted_rating =  user_results)

    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

          div(style = "text-align:center",
              a(img(src = movies$image_url[recom_result$MovieIdx[(i - 1) * num_movies + j]], height = 150))
             ),
          div(style="text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieIdx[(i - 1) * num_movies + j]])
             )

        )
      }))) # columns
    }) # rows

  }) # renderUI function

}) # server function
