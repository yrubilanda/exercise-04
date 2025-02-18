library(readr)

f1 <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/collins-scrabble-words-2019.txt"

f2 <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/google-10000-english-usa-no-swears.txt"

#this function takes a single argument "filename" that can be used to read either data files s or g.
load_dictionary <- function(file_name) { #takes one argument "file_name"
  read_csv(file_name, col_names = TRUE) #reads file
}

valid_list <- load_dictionary(f1) #variable valid_list assigned to function that reads in f1
solution_list <- load_dictionary(f2) #variable solution_list assigned to function that reads in f2

class(solution_list)
class(valid_list)

#converts columns from solution_list and valid_list into vector while extracting column name words
solution_list <- as.character(solution_list$words) 
valid_list <- as.character(valid_list$words) 

solution_list <- intersect(solution_list, valid_list) #finds common words between both lists and intersects them into new list as a vector
length(solution_list) #length of new list



#This function takes two arguments word_list: list of words, and word_length: the length of words we want
pick_solution <- function(word_list, word_length = 5) {
  
  #filter the word list to include only words of length of specified length, in this case 5
  filtered_words <- word_list[nchar(word_list) == word_length]
  
  #randomly select one word from list of filtered words of length 5
  chosen_word <- sample(filtered_words, 1) #the sample() function picks one random word
  
  #split the chosen words into individual letters
  split_word <- strsplit(chosen_word, "")[[1]] #strsplit() returns a list, so we extract the first element
  
  return(split_word)
}


solution_test <- pick_solution(solution_list)
print(solution_test)


#this function takes three arguments: 1) the solution, 2) valid guesses, and 3) number of guesses allowed
play_wordle <- function(solution, valid_list, num_guesses = 6){
  #explain rules of the game to the player
  cat("Welcome to Wordle\n")
  cat("You have", num_guesses, "chances to guess a word of length", nchar(solution), ".\n")
  
  #available letters
  letters_left <- letters
  
  #store guesses
  guess_history <- c() #an empty list to store past guesses
  
  #start the game loop
  for (i in 1:num_guesses) {
    cat("\nAttempt", i, "of", num_guesses, "\n")
    cat("Letters left:", paste(letters_left, collapse = " "), "\n") #convert to lowercase
    
    repeat { #keep asking until valid guess is given
      guess <- tolower(readline(prompt = "Enter your guess: ")) #convert to lowercase
      
      if (nchar(guess) != nchar(solution)) {
        cat("Invalid guess. Your word must be", nchar(solution), "letters long.\n")
      } else if (!(guess %in% valid_list)) {
        cat("Invalid guess. Word not in valid list.\n")
      } else {
        break
      }
    }
    
    #Store guess in history
    guess_history <- c(guess_history, guess)
    
    #Call evaluate_guess() (will define this function next)
    feedback <- evaluate_guess(guess, solution)
    
    #Show feedback
    cat("Feedback:", feedback, "\n")
    
    #Update available letters
    letters_left <- setdiff(letters_left, strsplit(guess, "")[[1]])
    
    #Check if the guess is correct
    if (guess == solution) {
      cat("\nCongratulations! You guessed the word!\n")
      cat("Your guesses:\n")
      print(guess_history)
      return(TRUE)  # Game over - player won
    }
  }
  
  #if the player runs out of guesses, show the correct word
  cat("\nGame over! The correct word was:", solution, "\n")
  cat("Your guesses:\n")
  print(guess_history)
  return(FALSE)  # Game over - player lost
  
}

evaluate_guess <- function(guess, solution) {
  #Create an empty feedback list with "-"
  feedback <- rep("-", nchar(solution))
  
  #Convert words into character vectors
  guess_letters <- strsplit(guess, "")[[1]]
  solution_letters <- strsplit(solution, "")[[1]]
  
  #Check for correct letters in correct positions
  for (i in seq_along(solution_letters)) {
    if (guess_letters[i] == solution_letters[i]) {
      feedback[i] <- "*"  # Correct letter, correct position
    } else if (guess_letters[i] %in% solution_letters) {
      feedback[i] <- "+"  # Correct letter, wrong position
    }
  }
  
  #Convert feedback list to a string
  return(paste(feedback, collapse = " "))
}

# Pick a random solution word from your solution_list
solution_word <- sample(solution_list, 1)

# Start the game
play_wordle(solution_word, valid_list)