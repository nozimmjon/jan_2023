

#list of positive and negative labels for each column
pos_labels_q1 <- c("Яхшиланади", "Ёмонлашади")
pos_labels_q2 <- c("Ошди", "Пасайди")
pos_labels_q3 <- c("Кўпаяди", "Қисқаради")
pos_labels_q4 <- c("Кўпайди", "Камайди")
pos_labels_q5 <- c("Кўпаяди", "Камаяди")
pos_labels_q6 <- c("Ҳа","Йўқ")

# Define a function to calculate balance score with custom labels
balance_score <- function(x, pos_labels) {
  # Get positive and negative responses
  positive <- sum(x[pos_labels[1]])
  negative <- sum(x[pos_labels[2]])
  
  # Calculate balance score
  bs_score <- (positive - negative) / sum(x) * 100 + 100
  
  return(bs_score)
}


