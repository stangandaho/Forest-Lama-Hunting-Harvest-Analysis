#
cond_proba <- function(data, by_row = T){
  
  if (!by_row) {
    data <- t(data)
  }
  
  row_name <- rownames(data)
  col_name <- colnames(data)
  prob_nam <- list()
  
  for (r in 1:length(row_name)) {
    for (c in 1:length(col_name)) {
      cell <- data[row_name[r], col_name[c]]/sum(data[row_name[r], ])
      prob_nam[[paste0("P(", col_name[c], "|", row_name[r], ")")]] <- cell
    }
  }
  
  prob_table <- as.data.frame(cbind(prob_nam)) %>% 
    tibble::rownames_to_column(var = "Probabilite") %>% 
    rename(Valeur = 2) %>% 
    mutate(Valeur = unlist(Valeur)) %>% 
    arrange(desc(Valeur)) %>% 
    as_tibble()
  
  return(prob_table)
}

bin_to_chr <- function(x){
  x <- if_else (x == 0, "Non", "Oui")
  
  return(x)
}
bin_to_chr(1)



# Define a function to calculate the angle for text rotation
calc_angle <- function(x) {
  angle <- 90 - 360 * (x - 0.5) / length(x)
  ifelse(angle < -90, angle + 180, angle)
}



